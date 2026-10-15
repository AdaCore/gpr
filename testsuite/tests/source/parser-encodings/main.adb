with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;
with GPR2.Build.Source_Base.Ada_Parser;
with GPR2.File_Readers;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter.Console;
with GPR2.Path_Name;
with Test_Reader;

procedure Main is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use GPR2;

   function Read_Bytes (Filename : String) return String;

   procedure On_Error (Message : String);
   procedure On_Unit
     (Unit_Name : String; Separate_From : String; Generic_Unit : Boolean);
   procedure On_With (Unit_Name : String; Is_Limited : Boolean);

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Message : String) is
      pragma Unreferenced (Message);
   begin
      Put_Line ("ERROR");
   end On_Error;

   -------------
   -- On_Unit --
   -------------

   procedure On_Unit
     (Unit_Name : String; Separate_From : String; Generic_Unit : Boolean)
   is
      pragma Unreferenced (Generic_Unit);
   begin
      Put_Line ("unit=" & Unit_Name);
      if Separate_From /= "" then
         Put_Line ("parent=" & Separate_From);
      end if;
   end On_Unit;

   -------------
   -- On_With --
   -------------

   procedure On_With (Unit_Name : String; Is_Limited : Boolean) is
      pragma Unreferenced (Is_Limited);
   begin
      Put_Line ("with=" & Unit_Name);
   end On_With;

   ----------------
   -- Read_Bytes --
   ----------------

   function Read_Bytes (Filename : String) return String is
      use Ada.Streams, Ada.Streams.Stream_IO;
      Size   : constant Natural := Natural (Ada.Directories.Size (Filename));
      File   : Ada.Streams.Stream_IO.File_Type;
      Raw    : Stream_Element_Array (1 .. Stream_Element_Offset (Size));
      Last   : Stream_Element_Offset;
      Buffer : String (1 .. Size);
   begin
      Open (File, In_File, Filename);
      Read (File, Raw, Last);
      Close (File);
      for J in 1 .. Last loop
         Buffer (Natural (J)) := Character'Val (Raw (J));
      end loop;
      return Buffer (1 .. Natural (Last));
   end Read_Bytes;

begin
   if Argument (1) = "tree" then
      declare
         Tree : Project.Tree.Object;
         Opts : Options.Object;
      begin
         Opts.Add_Switch (Options.P, Argument (2));
         if not Tree.Load
           (Opts, True, Reporter.Console.Create (Reporter.Quiet))
         then
            raise Program_Error with "project load failed";
         end if;
         Tree.Update_Sources (Sources_Units);
         for Source of Tree.Root_Project.Sources loop
            declare
               Unit : constant Build.Compilation_Unit.Object :=
                 Tree.Root_Project.Own_Unit (Name_Type (Source.Unit.Name));
            begin
               if not Unit.Is_Defined then
                  raise Program_Error with "Unicode unit lookup failed";
               end if;
               Put_Line ("unit=" & String (Unit.Name));
            end;
         end loop;
      end;
   elsif Argument (1) = "callback" then
      declare
         Bytes : constant String := Read_Bytes (Argument (2));
         Slice : String (9 .. Bytes'Length + 8) := Bytes;
      begin
         Build.Source_Base.Ada_Parser.Parse_Context_Clauses
           (Slice, On_Error'Access, On_With'Access, On_Unit'Access);
      end;
   else
      declare
         Source : Build.Source_Base.Object'Class := Build.Source_Base.Create
           (Filename         => Path_Name.Create_File
                                  (Filename_Type (Argument (2))),
            Language         => Ada_Language,
            Kind             => S_Body,
            Timestamp        => Ada.Calendar.Clock,
            Tree_Db          => null,
            Naming_Exception => False,
            Source_Dir_Idx   => 1,
            From_Src_Subdirs => False);
         Ref     : File_Readers.File_Reader_Reference :=
                     File_Readers.No_File_Reader_Reference;
         Success : Boolean;
      begin
         if Argument (1) in "reader" | "reader-error" then
            Ref := Test_Reader.Create (Argument (1) = "reader-error");
         end if;
         Build.Source_Base.Ada_Parser.Compute (Ref, Source, True, Success);
         if Success then
            if Source.Unit.Separate_Name /= "" then
               Put_Line ("unit=" & String (Source.Unit.Separate_Name));
               Put_Line ("parent=" & String (Source.Unit.Name));
            else
               Put_Line ("unit=" & String (Source.Unit.Name));
            end if;
            for Name of Source.Unit.Dependencies loop
               Put_Line ("with=" & String (Name));
            end loop;
         else
            Put_Line ("ERROR");
         end if;
      end;
   end if;
end Main;
