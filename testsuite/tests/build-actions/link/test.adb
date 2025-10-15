with Ada.Directories;
with Ada.Containers;
with Ada.Environment_Variables;

with GPR2.Build.Actions.Link;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNAT.OS_Lib;
with GNATCOLL.OS.FSUtil;  use GNATCOLL.OS;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;         use Test_Assert;

with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

use GPR2, GPR2.Build;

function Test return Integer is
   use type GPR2.Language_Id;
   use type Ada.Containers.Count_Type;
   package GBA renames GPR2.Build.Actions;
   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Project     : constant String := "tree/main.gpr";


   Action : GBA.Link.Object := GBA.Link.Undefined;

   -----------------
   -- Init_Action --
   -----------------

   procedure Init_Action
   is
      Source : GPR2.Build.Source.Object;
   begin
      for Root of Tree.Namespace_Root_Projects loop
         for Main of Root.Mains loop
            Action.Initialize
              (Kind     => GBA.Link.Executable,
               Src      => Main,
               No_Rpath => False);
            Assert
               (not Tree.Artifacts_Database.Has_Action (Action.UID),
                "Check that action is not already in the Tree DB");
            Assert
              (Tree.Artifacts_Database.Add_Action (Action),
               "Insert action to the database");
         end loop;
      end loop;
   end Init_Action;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command (Cmd : Argument_List; Cwd : String := "") is
      Ret     : Integer;
      Process : Process_Handle;
      First   : Boolean := True;
   begin
      Process := Start (Args => Cmd, Cwd => Cwd, Stdout => FS.Standout, Stderr => FS.Standerr);
      Ret := Wait (Process);
      Assert (Ret = 0, "Check action return code");
   end Execute_Command;

   ---------------------------
   -- Update_Linker_Options --
   ---------------------------

   procedure Update_Linker_Options (GNATBind_Src : String) is
      use Ada.Text_IO;
      use Ada.Strings;
      use Ada.Strings.Fixed;


      procedure Process_Option_Or_Object_Line
        (Line : String);
      --  Pass options to the linker. Do not pass object file lines

      -----------------------------------
      -- Process_Option_Or_Object_Line --
      -----------------------------------

      procedure Process_Option_Or_Object_Line
         (Line : String)
      is
         Switch_Index : Natural := Index (Line, "--");
      begin
         if Switch_Index = 0 then
            raise Program_Error
              with "Failed parsing line " & Line & " from " & GNATBind_Src;
         end if;

         --  Skip the "--" comment prefix

         Switch_Index := Switch_Index + 2;

         declare
            Trimed_Line : constant String :=
              Trim (Line (Switch_Index .. Line'Last), Both);
         begin

            --  Pass only options to the link action

            if Trimed_Line (Trimed_Line'First) = '-' then
               declare
                  Idx  : constant PAI.Object :=
                           PAI.Create (Trimed_Line, Case_Sensitive => True);
                  Attr : constant GPR2.Project.Attribute.Object :=
                           Tree.Root_Project.Attribute
                             (PRA.Binder.Bindfile_Option_Substitution, Idx);
               begin
                  if Attr.Is_Defined then
                     for V of Attr.Values loop
                        Action.Add_Option_From_Binder (V.Text);
                     end loop;
                  else
                     Action.Add_Option_From_Binder (Trimed_Line);
                  end if;
               end;
            end if;

         end;
      end Process_Option_Or_Object_Line;

      Src_File     : File_Type;
      Reading      : Boolean         := False;
      Begin_Marker : constant String := "--  BEGIN Object file/option list";
      End_Marker   : constant String := "--  END Object file/option list";
   begin
      Open (File => Src_File,
            Mode => In_File,
            Name => GNATBind_Src);

      while not End_Of_File (Src_File) loop
         declare
            Line : constant String := Get_Line (Src_File);
         begin
            if Index (Line, Begin_Marker) = Line'First then
               Reading := True;
            elsif Index (Line, End_Marker) = Line'First then
               Reading := False;
            elsif Reading then
               Process_Option_Or_Object_Line (Line);
            end if;
         end;
      end loop;
   end Update_Linker_Options;

   Obj_Dir : Path_Name.Object;
   Count   : Natural;

begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   Assert
     (Tree.Load (Opts, With_Runtime => True), "Load the tree");

   Assert
     (Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts),
      "Update sources");

   Obj_Dir := Tree.Root_Project.Object_Directory;

   declare
      Args           : Argument_List;
   begin
      Args.Append ("gprbuild");
      Args.Append ("-c");
      Args.Append ("-b");
      Args.Append ("-p");
      Args.Append ("-q");
      Args.Append (String (Tree.Root_Project.Path_Name.Simple_Name));
      Execute_Command (Args, Tree.Root_Project.Dir_Name.String_Value);
   end;

   Init_Action;

   Tree.Artifacts_Database.Add_Input
     (Action.UID, Artifacts.Files.Create (Obj_Dir.Compose ("main.o")), False);
   Tree.Artifacts_Database.Add_Input
     (Action.UID, Artifacts.Files.Create (Obj_Dir.Compose ("pkg.o")), False);
   Tree.Artifacts_Database.Add_Input
     (Action.UID, Artifacts.Files.Create (Obj_Dir.Compose ("dep_two.o")), False);
   Tree.Artifacts_Database.Add_Input
     (Action.UID, Artifacts.Files.Create (Obj_Dir.Compose ("b__main.o")), False);

   Update_Linker_Options
     (Obj_Dir.Compose ("b__main.adb").String_Value);

   Count := 0;

   for Input of Tree.Artifacts_Database.Inputs (Action.UID) loop
      Count := Count + 1;
      Assert
        (Artifacts.Files.Object'Class (Input).Path.Exists,
         "Check that all input object " & Input.Serialize & " exists");
   end loop;

   Assert (Count = 4);


   declare
      Args : Argument_List;
      Env  : Environment_Dict;
   begin
      Action.Load_Signature;
      Action.Update_Command_Line (1);
      Execute_Command (Action.Command_Line.Argument_List,
                       Action.Working_Directory.String_Value);
   end;

   Assert (Action.Output.Path.Exists, "Check that the output executable exists");

   return Report;
end Test;
