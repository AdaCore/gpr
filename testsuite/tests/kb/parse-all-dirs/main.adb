with Ada.Strings.Fixed;
with Ada.Text_IO;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.OS.FS; use GNATCOLL.OS.FS;
with GNATCOLL.OS.FSUtil;
with GPR2; use GPR2;
with GNATCOLL.Traces;


procedure Main is

   package GPC renames GPR2.Project.Configuration;

   type Unbounded_String_Array is
     array (Positive range <>) of Unbounded_String;

   procedure Print_Logs (Logs : Log.Object; Warnings : Boolean);
   --  Print the errors logs, and warnings'one if specified.


   procedure Test
     (KB            : in out GPR2.KB.Object;
      Compiler_Name : String);
   --  Print the runtimes and version found for a given compiler name

   ----------------
   -- Print_Logs --
   ----------------

   procedure Print_Logs (Logs : Log.Object ; Warnings : Boolean) is
   begin
      for C in Logs.Iterate
         (False, Warnings, True, True, True)
      loop
         declare
            M : constant GPR2.Message.Object := GPR2.Log.Element (C);
         begin
            Ada.Text_IO.Put_Line (M.Format);
         end;
      end loop;
   end Print_Logs;

   ----------
   -- Test --
   ----------

   procedure Test
     (KB            : in out GPR2.KB.Object;
      Compiler_Name : String)
   is
      --  Result       : Boolean := True;
      Found        : Boolean := False;
      Config_Log   : Log.Object;
      Descriptions : GPC.Description_Set :=
        (Positive'First => GPC.Create
           (Language => +"Ada"));
      Compilers    : GPR2.KB.Compiler_Array := KB.All_Compilers
        (Settings => Descriptions,
         Target   => "fake-target",
         Messages => Config_Log);

   begin

      Ada.Text_IO.Put_Line ("=== " & Compiler_Name & " ===");
      Print_Logs (Config_Log, True);

      for Compiler of Compilers loop
         if String (GPR2.KB.Name (Compiler)) = Compiler_Name then
            Ada.Text_IO.Put_Line ("Runtime: " & String (GPR2.KB.Runtime(Compiler)));
            Ada.Text_IO.Put_Line ("Version: " & String (GPR2.KB.Version(Compiler)));
            Ada.Text_IO.Put_Line ("");
            Found := True;
         end if;
      end loop;

      if not Found then
         Ada.Text_IO.Put_Line ("No compiler found");
         Ada.Text_IO.Put_Line ("");
      end if;

   end Test;

   KB         : GPR2.KB.Object;
   Flags      : GPR2.KB.Parsing_Flags := (True, True, True);
   FD : File_Descriptor;

begin

   --  Create files and directories used by the test

   FD := Open ("./link-to-dir", Mode => Write_Mode);
   Write (FD, "dir_path");
   Close (FD);

   if not (GNATCOLL.OS.FSUtil.Create_Directory ("dir_path")) then
      Ada.Text_IO.Put_Line ("Failed to create directory 'dir_path'");
   end if;

   if not (GNATCOLL.OS.FSUtil.Create_Directory ("dir_path/subdir_path")) then
      Ada.Text_IO.Put_Line ("Failed to create directory 'dir_path/subdir_path'");
   end if;

   FD := Open ("dir_path/file", Mode => Write_Mode);
   Close (FD);

   KB := GPR2.KB.Create (GPR2.KB.Default_Flags);
   KB.Add (Flags, GPR2.Path_Name.Create_File
                    ("fake-compiler-description.xml"));

   --  We do not display warnings to avoid undefined environment variable
   --  used by the KB.

   Print_Logs (KB.Log_Messages, False);

   for I in 1..14 loop
      declare
         S : String := I'Image;
      begin
         Test (KB, "FAKE-ADA-COMPILER-" & S (S'First + 1 .. S'Last));
      end;
   end loop;

end Main;