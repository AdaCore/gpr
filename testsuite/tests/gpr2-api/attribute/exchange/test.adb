with GNAT.OS_Lib;

with GNATCOLL.OS.Process;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Registry.Exchange;

package body Test is

   -------------------------------------
   -- Get_Exported_Registry_From_Tool --
   -------------------------------------

  function Get_Exported_Registry_From_Tool
     (Exec_Name : String;
      Default   : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Args_Vector : GNATCOLL.OS.Process.Argument_List;
      Output      : Ada.Strings.Unbounded.Unbounded_String;
      Dummy       : Integer;
   begin
      Args_Vector.Append (Locate_Exec_On_Path (Exec_Name));

         Args_Vector.Append ("--help");

         Output := GNATCOLL.OS.Process.Run
           (Args        => Args_Vector,
            Stdin       => GNATCOLL.OS.Process.FS.Null_FD,
            Stderr      => GNATCOLL.OS.Process.FS.To_Stdout,
            Status      => Dummy);

         if Dummy = 0 and then
           Ada.Strings.Unbounded.Count
             (Output, GPR2.Options.Print_GPR_Registry_Option) > 0
         then
            Args_Vector.Replace_Element
              (Index    => Natural'First + 1,
               New_Item => GPR2.Options.Print_GPR_Registry_Option);

            Output := GNATCOLL.OS.Process.Run
              (Args              => Args_Vector,
               Stdin             => GNATCOLL.OS.Process.FS.Null_FD,
               Stderr            => GNATCOLL.OS.Process.FS.To_Stdout,
               Universal_Newline => True,
               Status            => Dummy);

            if Dummy = 0 then
               return Output;
            end if;
         end if;
      return Default;
   end Get_Exported_Registry_From_Tool;

   -------------------------------
   -- Import_Registry_From_Tool --
   -------------------------------

   procedure Import_Registry_From_Tool
     (Exec_Name : String;
      Default   : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String)
   is
   begin
      GPR2.Project.Registry.Exchange.Import
        (Get_Exported_Registry_From_Tool (Exec_Name, Default));
   exception
      when others =>
         null;
   end Import_Registry_From_Tool;

   function Locate_Exec_On_Path (Exec_Name : String) return String is
      Path : GNAT.OS_Lib.String_Access
        := GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name);
      use type GNAT.OS_Lib.String_Access;
   begin
      if Path /= null then
         declare
            Result : constant String := Path.all;
         begin
            GNAT.OS_Lib.Free (Path);
            return Result;
         end;
      else
         return GPR2.Path_Name.Create_File
           (GPR2.Filename_Type (Exec_Name)).String_Value;
      end if;
   end Locate_Exec_On_Path;

end Test;
