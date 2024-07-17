--  with Test_Assert;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Build.ALI_Parser; use GPR2.Build.ALI_Parser;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO;
use GPR2;

procedure Main is
   procedure Print_Imports (ALI_File : GPR2.Path_Name.Object);

   procedure Print_Imports (ALI_File : GPR2.Path_Name.Object) is
      Imports  : Import_Info_Vectors.Vector :=
                   Import_Info_Vectors.Empty_Vector;
      Messages : GPR2.Log.Object;
   begin

      Ada.Text_IO.Put_Line
        ("=== Parsing file " & String (ALI_File.Simple_Name));
      GPR2.Build.ALI_Parser.Imports (ALI_File, Imports, Messages);

      if Messages.Has_Error then
         Messages.Output_Messages (Information => False, Warning => False);
      end if;

      for Imp of Imports loop
         Ada.Text_IO.Put_Line
           (To_String (Imp.Unit_Name) & ", " & To_String (Imp.Source) & ", " &
            To_String (Imp.ALI));
      end loop;
   end Print_Imports;

   Log  : GPR2.Log.Object;
   Tree : GPR2.Project.Tree.Object;
   Opts : GPR2.Options.Object;
begin
   if not Tree.Load (Opts, With_Runtime => False) then
      return;
   end if;

   Tree.Update_Sources
     (Option => GPR2.Sources_Units_Artifacts, Messages => Log);

   if Log.Has_Error then
      Log.Output_Messages;
      return;
   end if;

   Log.Clear;

   Print_Imports
     (GPR2.Path_Name.Create_File ("./ali_files/gnatcoll-memory.ali"));
end Main;