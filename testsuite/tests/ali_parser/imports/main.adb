--  with Test_Assert;

with GPR2.Containers;
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
      B_Imports  : GPR2.Containers.Name_Set;
      S_Imports  : GPR2.Containers.Name_Set;
      Needs_Body : Boolean;
      use Ada.Text_IO;
   begin

      Ada.Text_IO.Put_Line
        ("=== Parsing file " & String (ALI_File.Simple_Name));

      if not GPR2.Build.ALI_Parser.Imports
        (ALI_File, S_Imports, B_Imports, Needs_Body)
      then
         Ada.Text_IO.Put_Line ("Failed to import the ALI file");
      end if;

      Put_Line ("Needs body: " & Needs_Body'Image);

      Put_Line ("Withed by the spec:");
      for Imp of S_Imports loop
         Ada.Text_IO.Put_Line (" - " & String (Imp));
      end loop;

      Put_Line ("Withed by the body:");
      for Imp of B_Imports loop
         Ada.Text_IO.Put_Line (" - " & String (Imp));
      end loop;
   end Print_Imports;

   Tree : GPR2.Project.Tree.Object;
   Opts : GPR2.Options.Object;
begin
   if not Tree.Load (Opts, With_Runtime => False) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");

      return;
   end if;

   if not Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
   end if;

   Print_Imports
     (GPR2.Path_Name.Create_File ("./ali_files/gnatcoll-memory.ali"));
end Main;
