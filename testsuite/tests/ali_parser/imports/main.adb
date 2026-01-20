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
      ALI        : GPR2.Build.ALI_Parser.Object;
      use Ada.Text_IO;
   begin
      Ada.Text_IO.Put_Line
        ("=== Parsing file " & String (ALI_File.Simple_Name));

      ALI := Create (ALI_File);
      ALI.Parse;

      if not ALI.Is_Parsed then
         Ada.Text_IO.Put_Line ("Failed to import the ALI file");
      end if;

      Put_Line ("Needs body: " & ALI.Spec_Needs_Body'Image);

      Put_Line ("Withed by the spec:");
      for Imp of ALI.Withed_From_Spec loop
         Ada.Text_IO.Put_Line (" - " & String (Imp));
      end loop;

      Put_Line ("Withed by the body:");
      for Imp of ALI.Withed_From_Body loop
         Ada.Text_IO.Put_Line (" - " & String (Imp));
      end loop;
   end Print_Imports;

begin
   Print_Imports
     (GPR2.Path_Name.Create_File ("./ali_files/gnatcoll-memory.ali"));
end Main;
