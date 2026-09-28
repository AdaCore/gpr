with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Build.ALI_Parser; use GPR2.Build.ALI_Parser;
with Ada.Text_IO;
use GPR2;

procedure Main is
   procedure Print_Imports (ALI_File : GPR2.Path_Name.Object);

   procedure Print_Imports (ALI_File : GPR2.Path_Name.Object) is
      ALI : GPR2.Build.ALI_Parser.Object;
      use Ada.Text_IO;
   begin
      Ada.Text_IO.Put_Line
        ("=== Parsing file " & String (ALI_File.Simple_Name));

      ALI := Create (ALI_File);

      if not ALI.Parse then
         Ada.Text_IO.Put_Line ("Failed to import the ALI file");
      end if;

      Put_Line ("Withed by the body:");
      for Imp of ALI.Withed_From_Body loop
         Ada.Text_IO.Put_Line (" - " & String (Imp));
      end loop;

      Put_Line ("Unresolved withs (no Sfile/Afile on the with-line):");
      for Imp of ALI.Unresolved_Withs loop
         Ada.Text_IO.Put_Line (" - " & String (Imp));
      end loop;
   end Print_Imports;

begin
   Print_Imports
     (GPR2.Path_Name.Create_File ("./ali_files/foo.ali"));
end Main;
