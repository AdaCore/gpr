with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Build.ALI_Parser;
with Ada.Text_IO;

procedure Main is
   use GPR2;

   procedure Print_Switches (ALI_File : GPR2.Path_Name.Object);

   procedure Print_Switches (ALI_File : GPR2.Path_Name.Object) is
   begin
      Ada.Text_IO.Put_Line
        ("=== Parsing switches from " & String (ALI_File.Simple_Name));

      for Sw of GPR2.Build.ALI_Parser.Switches (ALI_File) loop
         Ada.Text_IO.Put_Line ("- " & Sw);
      end loop;
   end Print_Switches;

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

   Print_Switches
     (GPR2.Path_Name.Create_File ("./ali_files/dummy.ali"));
end Main;
