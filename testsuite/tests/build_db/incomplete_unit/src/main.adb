with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Build.Compilation_Unit;

procedure Main is
   Options      : GPR2.Options.Object;
   Project_File : constant String := "tree/prj.gpr";
   Project      : GPR2.Project.Tree.Object;
begin
   Options.Add_Switch (GPR2.Options.P, Project_File);
   if not Project.Load (Options, With_Runtime => True) then
      Put_Line ("Project loading failure!");
   end if;

   if not Project.Update_Sources then
      Put_Line ("Sources loading failure!");
   end if;

   for U of Project.Namespace_Root_Projects.First_Element.Units loop
      Put_Line (String (U.Name));
   end loop;
end Main;
