with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Options : GPR2.Options.Object;
   Project_File : constant String := "tree/prj.gpr";
   Target : constant String := "not_a_target";
   Project : GPR2.Project.Tree.Object;
begin
   Options.Add_Switch (GPR2.Options.P, Project_File);
   Options.Add_Switch (GPR2.Options.Target, Target);

   if not Project.Load (Options, With_Runtime => True) then
      Put_Line ("Project loading failure!");
   end if;
   if not Project.Update_Sources then
      Put_Line ("Sources not updated");
   end if;
   Put_Line ("Project has rts: " & Project.Has_Runtime_Project'Image);
end Main;
