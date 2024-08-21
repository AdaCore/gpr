with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Project.Tree;
with GPR2.Options;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opts : GPR2.Options.Object;
begin
   Opts.Add_Switch (GPR2.Options.P, "prj2/prj2.gpr");

   if not Tree.Load (Opts, With_Runtime => True) then
      Put_Line ("could not load project");
      return;
   end if;
end Main;
