with Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;

   Prj : Project.Tree.Object;
   Opt : Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Text_IO.Put_Line ("cannot load project");
   end if;
end Main;
