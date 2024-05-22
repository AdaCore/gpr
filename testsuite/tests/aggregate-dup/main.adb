with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Options;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Tree;

procedure Main is

   use Ada;
   use GPR2;
   use GPR2.Project;

   Prj : Project.Tree.Object;
   Opt : GPR2.Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Finalize;
   if not Prj.Load (Opt, Absent_Dir_Error => No_Error) then
      Text_IO.Put_Line ("cannot load project");
   end if;
end Main;
