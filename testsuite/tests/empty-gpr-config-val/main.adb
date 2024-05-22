with Ada.Environment_Variables;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use GPR2;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Res : Boolean;

begin
   Ada.Environment_Variables.Set ("GPR_CONFIG", "");
   Opt.Add_Switch (Options.P, "demo.gpr");
   Res := Prj.Load (Opt, Absent_Dir_Error => No_Error);
end Main;
