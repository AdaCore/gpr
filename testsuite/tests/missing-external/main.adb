with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use GPR2;

   Prj : Project.Tree.Object;
   Opt : Options.Object;
   Res : Boolean;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");
   Opt.Add_Switch (Options.X, "MyVAR2=whatever");
   Opt.Add_Switch (Options.X, "MyVAR3=");
   Res := Prj.Load (Opt, Absent_Dir_Error => No_Error);
end Main;
