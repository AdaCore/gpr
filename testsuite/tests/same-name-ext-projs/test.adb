with GPR2.Options;
with GPR2.Project.Tree;

procedure Test is
   use GPR2;

   Tree : Project.Tree.Object;
   Opt  : Options.Object;
   Res  : Boolean;
begin
   Opt.Add_Switch (Options.P, "main/main_master.gpr");
   Res := Tree.Load (Opt, Absent_Dir_Error => No_Error);
end Test;
