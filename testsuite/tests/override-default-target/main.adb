with GPR2.KB;
with GPR2.Project.Tree;
with GPR2.Options;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;
   Res  : Boolean;
begin
   GPR2.KB.Set_Default_Target ("not-a-target");

   Opt.Add_Switch (GPR2.Options.P, "prj1.gpr");
   Res := Tree.Load (Opt, Absent_Dir_Error => GPR2.No_Error);
end Main;
