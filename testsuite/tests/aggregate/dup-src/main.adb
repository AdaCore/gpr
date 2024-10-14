with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is

   use GPR2;

   Prj : Project.Tree.Object;
   Opt : GPR2.Options.Object;

begin
   Opt.Add_Switch (Options.P, "demo.gpr");

   if Prj.Load  (Opt, Absent_Dir_Error => No_Error) then
      Prj.Update_Sources;
   end if;
end Main;
