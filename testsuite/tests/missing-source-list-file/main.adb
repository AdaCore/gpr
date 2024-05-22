with GPR2.Log;
with GPR2.Options;
with GPR2.Project.Tree;

procedure Main is
   Tree : GPR2.Project.Tree.Object;
   Log   : GPR2.Log.Object;
   Opts : array (1..2) of GPR2.Options.Object;
   use GPR2;

begin
   Opts (1).Add_Switch (Options.P, "p.gpr");
   Opts (2).Add_Switch (Options.P, "p2.gpr");

   for Opt of Opts loop
      if Tree.Load (Opt, Absent_Dir_Error => No_Error) then
         Tree.Update_Sources (Messages => Log);
         Log.Output_Messages;
      end if;
   end loop;
end Main;
