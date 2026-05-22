with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with Test_Assert; use Test_Assert;


function Test return Integer is
   GPR     : constant String := "tree/abstract_multi_obj.gpr";
   Tree    : GPR2.Project.Tree.Object;
   Options : GPR2.Options.Object;
   View    : GPR2.Project.View.Object;
begin
   Options.Add_Switch (GPR2.Options.P, GPR);
   Assert (Tree.Load (Options));
   Assert (Tree.Update_Sources);

   return Report;
end Test;

