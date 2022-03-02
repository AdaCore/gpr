with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;
with GPR2.View_Ids;

function Test return Integer is
   package TGPR renames Test_GPR;
   package A renames Test_Assert;
   use type GPR2.Optional_Name_Type;
   use type GPR2.View_Ids.View_Id;

   Tree : GPR2.Project.Tree.Object;
begin
   --  Ensure that nested aggregate projects are loaded correctly
   TGPR.Load_With_No_Errors (Tree, "./data/root_project.gpr");

   --  Ensure that the right context is used in the leaf project
   for V of Tree.Ordered_Views loop
      A.Assert (V.Id = GPR2.View_Ids.Import (GPR2.View_Ids.Image (V.Id)));
   end loop;

   return A.Report;
end Test;
