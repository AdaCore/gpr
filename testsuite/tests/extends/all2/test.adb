with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;

function Test return Integer is

   package TGPR renames Test_GPR;
   package A renames Test_Assert;

   Tree : GPR2.Project.Tree.Object;
begin
   TGPR.Load_With_No_Errors (Tree, "./data/main2.gpr");

   TGPR.Assert_Variable
      (Tree     => Tree,
       View     => "Main2",
       Variable => "Variable",
       Value    => "main2-shared2");

   return A.Report;
end Test;
