with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;

function Test return Integer is

   package TGPR renames Test_GPR;
   package A renames Test_Assert;

   Tree : GPR2.Project.Tree.Object;
begin
   --  Ensure that a basic project with extends all is loaded correctly
   TGPR.Load_With_No_Errors (Tree, "./basic/root.gpr");

   --  Variable1 definition starts in B, then amended by C, then A
   TGPR.Assert_Variable
      (View     => Tree.Root_Project,
       Variable => "A_Variable1",
       Value    => "A extall C with D with ExtB.Var2");

   --  Variant of previous test in which C is referenced instead of A.
   TGPR.Assert_Variable
      (View     => Tree.Root_Project,
       Variable => "A_Variable2",
       Value    => "A with ExtB.Var2");

   --  Variant of previous test in which C is referenced instead of A, but
   --  variable value is not modified in the extension
   TGPR.Assert_Variable
      (View     => Tree.Root_Project,
       Variable => "B_Variable2",
       Value    => "B.Var2");

   return A.Report;
end Test;
