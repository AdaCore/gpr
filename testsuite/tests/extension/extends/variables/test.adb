with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;

function Test return Integer is

   package TGPR renames Test_GPR;
   package A renames Test_Assert;

   Tree : GPR2.Project.Tree.Object;
begin
   --  Ensure that a basic project with extends all is loaded correctly
   TGPR.Load_With_No_Errors (Tree, "./data/root.gpr");

   --  B_Variable1 is equal to B.Variable1 where B extends A and redefine
   --  value for A.Variable1
   TGPR.Assert_Variable
      (View     => Tree.Root_Project,
       Variable => "B_Variable1",
       Value    => "extended value1");

   --  B_Variable2 is equal to B.Variable2 where B extends A and Variable2 is
   --  only defined in A. We expected B.Variable2 to be equal to A.Variable2.
   TGPR.Assert_Variable
      (View     => Tree.Root_Project,
       Variable => "B_Variable2",
       Value    => "original value2");

   Tree.Unload;

   --  Now test with extension replacing project included by the extended
   TGPR.Load_With_No_Errors (Tree, "./data2/root.gpr");
   TGPR.Assert_Variable
      (View     => Tree.Root_Project,
       Variable => "Var2",
       Value    => "inc_ext");

   return A.Report;
end Test;
