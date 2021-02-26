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

   --  A_Variable1 value is A.Variable1 with A been "all extended" by C.
   --  in C A.Variable1 is redefined from "original value" to "extended value".
   --  We expect A.Variable1 to be equal in that case to C.Variable1.
   TGPR.Assert_Variable
      (Tree     => Tree,
       View     => "Root",
       Variable => "A_Variable1",
       Value    => "extended value");

   --  Variant of previous test in which C is referenced instead of A.
   TGPR.Assert_Variable
      (Tree     => Tree,
       View     => "Root",
       Variable => "B_Variable1",
       Value    => "extended value");

   --  Variant of previous test in which C is referenced instead of A, but
   --  variable value is not modified in the extension
   TGPR.Assert_Variable
      (Tree     => Tree,
       View     => "Root",
       Variable => "B_Variable2",
       Value    => "original value2");

   return A.Report;
end Test;
