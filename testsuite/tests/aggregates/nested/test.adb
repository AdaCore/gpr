with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;

function Test return Integer is
   package TGPR renames Test_GPR;
   package A renames Test_Assert;
   use type GPR2.Optional_Name_Type;

   Tree : GPR2.Project.Tree.Object;
begin
   --  Ensure that nested aggregate projects are loaded correctly
   TGPR.Load_With_No_Errors (Tree, "./data/root_project.gpr");

   --  Ensure that the right context is used in the leaf project
   for V of Tree.Ordered_Views loop
      if V.Name = "leaf" then
         TGPR.Assert_Variable
           (View     => V,
            Variable => "Leaf_Value",
            Value    => "value_from_root");
      end if;
   end loop;

   return A.Report;
end Test;
