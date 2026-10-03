with Test_GPR;
with Test_Assert;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.View_Ids;

function Test return Integer is
   package TGPR renames Test_GPR;
   package A renames Test_Assert;
   use type GPR2.Optional_Name_Type;
   use type GPR2.View_Ids.View_Id;
   use type GPR2.Project.View.Comparison_Result;
   use type GPR2.Project.View.Object;

   Tree : GPR2.Project.Tree.Object;
   Other_Tree : GPR2.Project.Tree.Object;
begin
   --  Ensure that nested aggregate projects are loaded correctly
   TGPR.Load_With_No_Errors (Tree, "./data/root_project.gpr");

   --  Ensure that the right context is used in the leaf project
   for V of Tree.Ordered_Views loop
      A.Assert (V.Id = GPR2.View_Ids.Import (GPR2.View_Ids.Image (V.Id)));
      A.Assert (GPR2.Project.View.Compare (V, V) = GPR2.Project.View.Equal);
      A.Assert (not (V < V));
   end loop;

   --  Equal IDs from independent trees must compare equal.
   TGPR.Load_With_No_Errors (Other_Tree, "./data/root_project.gpr");
   for Left of Tree.Ordered_Views loop
      for Right of Other_Tree.Ordered_Views loop
         declare
            Result : constant GPR2.Project.View.Comparison_Result :=
              GPR2.Project.View.Compare (Left, Right);
         begin
            A.Assert ((Result = GPR2.Project.View.Equal) =
                        (Left.Id = Right.Id));
            A.Assert ((Result = GPR2.Project.View.Less) =
                        (Left.Id < Right.Id));
            A.Assert ((Result = GPR2.Project.View.Greater) =
                        (Right.Id < Left.Id));
            A.Assert ((Left < Right) = (Left.Id < Right.Id));
         end;
      end loop;
   end loop;

   return A.Report;
end Test;
