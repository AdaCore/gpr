with GPR2;
with GPR2.Project.Tree;
with GPR2.Project.View;
with Test_Assert;
with Test_GPR;

function Test return Integer is
   use type GPR2.Project_Kind;

   Tree  : GPR2.Project.Tree.Object;
   Root  : GPR2.Project.View.Object;
   Count : Natural := 0;
begin
   Test_GPR.Load_With_No_Errors (Tree, "data/combined.gpr");
   Root := Tree.Root_Project;

   Test_Assert.Assert (Root.Kind = GPR2.K_Aggregate_Library);
   Test_Assert.Assert (Root.Is_Library);
   Test_Assert.Assert (not Root.Is_Abstract);
   Test_Assert.Assert (Root.Extended_Root.Is_Abstract);
   Test_Assert.Assert (Root.Language_Ids.Contains (GPR2.Ada_Language));

   for View of Root.Aggregated loop
      Count := Count + 1;
      Test_Assert.Assert (not View.Is_Abstract);
      Test_Assert.Assert (View.Language_Ids.Contains (GPR2.Ada_Language));
   end loop;

   Test_Assert.Assert (Count = 1);
   return Test_Assert.Report;
end Test;
