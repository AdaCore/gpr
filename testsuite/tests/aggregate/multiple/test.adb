with GPR2.Project.Tree;
with GPR2.Project.View;

with Test_GPR;
with Test_Assert;

function Test return Integer is
   Tree : GPR2.Project.Tree.Object;

begin
   --  Ensure that aggregate projects are loaded correctly
   Test_GPR.Load_With_No_Errors (Tree, "./data/everything.gpr");

   Test_Assert.Assert
     (Natural (Tree.Root_Project.Aggregated.Length),
      1,
      "total amount of aggregated projects");

   Test_Assert.Assert
     (String (Tree.Root_Project.Aggregated.First_Element.Name),
      "General_Spark",
      "name of the aggregated project");

   return Test_Assert.Report;
end Test;
