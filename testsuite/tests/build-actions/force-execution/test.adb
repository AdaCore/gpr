with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Build.Actions;
with Simple_Action;
with Test_Assert;

use GPR2;

function Test return Integer is
   package A renames Test_Assert;

   Tree   : GPR2.Project.Tree.Object;
   Opts   : GPR2.Options.Object;
   Action : Simple_Action.Object;

begin
   Opts.Add_Switch (GPR2.Options.P, "tree/prj.gpr");

   if not Tree.Load (Opts, With_Runtime => False) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
      return A.Report;
   end if;

   Action.Initialize (Tree.Root_Project);

   A.Assert
     (Action.Force_Execution,
      "Simple_Action.Force_Execution overrides the default to True");

   return A.Report;
end Test;
