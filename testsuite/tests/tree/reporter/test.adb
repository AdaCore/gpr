with GPR2.Project.Tree;
with Test_Assert; use Test_Assert;
with GPR2.Reporter; use GPR2.Reporter;
with GPR2.Options;
with GPR2.Reporter.Console;

function Test return Integer is
   Tree     : GPR2.Project.Tree.Object;
   Opt      : GPR2.Options.Object;
   Reporter : GPR2.Reporter.Console.Object :=
     GPR2.Reporter.Console.Create (Quiet);
begin
   Opt.Add_Switch (GPR2.Options.P, "./tree/main.gpr");

   if not Tree.Load
     (Opt, Absent_Dir_Error => GPR2.Error, Reporter => Reporter)
   then
      Assert (False,"Load the tree");
   end if;

   Assert (Reporter.Verbosity = Quiet);
   Assert (Tree.Reporter.Verbosity = Quiet);
   GPR2.Reporter.Console.Object
     (Tree.Reporter.Element.all).Set_Verbosity (Verbose);
   Assert (Tree.Reporter.Verbosity = Verbose);

   return Report;
end Test;

