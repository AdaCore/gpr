with Test_Assert; use Test_Assert;

with GPR2; use GPR2;
with GPR2.Options;
with GPR2.Project;
with GPR2.Project.Tree;

function Test return Integer is
   Proj_Opt : Options.Object;
   Status : Boolean;
   Tree : Project.Tree.Object;
begin
   Proj_Opt.Add_Switch (Options.P, "./tree/main.gpr");
   Proj_Opt.Add_Switch (Options.Autoconf, "myconf.cgpr");

   --  Loading the tree in autoconf mode
   Status := Tree.Load (Proj_Opt, Absent_Dir_Error => GPR2.No_Error);
   Assert (Status, "Loading the tree");
   Assert (Tree.Get_KB.Is_Defined, "Get KB");

   Tree.Unload;

   --  Reloading the tree, this time with the configuration file present
   Status := Tree.Load (Proj_Opt, Absent_Dir_Error => GPR2.No_Error);
   Assert (Status, "Loading the tree");
   Assert (Tree.Get_KB.Is_Defined, "Get KB");

   return Report;

end Test;