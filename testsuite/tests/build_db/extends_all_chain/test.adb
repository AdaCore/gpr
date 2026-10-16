with GPR2.Build.View_Db;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Set;

with Test_Assert;

function Test return Integer is
   use GPR2;
   use type Path_Name.Object;
   use type Project.View.Object;

   Tree     : Project.Tree.Object;
   Opt      : Options.Object;
   Leaf     : Project.View.Object;
   Middle   : Project.View.Object;
   External : Project.View.Object;

begin
   Opt.Add_Switch (Options.P, "tree/root.gpr");
   Test_Assert.Assert (Tree.Load (Opt, Absent_Dir_Error => No_Error));

   for View of Tree.Ordered_Views loop
      if View.Name = "Leaf" then
         Leaf := View;
      elsif View.Name = "Middle" then
         Middle := View;
      elsif View.Name = "External" then
         External := View;
      end if;
   end loop;

   Test_Assert.Assert (Leaf.Is_Defined and Middle.Is_Defined);
   Test_Assert.Assert (External.Is_Defined);
   Test_Assert.Assert (not Tree.Root_Project.Extended.Contains (Leaf));
   Test_Assert.Assert (Tree.Root_Project.Extended.Contains (Middle));
   Test_Assert.Assert (not Tree.Root_Project.Extended.Contains (External));
   Test_Assert.Assert (Middle.Extended.Contains (Leaf));
   Test_Assert.Assert (Tree.Root_Project.Is_Extending (Leaf));
   Test_Assert.Assert (not Tree.Root_Project.Is_Extending (External));
   Test_Assert.Assert (Leaf.Extending = Middle);
   Test_Assert.Assert (Middle.Extending = Tree.Root_Project);
   Test_Assert.Assert (not External.Is_Extended);

   for Attempt in 1 .. 2 loop
      Test_Assert.Assert (Tree.Update_Sources);
      Test_Assert.Assert
        (Tree.Root_Project.View_Db.Source ("pkg.ads").Path_Name =
           Path_Name.Create_File ("tree/middle/pkg.ads"));
      Test_Assert.Assert
        (Tree.Root_Project.View_Db.Has_Source ("base_unit.ads"));
      Test_Assert.Assert
        (Tree.Root_Project.View_Db.Has_Source ("root_unit.ads"));
      Test_Assert.Assert
        (not Tree.Root_Project.View_Db.Has_Source ("external_unit.ads"));
      Test_Assert.Assert (External.View_Db.Has_Source ("external_unit.ads"));
   end loop;

   declare
      Explicit_Tree : Project.Tree.Object;
      Explicit_Opt  : Options.Object;
   begin
      Explicit_Opt.Add_Switch (Options.P, "tree/explicit.gpr");
      Test_Assert.Assert
        (Explicit_Tree.Load (Explicit_Opt, Absent_Dir_Error => No_Error));
      declare
         Root   : constant Project.View.Object := Explicit_Tree.Root_Project;
         Parent : constant Project.View.Object := Root.Extended_Root;
      begin
         Test_Assert.Assert (Parent.Is_Externally_Built);
         Test_Assert.Assert (Parent.Is_Extended);
         Test_Assert.Assert (Parent.Extending = Root);
         Test_Assert.Assert (Root.Extended.Contains (Parent));
         Test_Assert.Assert (Explicit_Tree.Update_Sources);
         Test_Assert.Assert (Root.View_Db.Has_Source ("external_unit.ads"));
      end;
   end;

   return Test_Assert.Report;
end Test;
