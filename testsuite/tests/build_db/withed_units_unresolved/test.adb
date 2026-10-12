with GPR2.Build.ALI_Parser;
with GPR2.Build.Actions.Process.Compile.Ada;
with GPR2.Build.Compilation_Unit;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter.Console;

with Test_Assert; use Test_Assert;

use GPR2, GPR2.Reporter;

function Test return Integer is
   Tree      : GPR2.Project.Tree.Object;
   Opts      : GPR2.Options.Object;
   Project   : constant String := "tree/prj.gpr";
   Root_View : GPR2.Project.View.Object;
   CU        : GPR2.Build.Compilation_Unit.Object;
   Comp      : GPR2.Build.Actions.Process.Compile.Ada.Object;

   function Has (Set : GPR2.Containers.Name_Set; Name : String)
                 return Boolean
   is (Set.Contains (GPR2.Name_Type (Name)));

begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   Assert
     (Tree.Load (Opts, True, Console.Create (Quiet)), "Loading the tree");

   Tree.Update_Sources (Sources_Units);

   Root_View := Tree.Namespace_Root_Projects.First_Element;

   CU := Root_View.Own_Unit ("Foo");
   Assert (CU.Is_Defined, "Compilation unit Foo found");

   --  Foo's own .ali (tree/obj/foo.ali) is a hand-placed fixture, not the
   --  product of a real compile: it carries all four with-line shapes
   --  (W/Z, each resolved or bare) so that Withed_Units/_From_Spec/
   --  _From_Body can be checked directly against it, without depending on
   --  the compiler ever actually producing a bare with-line on demand.

   Comp.Initialize (Src => CU);
   Assert
     (Tree.Artifacts_Database.Add_Action (Comp), "Add compile action");

   declare
      All_Withs  : constant GPR2.Containers.Name_Set := Comp.Withed_Units;
      Spec_Withs : constant GPR2.Containers.Name_Set :=
        Comp.Withed_Units_From_Spec;
      Body_Withs : constant GPR2.Containers.Name_Set :=
        Comp.Withed_Units_From_Body;
   begin
      --  All-units path (Withed_Units, All_Units => True, the default)

      Assert (Has (All_Withs, "bar"), "all-units: bar (W, resolved) kept");
      Assert (Has (All_Withs, "qux"), "all-units: qux (Z, resolved) kept");
      Assert
        (not Has (All_Withs, "baz"),
         "all-units: baz (W, unresolved) filtered out");
      Assert
        (not Has (All_Withs, "quux"),
         "all-units: quux (Z, unresolved) filtered out");

      --  Spec path: the fixture's only U line is "foo%b" (a body-only
      --  unit, like a Main procedure), so nothing is attributed to the
      --  spec at all.

      Assert (Spec_Withs.Is_Empty, "spec path: empty, as expected");

      --  Body path: same filtering as the all-units path should apply,
      --  since every with-line in this fixture is attributed to the body.

      Assert (Has (Body_Withs, "bar"), "body: bar (W, resolved) kept");
      Assert (Has (Body_Withs, "qux"), "body: qux (Z, resolved) kept");
      Assert
        (not Has (Body_Withs, "baz"),
         "body: baz (W, unresolved) filtered out");
      Assert
        (not Has (Body_Withs, "quux"),
         "body: quux (Z, unresolved) filtered out");
   end;

   return Report;
end Test;
