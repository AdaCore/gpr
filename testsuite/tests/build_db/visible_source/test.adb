with GPR2.Build.Source;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;
with Test_Assert; use Test_Assert;

function Test return Integer is
   use GPR2;
   use GPR2.Build;

   Tree : GPR2.Project.Tree.Object;
   Opt  : GPR2.Options.Object;

begin
   Opt.Add_Switch (Options.P, "trees/foo/app.gpr");

   Assert
     (Tree.Load (Opt, Absent_Dir_Error => No_Error),
      "failed to load trees/foo/app.gpr");

   Tree.Update_Sources;

   --  "foo.c" is owned by App itself, but Libfoo (withed by App) has a
   --  homonym: this must be reported as ambiguous.

   declare
      Ambiguous : Boolean;
      Src       : constant Build.Source.Object :=
                    Tree.Root_Project.Visible_Source ("foo.c", Ambiguous);
   begin
      Assert (Src.Is_Defined, """foo.c"" not found");
      Assert
        (String (Src.Owning_View.Name), "App",
         """foo.c"" found in the wrong view");
      Assert (Ambiguous, """foo.c"" should be reported ambiguous");
   end;

   --  "main.adb" is owned by App and has no homonym anywhere in the
   --  closure: this must not be reported as ambiguous.

   declare
      Ambiguous : Boolean;
      Src       : constant Build.Source.Object :=
                    Tree.Root_Project.Visible_Source ("main.adb", Ambiguous);
   begin
      Assert (Src.Is_Defined, """main.adb"" not found");
      Assert
        (String (Src.Owning_View.Name), "App",
         """main.adb"" found in the wrong view");
      Assert
        (not Ambiguous,
         """main.adb"" has no homonym, should not be ambiguous");
   end;

   Tree.Unload;

   return Report;
end Test;
