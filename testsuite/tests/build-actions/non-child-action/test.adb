with Ada.Text_IO;
with GPR2.Options;
with GPR2.Project.Tree;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GPR2.Build.Actions;
with External_Action;
with Test_Assert; use Test_Assert;

use GPR2;

function Test return Integer is
   package A renames Test_Assert;

   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Project : constant String := "tree/agg.gpr";

   Lib1_Action : External_Action.Object;
   Lib2_Action : External_Action.Object;

   ------------------
   -- Init_Actions --
   ------------------

   procedure Init_Actions is
   begin
      for Root of Tree.Namespace_Root_Projects loop
         if Root.Name = "Lib1" then
            Lib1_Action.Initialize (Root);
            A.Assert (Tree.Artifacts_Database.Add_Action (Lib1_Action));

         elsif Root.Name = "Lib2" then
            Lib2_Action.Initialize (Root);
            A.Assert (Tree.Artifacts_Database.Add_Action (Lib2_Action));
         end if;
      end loop;

   end Init_Actions;

begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => True) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
   end if;

   if not Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
   end if;

   Init_Actions;

   --  Test view getter and setter

   A.Assert (Lib1_Action.View.Name = "Lib1");
   A.Assert (Lib2_Action.View.Name = "Lib2");
   Lib1_Action.Set_View (Lib2_Action.View);
   A.Assert (Lib1_Action.View.Name = "Lib2");

   return A.Report;
end Test;
