with Ada.Text_IO;
with Ada.Strings.Fixed;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Build.Actions;

with Test_Actions;
with Test_Assert; use Test_Assert;

use GPR2;

function Test return Integer is
   package A renames Test_Assert;

   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Project : constant String := "tree/lib.gpr";

   Default_Action : Test_Actions.Default_Msg_Action;
   Custom_Action  : Test_Actions.Custom_Msg_Action;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => True) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
      return 1;
   end if;

   if not Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
      return 1;
   end if;

   --  Initialize both actions with the same view

   for Root of Tree.Namespace_Root_Projects loop
      Default_Action.Initialize (Root);
      A.Assert
        (Tree.Artifacts_Database.Add_Action (Default_Action),
         "Insert default action");

      Custom_Action.Initialize (Root);
      A.Assert
        (Tree.Artifacts_Database.Add_Action (Custom_Action),
         "Insert custom action");
   end loop;

   --  Test 1: Default Failure_Message should contain the UID image and
   --  end with " failed."

   declare
      Msg : constant String := Default_Action.Failure_Message;
   begin
      Ada.Text_IO.Put_Line ("Default message: " & Msg);
      A.Assert
        (Ada.Strings.Fixed.Index (Msg, "failed.") > 0,
         "Default Failure_Message should contain 'failed.'");
      A.Assert
        (Ada.Strings.Fixed.Index (Msg, "Default Action") > 0,
         "Default Failure_Message should contain the action class");
   end;

   --  Test 2: Overridden Failure_Message should return the custom message

   declare
      Msg : constant String := Custom_Action.Failure_Message;
   begin
      Ada.Text_IO.Put_Line ("Custom message: " & Msg);
      A.Assert
        (Msg = "custom failure: action did not succeed",
         "Custom Failure_Message should return the overridden message");
   end;

   --  Test 3: Verify the messages are different

   A.Assert
     (Default_Action.Failure_Message /= Custom_Action.Failure_Message,
      "Default and custom messages should differ");

   return A.Report;
end Test;
