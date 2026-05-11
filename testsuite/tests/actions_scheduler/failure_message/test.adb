with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Build.Actions_Scheduler;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Console;

with Failing_Action;

use GPR2;
use GPR2.Reporter;

function Test return Integer is
   Tree      : GPR2.Project.Tree.Object;
   Opts      : GPR2.Options.Object;
   Project   : constant String := "tree/lib.gpr";
   Scheduler : GPR2.Build.Actions_Scheduler.Object;
   Exec_Opts : GPR2.Build.Actions_Scheduler.Options;
   Status    : GPR2.Build.Actions_Scheduler.Execution_Status;

   Scenario_Idx : Integer := Integer'Value (Ada.Command_Line.Argument (1));

   use type GPR2.Build.Actions_Scheduler.Execution_Status;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load
     (Opts, With_Runtime => True,  Reporter => Console.Create (No_Warnings))
   then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
      return 1;
   end if;

   if not Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
      return 1;
   end if;

   case Scenario_Idx is
      when 1 =>
         ----
         --  Scenario 1
         --
         --  A failing action with a custom Failure_Message.
         --  The message should be reported via the reporter.
         ----

         for Root of Tree.Namespace_Root_Projects loop
            declare
               Act : Failing_Action.Object;
            begin
               Act.Initialize (Root);
               if not Tree.Artifacts_Database.Add_Action (Act) then
                  Ada.Text_IO.Put_Line ("Failed to insert failing action");
                  return 1;
               end if;
            end;
         end loop;

      when 2 =>
         ----
         --  Scenario 2
         --
         --  A failing action that returns "" as Failure_Message.
         --  Nothing should be displayed on error.
         ----

         for Root of Tree.Namespace_Root_Projects loop
            declare
               Act : Failing_Action.Object;
            begin
               Act.Initialize (Root, Message => "");
               if not Tree.Artifacts_Database.Add_Action (Act) then
                  Ada.Text_IO.Put_Line ("Failed to insert failing action");
                  return 1;
               end if;
            end;
         end loop;

      when others =>
         null;
   end case;

   Exec_Opts.Jobs := 1;
   Exec_Opts.Force := True;

   Status := Tree.Artifacts_Database.Execute (Scheduler, Exec_Opts);

   return 0;
end Test;
