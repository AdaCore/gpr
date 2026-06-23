--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.Build.Actions.Thread.Trigger;
with GPR2.Build.Actions_Scheduler;
with GPR2.Options;
with GPR2.Project.Tree;

use GPR2;
use GPR2.Project.Tree;

function Test return Integer is
   Opts      : GPR2.Options.Object;
   Exec_Opts : GPR2.Build.Actions_Scheduler.Options;

   use type GPR2.Build.Actions_Scheduler.Execution_Status;

   Tree      : GPR2.Project.Tree.Object;
   Action    : GPR2.Build.Actions.Thread.Trigger.Object;
   Scheduler : GPR2.Build.Actions_Scheduler.Object;
   Status    : GPR2.Build.Actions_Scheduler.Execution_Status;
begin
   Opts.Add_Switch (GPR2.Options.P, "tree/main.gpr");
   Exec_Opts.Jobs := 1;

   if not Tree.Load
     (Opts, With_Runtime => False, Create_Missing_Dirs => Create_Always)
   then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
      return 1;
   end if;

   if not Tree.Update_Sources (GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
      return 1;
   end if;

   Action.Initialize (Tree.Root_Project);

   if not Tree.Artifacts_Database.Add_Action (Action) then
      Ada.Text_IO.Put_Line ("Failed to add action");
      return 1;
   end if;

   Status := Tree.Artifacts_Database.Execute (Scheduler, Exec_Opts);

   if Status /= GPR2.Build.Actions_Scheduler.Success then
      Ada.Text_IO.Put_Line ("Scheduler failed");
      return 1;
   end if;

   return 0;
end Test;
