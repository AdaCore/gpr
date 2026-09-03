--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.Build.Actions.Thread.Always_Execute;
with GPR2.Build.Actions_Scheduler;
with GPR2.Build.Jobserver;
with GPR2.Options;
with GPR2.Project.Tree;

use GPR2;

function Test return Integer is
   Opts      : GPR2.Options.Object;
   Exec_Opts : GPR2.Build.Actions_Scheduler.Options;
   Make_JS   : GPR2.Build.Jobserver.Object;
   --  Never connected: this test does not run under make

   use type GPR2.Build.Actions_Scheduler.Execution_Status;

   procedure Run_Once is
      Tree      : GPR2.Project.Tree.Object;
      Action    : GPR2.Build.Actions.Thread.Always_Execute.Object;
      Scheduler : GPR2.Build.Actions_Scheduler.Object;
      Status    : GPR2.Build.Actions_Scheduler.Execution_Status;
   begin
      if not Tree.Load (Opts, With_Runtime => False) then
         Ada.Text_IO.Put_Line ("Failed to load the tree");
         return;
      end if;

      if not Tree.Update_Sources (GPR2.Sources_Units_Artifacts) then
         Ada.Text_IO.Put_Line ("Failed to update sources");
         return;
      end if;

      Action.Initialize (Tree.Root_Project);

      if not Tree.Artifacts_Database.Add_Action (Action) then
         Ada.Text_IO.Put_Line ("Failed to add action");
         return;
      end if;

      Status :=
        Tree.Artifacts_Database.Execute (Scheduler, Exec_Opts, Make_JS);

      if Status /= GPR2.Build.Actions_Scheduler.Success then
         Ada.Text_IO.Put_Line ("Scheduler failed");
      end if;
   end Run_Once;

begin
   Opts.Add_Switch (GPR2.Options.P, "tree/main.gpr");
   Exec_Opts.Jobs := 1;

   --  First run: no saved signature, action runs unconditionally.
   Run_Once;

   --  Second run: signature is now valid (output file exists and matches),
   --  but Force_Execution = True ensures the action runs again.
   Run_Once;

   return 0;
end Test;
