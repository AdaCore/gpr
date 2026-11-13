--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with GPR2.Build.Actions_Scheduler;
with GPR2.Build.Jobserver;
with GPR2.Options;
with GPR2.Project.Tree;

with Failing_Action;

function Test return Integer is
   package Scheduler renames GPR2.Build.Actions_Scheduler;

   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Runner  : Scheduler.Object;
   Make_JS : GPR2.Build.Jobserver.Object;
   Action  : Failing_Action.Object;
   Options : Scheduler.Options;
   Status  : Scheduler.Execution_Status;

   use type Scheduler.Execution_Status;
begin
   Ada.Directories.Create_Path ("tree/obj");
   Opts.Add_Switch (GPR2.Options.P, "tree/main.gpr");

   if not Tree.Load (Opts, With_Runtime => True)
     or else not Tree.Update_Sources (GPR2.Sources_Units_Artifacts)
   then
      return 1;
   end if;

   Action.Initialize
     (Tree.Root_Project,
      Failing_Action.Failure_Kind'Value (Ada.Command_Line.Argument (1)));

   if not Tree.Artifacts_Database.Add_Action (Action) then
      return 1;
   end if;

   Options.Jobs := 1;
   Options.Force := True;
   Options.Stop_On_Fail := Boolean'Value (Ada.Command_Line.Argument (2));

   Status := Tree.Artifacts_Database.Execute (Runner, Options, Make_JS);
   Ada.Text_IO.Put_Line ("status: " & Status'Image);

   return (if Status = Scheduler.Failed then 0 else 1);
end Test;
