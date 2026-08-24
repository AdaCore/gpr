--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Command_Line;
with Ada.Text_IO;

with GPR2.Build.Actions.Thread.Report_Messages;
with GPR2.Build.Actions_Scheduler;
with GPR2.Build.Jobserver;

with GPR2.Options;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNATCOLL.Traces;

use GPR2;

function Test return Integer is
   Tree      : GPR2.Project.Tree.Object;
   Opts      : GPR2.Options.Object;
   Project   : constant String := "tree/main.gpr";
   Scheduler : GPR2.Build.Actions_Scheduler.Object;
   Make_JS   : GPR2.Build.Jobserver.Object;
   --  Never connected: this test does not run under make
   Root_View : GPR2.Project.View.Object;
   Exec_Opts : GPR2.Build.Actions_Scheduler.Options;
   Ret_Code  : Integer := 0;

   package GBA renames GPR2.Build.Actions;

   use type GPR2.Build.Actions_Scheduler.Execution_Status;
begin

   GNATCOLL.Traces.Parse_Config_File;

   if Ada.Command_Line.Argument_Count > 0 then
      Ret_Code := Integer'Value (Ada.Command_Line.Argument (1));
   end if;

   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, True)
     or else not Tree.Update_Sources (GPR2.Sources_Units_Artifacts)
   then
      return 1;
   end if;

   Root_View := Tree.Namespace_Root_Projects.First_Element;

   declare
      A : GBA.Thread.Report_Messages.Object;
   begin
      A.Initialize (Root_View, Ret_Code);

      if not Tree.Artifacts_Database.Add_Action (A) then
         return 1;
      end if;
   end;

   --  Always run the action, so that a signature left over by a previous run
   --  does not turn it into a skipped action.

   Exec_Opts.Force := True;

   if Tree.Artifacts_Database.Execute (Scheduler, Exec_Opts, Make_JS) =
        GPR2.Build.Actions_Scheduler.Success
   then
      Ada.Text_IO.Put_Line ("execution succeeded");
   else
      Ada.Text_IO.Put_Line ("execution failed");
   end if;

   return 0;
end Test;
