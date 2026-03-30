--
--  Copyright (C) 2024-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.Build.Actions.Thread.Raise_Exception;
with GPR2.Build.Actions_Scheduler;

with GPR2.Options;
with GPR2.Path_Name;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNATCOLL.Traces;

use GPR2;

function Test return Integer is
   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Project     : constant String := "tree/main.gpr";
   Scheduler   : GPR2.Build.Actions_Scheduler.Object;
   Root_View   : GPR2.Project.View.Object;
   Exec_Opts   : GPR2.Build.Actions_Scheduler.Options;

   package GBA renames GPR2.Build.Actions;

   use type GPR2.Build.Actions_Scheduler.Execution_Status;
begin

   GNATCOLL.Traces.Parse_Config_File;

   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, True)
     or else not Tree.Update_Sources (GPR2.Sources_Units_Artifacts)
   then
      return 1;
   end if;

   Root_View := Tree.Namespace_Root_Projects.First_Element;

   declare
      A : GBA.Thread.Raise_Exception.Object;
   begin
      A.Initialize (Root_View);

      if not Tree.Artifacts_Database.Add_Action (A) then
         return 1;
      end if;
   end;

   if Tree.Artifacts_Database.Execute (Scheduler, Exec_Opts)
     = GPR2.Build.Actions_Scheduler.Success
   then
      Ada.Text_IO.Put_Line ("Error: action execution should have failed");
      return 1;
   end if;

   return 0;
end Test;
