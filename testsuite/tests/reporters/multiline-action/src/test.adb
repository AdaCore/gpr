--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;

with GPR2.Build.Actions.Process.Multiline;
with GPR2.Build.Actions_Scheduler;
with GPR2.Build.Jobserver;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Set;

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

   package GBA renames GPR2.Build.Actions;
   use type GPR2.Build.Actions_Scheduler.Execution_Status;

   Executable : constant GPR2.Path_Name.Object :=
                  GPR2.Path_Name.Create_File
                    (Name => "printer", Directory => "printer");

   A : GBA.Process.Multiline.Object;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load
            (Opts,
             With_Runtime        => True,
             Absent_Dir_Error    => No_Error,
             Create_Missing_Dirs => GPR2.Project.Tree.Create_Always)
     or else not Tree.Update_Sources (GPR2.Sources_Units_Artifacts)
   then
      Ada.Text_IO.Put_Line ("failed to load the tree");
      return 1;
   end if;

   Root_View := Tree.Namespace_Root_Projects.First_Element;

   A.Initialize (Root_View, Executable);

   if not Tree.Artifacts_Database.Add_Action (A) then
      Ada.Text_IO.Put_Line ("failed to add the action");
      return 1;
   end if;

   --  Run it through the scheduler: the captured multi-line stdout is relayed
   --  to the console reporter in a single Report call.
   Exec_Opts.Jobs := 1;

   if Tree.Artifacts_Database.Execute (Scheduler, Exec_Opts, Make_JS)
        /= GPR2.Build.Actions_Scheduler.Success
   then
      Ada.Text_IO.Put_Line ("execute detected errors");
   end if;

   return 0;
end Test;
