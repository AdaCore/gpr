--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Text_IO;
with GPR2.Build.Actions_Population;
with GPR2.Build.Actions_Scheduler;
with GPR2.Options;
with GPR2.Build.Options;
with GPR2.Project.Tree;
with GNATCOLL.Traces;

function Test return Integer is

   use GPR2;
   use all type GPR2.Build.Actions_Scheduler.Action_Report;
   use all type GPR2.Build.Actions_Scheduler.Report_Status;
   use GPR2.Options;

   Prj     : Project.Tree.Object;
   Opt     : GPR2.Options.Object;
   DAG_Opt : Build.Options.Build_Options;

   Nb_Of_Actions : Natural := 0;
begin
   GNATCOLL.Traces.Parse_Config_File;

   Opt.Add_Switch (Options.P, "tree/app.gpr");

   if Prj.Load
        (Opt,
         Absent_Dir_Error    => No_Error,
         With_Runtime        => True,
         Create_Missing_Dirs => GPR2.Project.Tree.Create_Always)
   then
      Prj.Update_Sources;
   else
      Ada.Text_IO.Put_Line ("Failed to load sources");
      return 1;
   end if;

   if not Build.Actions_Population.Populate_Actions (Prj, DAG_Opt, True) then
      Ada.Text_IO.Put_Line ("Failed to populate actions");
      return 2;
   end if;

   while Prj.Artifacts_Database.Execute_Next_Action.Status
     /= No_Action_To_Execute
   loop
      Nb_Of_Actions := Nb_Of_Actions + 1;
   end loop;

   Ada.Text_IO.Put_Line (Nb_Of_Actions'Image & " actions have been executed");

   return 0;
end Test;
