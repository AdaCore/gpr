--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories;
with Ada.Text_IO;

with GPR2.Build.Actions.Write_File;
with GPR2.Build.Process_Manager.JSON;

with GPR2.Options;
with GPR2.Path_Name;

with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter.Console;

with GNATCOLL.VFS; use GNATCOLL.VFS;

use GPR2, GPR2.Reporter;

function Test return Integer is
   Tree      : GPR2.Project.Tree.Object;
   Opts      : GPR2.Options.Object;
   Project   : constant String := "tree/main.gpr";
   Process_M : GPR2.Build.Process_Manager.JSON.Object;
   Root_View : GPR2.Project.View.Object;
   Exec_Opts : GPR2.Build.Process_Manager.PM_Options;

   package GBA renames GPR2.Build.Actions;
   use type GPR2.Build.Process_Manager.Execution_Status;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, True, Console.Create (Quiet))
     or else not Tree.Update_Sources (GPR2.Sources_Units_Artifacts)
   then
      return 1;
   end if;

   Root_View := Tree.Namespace_Root_Projects.First_Element;

   for Action_Index in 1 .. 10 loop
      --  Execute serially
      --  CPU1: 1 -> 2 -> 3 -> 4 -> 5
      --  CPU2: 6 -> 7 -> 8 -> 9 -> 10
      --
      --  DAG perspective:
      --  All nodes depend on their predecessor note, except 1 and 6
      --
      --  To have repeatable output, we artificially make
      --  6 wait for 5.txt
      --  5 wait for 10.txt
      --  so tat we artificially block CPU slot 2 while CPU slot 1 executes
      --  almost all of its pipeline, then blocks on the last action to have
      --  the pipeline of slot 2 actually executed there.

      declare
         A          : GBA.Write_File.Object;
         Ret_Code   : Integer               := 0;
         With_Deps  : Boolean               := Action_Index not in 1 | 6;
         with_Wait  : Natural               := (if Action_Index = 6 then 5
                                                elsif Action_Index = 5 then 10
                                                else 0);

         Executable : GPR2.Path_Name.Object :=
                        GPR2.Path_Name.Create_File
                          (Name => "write_file", Directory => "write_file");
      begin
         A.Initialize
           (Root_View,
            Action_Index,
            Executable,
            Ret_Code,
            With_Deps,
            With_Wait);
         if not Tree.Artifacts_Database.Add_Action (A) then
            return 1;
         end if;
      end;
   end loop;

   Process_M.Set_JSON_File (Path_Name.Create_File ("jobs.json"));

   Exec_Opts.Jobs := 2;

   if Tree.Artifacts_Database.Execute (Process_M, Exec_Opts) /=
     GPR2.Build.Process_Manager.Success
   then
      return 1;
   else
      return 0;
   end if;
end Test;
