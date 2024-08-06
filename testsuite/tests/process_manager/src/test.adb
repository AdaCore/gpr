--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with GPR2.Build.Actions.Write_File;
with GPR2.Build.Process_Manager.JSON;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNATCOLL.VFS; use GNATCOLL.VFS;

use GPR2;

function Test return Integer is
   Tree      : GPR2.Project.Tree.Object;
   Opts      : GPR2.Options.Object;
   Log       : GPR2.Log.Object;
   Project   : constant String := "tree/main.gpr";
   Process_M : GPR2.Build.Process_Manager.JSON.Object;
   Root_View : GPR2.Project.View.Object;

   Scenario_Idx : Integer := Integer'Value (Ada.Command_Line.Argument (1));
   --  To ease the testing, this file contains all the tests scenarios.
   --  The scenario to run is given by test.py

   package GBA renames GPR2.Build.Actions;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => True) then
      return 1;
   end if;

   Tree.Update_Sources
     (Option => GPR2.Sources_Units_Artifacts, Messages => Log);

   if Log.Has_Error then
      Log.Output_Messages;
      return 1;
   end if;

   Log.Clear;

   Root_View := Tree.Namespace_Root_Projects.First_Element;

   case Scenario_Idx is
      when 1 =>
         ----
         --  Scenario 1
         --
         --  All actions pass correctly.
         ----

         for Action_Index in 1 .. 3 loop
            declare
               A          : GBA.Write_File.Object;
               Ret_Code   : Integer               := 0;
               With_Deps  : Boolean               := True;
               Executable : GPR2.Path_Name.Object :=
                 GPR2.Path_Name.Create_File
                   (Name => "write_file", Directory => "write_file");
            begin
               A.Initialize
                 (Root_View, Action_Index, Executable, Ret_Code, With_Deps);
               Tree.Artifacts_Database.Add_Action (A, Log);

               if Log.Has_Error then
                  Log.Output_Messages (Warning => False);

                  return 1;
               end if;
            end;
         end loop;

      when 2 =>
         ----
         --  Scenario 2
         --
         --  Action n. 2 returns an erroneous code
         ----

         for Action_Index in 1 .. 3 loop
            declare
               A          : GBA.Write_File.Object;
               Ret_Code   : Integer;
               With_Deps  : Boolean               := True;
               Executable : GPR2.Path_Name.Object :=
                 GPR2.Path_Name.Create_File
                   (Name => "write_file", Directory => "write_file");
            begin
               if Action_Index = 2 then
                  Ret_Code := 1;
               else
                  Ret_Code := 0;
               end if;

               A.Initialize
                 (Root_View, Action_Index, Executable, Ret_Code, With_Deps);
               Tree.Artifacts_Database.Add_Action (A, Log);

               if Log.Has_Error then
                  Log.Output_Messages (Warning => False);

                  return 1;
               end if;
            end;
         end loop;

      when 3 =>
         ----
         --  Scenario 3
         --
         --  Action n. 5 does not depend on action n. 4
         ----

         --  To have repeatable output, we ensure that Write_File wait
         --  other instances to be finished before actually executing.

         declare
            A          : GBA.Write_File.Object;
            Ret_Code   : Integer               := 0;
            With_Deps  : Boolean;
            Executable : GPR2.Path_Name.Object :=
                           GPR2.Path_Name.Create_File
                             (Name      => "write_file",
                              Directory => "write_file");
            With_Wait  : Natural;

         begin
            for Action_Index in 1 .. 10 loop
               if Action_Index = 5 then
                  With_Deps := False;
               else
                  With_Deps := True;
               end if;

               if Action_Index = 1 then
                  With_Wait := 10;
               else
                  With_Wait := 0;
               end if;

               A.Initialize
                 (Root_View,
                  Action_Index,
                  Executable,
                  Ret_Code,
                  With_Deps,
                  With_Wait);
               Tree.Artifacts_Database.Add_Action (A, Log);

               if Log.Has_Error then
                  Log.Output_Messages (Warning => False);

                  return 1;
               end if;
            end loop;
         end;

      when 4 =>
         ----
         --  Scenario 4
         --
         --  Action n. 3 does not have a valid executable
         ----

         for Action_Index in 1 .. 5 loop
            declare
               A                  : GBA.Write_File.Object;
               Ret_Code           : Integer               := 0;
               With_Deps          : Boolean               := True;
               Valid_Executable   : GPR2.Path_Name.Object :=
                 GPR2.Path_Name.Create_File
                   (Name => "write_file", Directory => "write_file");
               Invalid_Executable : GPR2.Path_Name.Object :=
                 GPR2.Path_Name.Create_File
                   (Name => "exec_that_does_not_exist");
            begin
               if Action_Index = 3 then
                  A.Initialize
                    (Root_View, Action_Index, Invalid_Executable,
                     Ret_Code, With_Deps);
               else
                  A.Initialize
                    (Root_View, Action_Index, Valid_Executable,
                     Ret_Code, With_Deps);
               end if;

               Tree.Artifacts_Database.Add_Action (A, Log);

               if Log.Has_Error then
                  Log.Output_Messages (Warning => False);

                  return 1;
               end if;
            end;
         end loop;
      when others =>
         null;
   end case;

   Process_M.Execute (Tree.Artifacts_Database, 2);

   return 0;
end Test;
