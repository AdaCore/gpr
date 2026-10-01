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
with GPR2.Utils.Hash;

use GPR2;

function Test return Integer is

   Actions : constant := 3;
   --  The first run forces this many actions to execute

   Opts      : GPR2.Options.Object;
   Exec_Opts : GPR2.Build.Actions_Scheduler.Options;
   Make_JS   : GPR2.Build.Jobserver.Object;
   --  Never connected: this test does not run under make

   Tree      : GPR2.Project.Tree.Object;
   Scheduler : GPR2.Build.Actions_Scheduler.Object;
   Status    : GPR2.Build.Actions_Scheduler.Execution_Status;

   use type GPR2.Build.Actions_Scheduler.Execution_Status;
   use all type GPR2.Build.Actions_Scheduler.Report_Status;

   procedure Run_Again (Label_Text : String);

   ---------------
   -- Run_Again --
   ---------------

   procedure Run_Again (Label_Text : String) is
      First          : Boolean := True;
      Finished_Count : Natural := 0;
   begin
      loop
         declare
            Report : constant GPR2.Build.Actions_Scheduler.Action_Report :=
              Tree.Artifacts_Database.Execute_Next_Action
                (Clear_Exec_Ctxt => First);
         begin
            First := False;
            exit when Report.Status = No_Action_To_Execute;

            if Report.Status = Finished and then Report.Return_Code = 0 then
               Finished_Count := Finished_Count + 1;
            elsif Report.Status /= Skipped then
               raise Program_Error with "Unexpected action status";
            end if;
         end;
      end loop;

      Ada.Text_IO.Put_Line (Label_Text & Finished_Count'Image);
   end Run_Again;

begin
   Opts.Add_Switch (GPR2.Options.P, "tree/main.gpr");
   Exec_Opts.Jobs := 1;
   Exec_Opts.Force := True;

   if not Tree.Load (Opts, With_Runtime => False) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
      return 1;
   end if;

   if not Tree.Update_Sources (GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
      return 1;
   end if;

   for Index in 1 .. Actions loop
      declare
         Action : GPR2.Build.Actions.Thread.Always_Execute.Object;
      begin
         Action.Initialize (Tree.Root_Project, Index, Force => False);

         if not Tree.Artifacts_Database.Add_Action (Action) then
            Ada.Text_IO.Put_Line ("Failed to add action" & Index'Image);
            return 1;
         end if;
      end;
   end loop;

   Status := Tree.Artifacts_Database.Execute (Scheduler, Exec_Opts, Make_JS);

   if Status /= GPR2.Build.Actions_Scheduler.Success then
      Ada.Text_IO.Put_Line ("Scheduler failed");
      return 1;
   end if;

   --  The scheduler opens one generation when it starts and one after each
   --  executed action, so that a file an action modifies is stat'ed again
   --  for the next one.

   Ada.Text_IO.Put_Line
     ("actions:" & Natural'Image (Actions)
      & "  file index generations:"
      & Natural'Image (Tree.Artifacts_Database.File_Indexer.Generations));

   --  Trust the input's hash even if the fixture was created recently.

   declare
      Digest : constant GPR2.Utils.Hash.Hash_Digest :=
        Tree.Artifacts_Database.File_Indexer.Hash_File
          (Tree.Root_Project.Dir_Name.Compose ("foo.ads").Value,
           Force_Cache => True);
      pragma Unreferenced (Digest);
   begin
      Run_Again ("unchanged input, executed:");
   end;

   --  Restarting single-action execution must see changes to cached inputs.

   declare
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.Append_File, "tree/foo.ads");
      Ada.Text_IO.Put_Line (File, "");
      Ada.Text_IO.Put_Line (File, "-- Changed between executions");
      Ada.Text_IO.Close (File);
   end;

   Run_Again ("changed input, executed:");

   return 0;
end Test;
