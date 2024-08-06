--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.Directed_Graph; use GNATCOLL.Directed_Graph;

with GPR2.Build.Actions; use GPR2.Build.Actions;
with GPR2.Log;
with GPR2.Message.Reporter;
with GPR2.Path_Name;
with GPR2.Source_Reference;

package body GPR2.Build.Process_Manager is

   -----------------
   -- Collect_Job --
   -----------------

   overriding
   function Collect_Job
      (Self           : in out Object;
       Job            : DG.Node_Id;
       Proc_Handler   : Process_Handler;
       Stdout, Stderr : Unbounded_String)
      return Collect_Status
   is
      Act : GPR2.Build.Actions.Object'Class :=
              Self.Tree_Db.Action (Self.Tree_Db.Action_Id (Job));

   begin
      pragma Assert
        (Proc_Handler.Status /= Running,
         "The process linked to the action '" & Act.UID.Image &
           "' is still running. Cannot collect the job before it finishes");

      if Proc_Handler.Status = Failed_To_Launch
        and then Self.Stop_On_Fail
      then
         return Abort_Execution;
      end if;

      if Proc_Handler.Status = Finished then
         if Self.Traces.Is_Active then
            Self.Traces.Trace
              ("Job '" & Act.UID.Image & "' returned. Status:" &
                 Proc_Handler.Process_Status'Img & ", output: '" &
                 To_String (Stdout) & "'" & ", stderr: '" &
                 To_String (Stderr) & "'");
         end if;

         if Proc_Handler.Process_Status /= PROCESS_STATUS_OK then
            Message.Reporter.Active_Reporter.Report
              (Message.Create
                 (Message.Warning,
                  Act.UID.Image & " failed with status" &
                    Proc_Handler.Process_Status'Image & ASCII.LF &
                    To_String (Stdout) & To_String (Stderr),
                  Source_Reference.Create (Act.View.Path_Name.Value, 0, 0)));
         end if;
      end if;

      Act.Post_Command;

      if Proc_Handler.Status = Finished then
         if Proc_Handler.Process_Status = PROCESS_STATUS_OK then
            Act.Compute_Signature;

         elsif Self.Stop_On_Fail then
            return Abort_Execution;
         end if;
      end if;

      --  We do not want to manipulate reference types during post commands
      --  procedure as it would prevent actions addition / deletion.

      Self.Tree_Db.Action_Id_To_Reference
        (Self.Tree_Db.Action_Id (Job)) := Act;

      return Continue_Execution;
   end Collect_Job;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Self         : in out Object;
      Tree_Db      : GPR2.Build.Tree_Db.Object_Access;
      Jobs         : Natural := 0;
      Stop_On_Fail : Boolean := True)
   is
   begin
      Self.Tree_Db := Tree_Db;
      Self.Stop_On_Fail := Stop_On_Fail;
      Self.Execute (Self.Tree_Db.Actions_Graph_Access.all, Jobs);
   end Execute;

   ----------------
   -- Launch_Job --
   ----------------

   overriding
   procedure Launch_Job
      (Self           : in out Object;
       Job            : DG.Node_Id;
       Proc_Handler   : out Process_Handler;
       Capture_Stdout : out File_Descriptor;
       Capture_Stderr : out File_Descriptor)
   is
      package FS renames GNATCOLL.OS.FS;

      function Image (Command : Argument_List) return String;

      -----------
      -- Image --
      -----------

      function Image (Command : Argument_List) return String is
         Result : Unbounded_String;
      begin
         for Arg of Command loop
            if Length (Result) > 0 then
               Append (Result, " ");
            end if;

            Append (Result, Arg);
         end loop;

         return -Result;
      end Image;

      P_Wo : FS.File_Descriptor;
      P_Ro : FS.File_Descriptor;
      P_We : FS.File_Descriptor;
      P_Re : FS.File_Descriptor;

      Act      : GPR2.Build.Actions.Object'Class :=
                   Self.Tree_Db.Action (Self.Tree_Db.Action_Id (Job));
      Args     : Argument_List;
      Env      : Environment_Dict;
      Cwd      : GPR2.Path_Name.Object;
      Messages : GPR2.Log.Object;

   begin

      Act.Compare_Signature (Messages);

      --  ??? Process Messages

      if Act.Valid_Signature then
         Trace
           (Self.Traces,
            "Signature is valid, do not execute the job '" &
            Self.Tree_Db.Action_Id (Job).Image & "'");
         Proc_Handler := Process_Handler'(Status => Skipped);

         return;
      end if;

      Act.Compute_Command (Args, Env);
      Cwd := Act.Working_Directory;

      if Act.Skip then
         Self.Traces.Trace
           ("job asked to be skipped: " & Act.UID.Image);
         Proc_Handler := Process_Handler'(Status => Skipped);

         return;

      elsif Args.Is_Empty then
         Self.Traces.Trace
           ("job arguments is empty, skipping '"  & Act.UID.Image & "'");
         Proc_Handler := Process_Handler'(Status => Skipped);

         return;
      end if;

      if Self.Traces.Is_Active then
         Trace
           (Self.Traces,
            "Signature is invalid. Execute the job " &
              Self.Tree_Db.Action_Id (Job).Image & ", command: " &
              Image (Args));
      end if;

      FS.Open_Pipe (P_Ro, P_Wo);
      FS.Open_Pipe (P_Re, P_We);

      begin
         --  ??? Both message level and Project tree verbosity don't cope with
         --  tooling messages that need quiet/normal/detailed info. Let's go
         --  for the default one *and* verbose one for now
         Message.Reporter.Active_Reporter.Report
           (Act.UID.Image);
         Message.Reporter.Active_Reporter.Report
           (Image (Args));
         Proc_Handler :=
           (Status => Running,
            Handle => Start
              (Args        => Args,
               Env         => Env,
               Cwd         => Cwd.String_Value,
               Stdout      => P_Wo,
               Stderr      => P_We,
               Inherit_Env => True));
      exception
         when Ex : GNATCOLL.OS.OS_Error =>
            FS.Close (P_Wo);
            FS.Close (P_We);

            GPR2.Message.Reporter.Active_Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  Args.First_Element & ": " &
                    Ada.Exceptions.Exception_Message (Ex),
                  GPR2.Source_Reference.Create
                    (Act.View.Path_Name.Value, 0, 0)));

            Proc_Handler :=
              (Status        => Failed_To_Launch,
               Error_Message => To_Unbounded_String
                 ("Command '" & Image (Args) & "' failed: " &
                  Ada.Exceptions.Exception_Message (Ex)));
            return;
      end;

      FS.Close (P_Wo);
      FS.Close (P_We);

      Capture_Stdout := P_Ro;
      Capture_Stderr := P_Re;
   end Launch_Job;

end GPR2.Build.Process_Manager;
