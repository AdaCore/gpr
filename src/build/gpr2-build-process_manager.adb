--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.Directed_Graph; use GNATCOLL.Directed_Graph;
with GPR2.Build.Actions; use GPR2.Build.Actions;
with GPR2.Log;
with Ada.Exceptions;
with Ada.Text_IO;


package body GPR2.Build.Process_Manager is

   -----------------
   -- Collect_Job --
   -----------------

   overriding
   function Collect_Job
      (Self           : in out Object;
       Job            : DG.Node_Id;
       Proc_Status    : Integer;
       Stdout, Stderr : Unbounded_String)
      return Collect_Status
   is
      Act : GPR2.Build.Actions.Object'Class :=
        Self.Tree_Db.Action (Self.Tree_Db.Action_Id (Job));
   begin

      Trace
        (Self.Traces,
         "Job '" & Act.UID.Image & "' returned. Status:" &
         Proc_Status'Img & ", output: '" & To_String (Stdout) & "'" &
         ", stderr: '" & To_String (Stderr) & "'");

      if Proc_Status /= 0 and then Proc_Status /= 127 then
         --  ??? Move this message in the log system
         Ada.Text_IO.Put_Line
           ("Job '" & Act.UID.Image & "' failed. Status:" &
            Proc_Status'Img & ", output: '" & To_String (Stdout) & "'" &
            ", stderr: '" & To_String (Stderr) & "'");
         return Abort_Execution;
      end if;

      Act.Post_Command;
      Act.Compute_Signature;

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
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Jobs    : Natural := 0)
   is
   begin
      Self.Tree_Db := Tree_Db;
      Self.Execute (Self.Tree_Db.Actions_Graph_Access.all, Jobs);
   end Execute;

   ----------------
   -- Launch_Job --
   ----------------

   overriding
   procedure Launch_Job
      (Self           : in out Object;
       Job            : DG.Node_Id;
       Proc_Handle    : out Process_Handle;
       Capture_Stdout : out File_Descriptor;
       Capture_Stderr : out File_Descriptor)
   is
      package FS renames GNATCOLL.OS.FS;

      P_Wo : FS.File_Descriptor;
      P_Ro : FS.File_Descriptor;
      P_We : FS.File_Descriptor;
      P_Re : FS.File_Descriptor;

      Act      : GPR2.Build.Actions.Object'Class :=
                   Self.Tree_Db.Action (Self.Tree_Db.Action_Id (Job));
      Args     : constant Argument_List          := Act.Command;
      Command  : Unbounded_String;
      Messages : GPR2.Log.Object;
   begin

      Act.Compare_Signature (Messages);

      --  ??? Process Messages

      if Act.Valid_Signature then
         Trace (Self.Traces, "Signature is valid, do not execute the job '" &
            Self.Tree_Db.Action_Id (Job).Image & "'");
         Proc_Handle := Invalid_Handle;

         return;
      end if;

      for Arg of Args loop
         Command := Command & To_Unbounded_String (Arg & " ");
      end loop;

      Trace (Self.Traces, "Signature is invalid. Execute the job " &
        Self.Tree_Db.Action_Id (Job).Image & ", command: " &
        To_String (Command));

      FS.Open_Pipe (P_Ro, P_Wo);
      FS.Open_Pipe (P_Re, P_We);
      Proc_Handle := Start (Args => Args, Stdout => P_Wo, Stderr => P_We);
      FS.Close (P_Wo);
      FS.Close (P_We);

      Capture_Stdout := P_Ro;
      Capture_Stderr := P_Re;

      exception
         when Ex : others =>
            Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Ex));
            raise;
   end Launch_Job;

end GPR2.Build.Process_Manager;