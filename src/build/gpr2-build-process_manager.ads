--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.OS.FS; use GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with GPR2.Build.Actions;
with GPR2.Build.Tree_Db;

private with GNATCOLL.Traces;

package GPR2.Build.Process_Manager is

   type Collect_Status is
      (Continue_Execution,
       Abort_Execution,
       Retry_Job);
   --  Status return by Collect_Job method.
   --
   --  Continue_Execution: iteration can continue,
   --  Retry_Job: job should be requeued.
   --  Abort_Execution: abort process manager

   type Execution_Verbosity is
     (Quiet,
      Minimal,
      Verbose,
      Very_Verbose);

   type Object is tagged limited private;

   PROCESS_STATUS_OK : constant Integer;

   type Process_Handler_Status is
     (Skipped, Failed_To_Launch, Running, Finished);

   type Process_Handler (Status : Process_Handler_Status := Running) is
   record
      case Status is
         when Running =>
            Handle : GNATCOLL.OS.Process.Process_Handle;
         when Failed_To_Launch =>
            Error_Message : Unbounded_String;
         when Finished =>
            Process_Status : Integer;
         when others =>
            null;
      end case;
   end record;

   function Collect_Job
     (Self           : in out Object;
      Job            : in out Actions.Object'Class;
      Proc_Handler   : Process_Handler;
      Stdout, Stderr : Unbounded_String)
      return Collect_Status;

   procedure Launch_Job
     (Self           : in out Object;
      Job            : in out Actions.Object'Class;
      Proc_Handler   : out Process_Handler;
      Capture_Stdout : out File_Descriptor;
      Capture_Stderr : out File_Descriptor);

   procedure Execute
     (Self         : in out Object;
      Tree_Db      : GPR2.Build.Tree_Db.Object_Access;
      Jobs         : Natural := 0;
      Verbosity    : Execution_Verbosity := Minimal;
      Stop_On_Fail : Boolean := True);

   procedure Execution_Post_Process (Self : in out Object) is null;
   --  ??? Did not manage to have this subprogram in the private part
   --  and to be overrided by childs

   ----------------------------------------
   -- Process scheduler data information --
   ----------------------------------------

   function Max_Active_Jobs (Self : Object) return Natural;

   function Total_Jobs (Self : Object) return Natural;

private

   type Process_Manager_Stats is record
      Max_Active_Jobs : Natural := 0;
      Total_Jobs      : Natural := 0;
   end record;

   Empty_Stats : constant Process_Manager_Stats :=
                   (Max_Active_Jobs => 0,
                    Total_Jobs      => 0);

   PROCESS_STATUS_OK : constant Integer := 0;

   type Object is tagged limited record
      Stats        : Process_Manager_Stats := Empty_Stats;
      Tree_Db      : GPR2.Build.Tree_Db.Object_Access;
      Traces       : GNATCOLL.Traces.Trace_Handle :=
                       GNATCOLL.Traces.Create ("PROCESS_MANAGER");
      Stop_On_Fail : Boolean := True;
      Verbosity    : Execution_Verbosity;
   end record;

end GPR2.Build.Process_Manager;
