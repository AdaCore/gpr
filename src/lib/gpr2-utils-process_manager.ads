--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Core infrastructure to implement a process manager for a set of jobs
--  ordered using a DAG.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Directed_Graph;
with GNATCOLL.OS.Process;
with GNATCOLL.OS.FS;

package GPR2.Utils.Process_Manager is

   package DG renames GNATCOLL.Directed_Graph;
   package Proc renames GNATCOLL.OS.Process;
   package FS renames GNATCOLL.OS.FS;

   type Collect_Status is
      (Continue_Execution,
       Abort_Execution,
       Retry_Job);
   --  Status return by Collect_Job method.
   --
   --  Continue_Execution: iteration can continue,
   --  Retry_Job: job should be requeued.
   --  Abort_Execution: abort process manager

   type Process_Manager is abstract tagged limited private;

   procedure Launch_Job
      (Self           : in out Process_Manager;
       Job            : DG.Node_Id;
       Process        : out Proc.Process_Handle;
       Capture_Stdout : out FS.File_Descriptor;
       Capture_Stderr : out FS.File_Descriptor) is abstract;
   --  Launch a job in background and return its Handle
   --
   --  If Process is set to Invalid_Handle then Collect_Job is called with a
   --  Process_Status set to 127 and Stdout and Stderr set to empty string.
   --
   --  If Process is a valid handle then the manager wait for the end of the
   --  process. If Capture_Stdout and/or Capture_Stderr are set to a valid
   --  file descriptor then the manager listen to these file descriptors to
   --  fetch respectively the stdout and/or stderr of the process.

   --  Warning: when capturing output, the user need to open a pipe, set the
   --  writable side as output of the process, spawn the process, close that
   --  side of the pipe and return in Capture_Output the readable side of the
   --  pipe. Not closing the writable side of the pipe causes process hanging.

   function Collect_Job
      (Self           : in out Process_Manager;
       Job            : DG.Node_Id;
       Process_Status : Integer;
       Stdout, Stderr : Unbounded_String)
      return Collect_Status;
   --  Called on each job termination. Stdout, Stderr are set to the captured
   --  stdout and/or stderr if Capture_Stdout and/or Capture_Stderr were set
   --  to a valid file descriptor on process launch. Otherwise they are set to
   --  the empty string.

   procedure Execute
     (Self  : in out Process_Manager'Class;
      Graph : in out DG.Directed_Graph;
      Jobs  : Natural := 0);
   --  Run the jobs identified in the DAG until all nodes of the DAG are
   --  processed or a failure occurs.
   --  Graph: the graph used to iterate the jobs to be processed
   --  Jobs: the maximum number of jobs that can execute in parallel.
   --    If set to 0, then the number of CPU of the host is used.

   procedure Execution_Post_Process (Self : in out Process_Manager);
   --  ??? Did not manage to have this subprogram in the private part
   --  and to be overrided by childs

   ----------------------------------------
   -- Process scheduler data information --
   ----------------------------------------

   function Max_Active_Jobs (Self : Process_Manager) return Natural;

   function Total_Jobs (Self : Process_Manager) return Natural;


private


   type Process_Manager_Data is record
      Max_Active_Jobs : Natural := 0;
      Total_Jobs      : Natural := 0;
   end record;

   No_Data : constant Process_Manager_Data :=
               (Max_Active_Jobs => 0,
                Total_Jobs      => 0);

   type Process_Manager is abstract tagged limited record
      Data : Process_Manager_Data := No_Data;
   end record;

   procedure Clear_Data (Self : in out Process_Manager);

end GPR2.Utils.Process_Manager;
