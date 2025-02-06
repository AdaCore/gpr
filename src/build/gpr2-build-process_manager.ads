--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Directed_Graph;
with GNATCOLL.OS.FS; use GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;

with GPR2.Build.Actions;
limited with GPR2.Build.Tree_Db;

private with GNATCOLL.Traces;

package GPR2.Build.Process_Manager is

   Process_Manager_Error : exception;

   type PM_Options is record
      Jobs            : Natural := 1;
      Force           : Boolean := False;
      Stop_On_Fail    : Boolean := True;
      Keep_Temp_Files : Boolean := False;
      Show_Progress   : Boolean := False;
   end record;

   type Collect_Status is
      (Continue_Execution,
       Abort_Execution);
   --  Status return by Collect_Job method.
   --
   --  Continue_Execution: iteration can continue,
   --  Abort_Execution: abort process manager

   type Object is tagged limited private;

   type Process_Handler_Status is
     (Skipped, Deactivated, Failed_To_Launch, Running, Finished);

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

   package Action_Node_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (GPR2.Build.Actions.Action_Id'Class,
      GNATCOLL.Directed_Graph.Node_Id,
      GPR2.Build.Actions."<",
      GNATCOLL.Directed_Graph."=");

   package Node_Action_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (GNATCOLL.Directed_Graph.Node_Id,
      GPR2.Build.Actions.Action_Id'Class,
      GNATCOLL.Directed_Graph."<",
      GPR2.Build.Actions."=");

   type Process_Execution_Context is record
      Graph   : GNATCOLL.Directed_Graph.Directed_Graph;
      Actions : Node_Action_Maps.Map;
      Nodes   : Action_Node_Maps.Map;
      Errors  : Boolean := False;
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
      Slot_Id        :        Positive;
      Force          :        Boolean;
      Proc_Handler   :    out Process_Handler;
      Capture_Stdout :    out File_Descriptor;
      Capture_Stderr :    out File_Descriptor);
   --  Execute the Action "Job", possibly using a response file if the
   --    command line exceeds the maximum size authorized on the host.
   --  Response_File indicates the kind of response file the tool allows.
   --  Slot_Id identifies an Id that is unique during execution (cannot be
   --    re-used until the action is finished).
   --  Proc_Handler indicates the status of the operation, in particular if
   --    the job was skipped, is launched, or if an error occurred.
   --  Capture_Stdout/err are file descriptors used to capture the spawned
   --    process standard output and error.

   procedure Execute
     (Self            : in out Object;
      Tree_Db         : GPR2.Build.Tree_Db.Object_Access;
      Context         : access Process_Execution_Context;
      Options         : PM_Options);

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

   type Object is tagged limited record
      Stats        : Process_Manager_Stats := Empty_Stats;
      Tree_Db      : access GPR2.Build.Tree_Db.Object;
      Traces       : GNATCOLL.Traces.Trace_Handle :=
                       GNATCOLL.Traces.Create ("PROCESS_MANAGER");
   end record;

end GPR2.Build.Process_Manager;
