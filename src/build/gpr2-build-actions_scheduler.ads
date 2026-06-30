--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GPR2.Build.Jobserver;

with GPR2.Path_Name;
with GNATCOLL.Directed_Graph;
with GNATCOLL.OS.Process;
with GPR2.Build.Actions;
limited with GPR2.Build.Tree_Db;

--  This package provides the core execution engine for the GPR2 build
--  system. It schedules, launches, and collects the results of build actions
--  (compilations, linking, etc.) by traversing a directed acyclic graph (DAG)
--  of action dependencies.
--
--  The scheduler supports parallel execution via configurable job slots and
--  integrates with the Make jobserver protocol to coordinate global
--  parallelism limits. It handles both process-based actions (OS subprocess
--  execution with stdout/stderr capture) and thread-based actions (in-process
--  execution via Ada tasks).
--
--  Typical usage:
--    1. Build a Context containing the dependency graph and action mappings.
--    2. Configure an Options record with desired parallelism, force rebuild,
--       and other settings.
--    3. Call Execute, which blocks until all actions are completed or an
--       abort condition is met.
--    4. Inspect Context.Status for overall execution outcome.

package GPR2.Build.Actions_Scheduler is

   Actions_Scheduler_Error : exception;
   --  Raised on fatal internal errors (e.g., invalid scheduler state)

   type Object is tagged limited private;
   --  The main scheduler object. Holds a reference to the Tree_Db, tracks
   --  execution progress, and optionally integrates with a Make jobserver.

   type Options is tagged record
      Force : Boolean := False;
      --  When set, this forces the (re-)execution of the actions.

      Stop_On_Fail : Boolean := True;
      --  If unset, the actions scheduler will try to continue executing the
      --  actions after a failure.

      Keep_Temp_Files : Boolean := False;
      --  When set, the temporary files are not deleted after execution

      Show_Progress : Boolean := False;
      --  Displays extra information on the number of executed action and
      --  the total number of actions.

      No_Warnings_Replay : Boolean := False;
      --  When set, when a job is skipped (signature OK), do not provide the
      --  saved stderr but an empty string.
      --  Else provide the saved signature

      Jobs : Natural := 0;
      --  Number of jobs to execute in parallel. 0 will autodetect the
      --  number of CPUs available and use that value.

      Script_File : Path_Name.Object;
      --  When defined, it indicates a file where we will store the commands
      --  that have been executed during the run.

      Force_Jobserver : Boolean := False;
      --  When set, if we don't have a jobserver protocol defined, stop the
      --  actions scheduler.

   end record;

   type Execution_Status is
     (Success, --  successful overall executions
      Errors,  --  some actions reported errors
      Failed); --  some actions failed to execute

   package Action_Node_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (GPR2.Build.Actions.Action_Id'Class,
        GNATCOLL.Directed_Graph.Node_Id,
        GPR2.Build.Actions."<",
        GNATCOLL.Directed_Graph."=");

   package Node_Action_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (GNATCOLL.Directed_Graph.Node_Id,
        GPR2.Build.Actions.Action_Id'Class,
        GNATCOLL.Directed_Graph."<",
        GPR2.Build.Actions."=");

   type Context is record
      Graph   : GNATCOLL.Directed_Graph.Directed_Graph;
      Actions : Node_Action_Maps.Map;
      Nodes   : Action_Node_Maps.Map;
      Status  : Execution_Status := Success;
   end record;
   --  Contains information required for the execution that comes from the
   --  Tree DB. Success is used by the tree to report overall status.

   procedure Clear (Ctxt : in out Context);
   --  Clear the context by resetting all fields to their initial values

   procedure Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Context : access GPR2.Build.Actions_Scheduler.Context;
      Options : GPR2.Build.Actions_Scheduler.Options'Class);
   --  Execute the actions scheduler. Iterates over the
   --  directed graph in Context, launching and collecting
   --  actions according to the given Options.

   -----------------------------
   -- SINGLE ACTION EXECUTION --
   -----------------------------

   type Report_Status is
     (No_Action_To_Execute,
      --  The DAG does not contain actions to execute

      Skipped,
      --  Signature is still valid; no re-execution needed

      Deactivated,
      --  Action is externally built or explicitly deactivated

      Failed_Cmd_Line_Computation,
      --  Command-line construction failed

      Failed_To_Launch,
      --  OS-level process launch failed

      Exception_Raised,
      --  An exception was raised during a thread action execution

      Failed_Pre_Execution,
      --  Pre_Execution hook returned False

      Finished
      --  Action execution completed (check return code for success)
     );
   --  Outcome of a single action execution attempt

   type Action_Report (Status : Report_Status) is tagged private;
   --  Result of a single Execute_Next_Action call. The discriminant Status
   --  describes what happened.

   function Execute_Next_Action
     (Tree_Db            : GPR2.Build.Tree_Db.Object_Access;
      Context            : access GPR2.Build.Actions_Scheduler.Context;
      Catch_Exceptions   : Boolean := True;
      Force_Execution    : Boolean := False;
      Keep_Temp_Files    : Boolean := False;
      No_Warnings_Replay : Boolean := False) return Action_Report;
   --  Pick the next eligible action from Context, execute it, and return an
   --  Action_Report describing the outcome. Returns a report with Status set
   --  to No_Action_To_Execute when the DAG is exhausted. This is the low-level
   --  entry point used by the Tree_Db wrapper; prefer
   --  Tree_Db.Execute_Next_Action for normal use.

   function Action_UID
     (Report : Action_Report) return GPR2.Build.Actions.Action_Id'Class
   with Pre => Report.Status /= No_Action_To_Execute;
   --  Return the action identifier specified in the report

   function Stdout (Report : Action_Report) return String
   with Pre => Report.Status /= No_Action_To_Execute;
   --  Return the standard output of the action report

   function Stderr (Report : Action_Report) return String
   with Pre => Report.Status /= No_Action_To_Execute;
   --  Return the standard error of the action report

   function Status (Report : Action_Report) return Report_Status;
   --  Return the status of the action report

   function Return_Code (Report : Action_Report) return Integer
   with Pre => Report.Status = Finished;
   --  Return the OS return code of the action report

private

   type Object is tagged limited record
      Previous_Progress : Natural := 0;
      Tree_Db           : access GPR2.Build.Tree_Db.Object;
      Make_JS           : GPR2.Build.Jobserver.Object;
      --  When used from make, the make job server to limit our number of
      --  simultaneous processes to what make accepts.
   end record;

   type Action_Status is
     (Unknown,
      --  Initial state before evaluation

      Skipped,
      --  Signature is still valid; no re-execution needed

      Deactivated,
      --  Action is externally built or explicitly deactivated

      Failed_Cmd_Line_Computation,
      --  Command-line construction failed

      Failed_To_Launch,
      --  OS-level process launch failed

      Exception_Raised,
      --  An exception was raised during a thread action execution

      Failed_Pre_Execution,
      --  Pre_Execution hook returned False

      Ready_To_Run,
      --  Action is ready to be dispatched to a runner

      Running,
      --  Action has been dispatched and is currently executing

      Finished,
      --  Action execution completed (check return code for success)

      Pending
      --  Waiting for a jobserver token before execution can begin
     );

   type Process_Handler (Status : Action_Status := Unknown) is record
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
   --  Discriminated record used by Process_Runner tasks to communicate
   --  the result of launching an OS process back to the main loop.

   type Collect_Status is (Continue_Execution, Abort_Execution);
   --  Status return by Collect_Action.
   --
   --  Continue_Execution: iteration can continue,
   --  Abort_Execution: abort actions scheduler

   type Collect_Handler (Status : Action_Status := Unknown) is record
      UID_Holder  : Actions.Action_Id_Holder.Holder;
      Action_Slot : Natural;
      Stdout      : Unbounded_String;
      Stderr      : Unbounded_String;

      case Status is
         when Finished =>
            Return_Code : Integer;

         when others =>
            null;
      end case;
   end record;
   --  Record passed through the collection queue after an action completes.
   --  Carries the action identity, assigned slot, captured output, and
   --  (for finished actions) the process return code.

   function Collect_Action
     (Self    : in out Object;
      Action  : in out Actions.Object'Class;
      Handler : Collect_Handler;
      Context : access GPR2.Build.Actions_Scheduler.Context)
      return Collect_Status;
   --  Collect the result of an action execution. Reports
   --  output and errors, runs post-commands, propagates
   --  newly created actions, writes signatures, and
   --  unlocks dependent actions in the graph. Returns
   --  Continue_Execution or Abort_Execution.

   type Action_Report (Status : Report_Status) is tagged record
      UID_Holder : Actions.Action_Id_Holder.Holder;
      Stdout     : Unbounded_String := Null_Unbounded_String;
      Stderr     : Unbounded_String := Null_Unbounded_String;
      case Status is
         when Finished =>
            Return_Code : Integer;

         when others =>
            null;
      end case;
   end record;

   function Action_UID
     (Report : Action_Report) return GPR2.Build.Actions.Action_Id'Class
   is (Report.UID_Holder.Element);

   function Stdout (Report : Action_Report) return String
   is (To_String (Report.Stdout));

   function Stderr (Report : Action_Report) return String
   is (To_String (Report.Stderr));

   function Status (Report : Action_Report) return Report_Status
   is (Report.Status);

   function Return_Code (Report : Action_Report) return Integer
   is (Report.Return_Code);
end GPR2.Build.Actions_Scheduler;
