--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

pragma Warnings (Off);
with System.Multiprocessors;
pragma Warnings (On);

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Synchronous_Task_Control;
with Ada.Unchecked_Deallocation;

with GNATCOLL.OS.FS;
with GNATCOLL.Traces;

with GPR2.Build.Actions.Process;
with GPR2.Build.Actions.Thread;
with GPR2.Build.Actions.Process.Link;
with GPR2.Build.Response_Files;
with GPR2.Build.Tree_Db;
with GPR2.Reporter;
with GPR2.Source_Reference;
with GPR2.Message;

package body GPR2.Build.Actions_Scheduler is
   use Ada.Synchronous_Task_Control;
   use GNATCOLL.OS.Process;
   use GNATCOLL.Directed_Graph;
   use GPR2.Build.Actions;
   use GPR2.Reporter;

   package GDG renames GNATCOLL.Directed_Graph;
   package GOP renames GNATCOLL.OS.Process;

   function Effective_Job_Number (N : Natural) return Natural;
   --  If N = 0 return the number of CPUs otherwise return N.

   function Image
     (Command : Argument_List; For_Script : Boolean := False) return String;
   --  Return the representation of the command

   procedure Internal_Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Context : access GPR2.Build.Actions_Scheduler.Context;
      Options : GPR2.Build.Actions_Scheduler.Options'Class);
   --  Execute the actions scheduler, which occurs after an option cast

   procedure Launch_Process
     (Job            : in out Actions.Process.Object'Class;
      Proc_Handler   : in out Process_Handler;
      Capture_Stdout : out GNATCOLL.OS.FS.File_Descriptor;
      Capture_Stderr : out GNATCOLL.OS.FS.File_Descriptor);
   --  Launch the given process action. On success,
   --  Proc_Handler is set to Running and Capture_Stdout/
   --  Capture_Stderr provide file descriptors for the
   --  captured output. On failure, Proc_Handler is set
   --  to Failed_To_Launch with an error message.

   function Collect_Action
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Action  : in out Actions.Object'Class;
      Handler : Collect_Handler;
      Context : access GPR2.Build.Actions_Scheduler.Context)
      return Collect_Status;

   subtype Pre_Run_Action_Status is Action_Status
   with
     Static_Predicate =>
       Pre_Run_Action_Status
       in Skipped | Deactivated | Ready_To_Run | Failed_Cmd_Line_Computation;

   function Pre_Run_Status
     (Action  : in out Actions.Object'Class;
      Slot_Id : Positive;
      Force   : Boolean) return Pre_Run_Action_Status;
   --  Determine the status of Action before execution.
   --  Returns Deactivated if the action is externally
   --  built or deactivated, Skipped if its signature is
   --  still valid and Force is False, or Ready_To_Run
   --  otherwise.


   procedure Display
     (Action  : Actions.Action_Id'Class;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access);
   --  Display information about the specified action execution

   function RF_Message
     (Resp_File_Path : Path_Name.Object; Command : Unbounded_String)
      return String;
   --  Return a message containig the path of the response file followed by the
   --  provided commands.

   procedure Display_Command_Line
     (Action  : Actions.Process.Object'Class;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access);
   --  Report the command about to be executed. At verbose level, prints the
   --  full argument list and any response file contents; otherwise delegates
   --  to Display for the compact one-line form.

   procedure Report_Deactivated_Invalid_Signature
     (Act     : Actions.Object'Class;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access);
   --  If Act has been deactivated but its signature is invalid, report the
   --  list of activated successor actions that will not be executed as a
   --  consequence.

   Traces : constant GNATCOLL.Traces.Logger :=
     GNATCOLL.Traces.Create ("GPR.ACTIONS_SCHEDULER", GNATCOLL.Traces.Off);

   --------------
   -- Listener --
   --------------

   --  A listener is a task dedicated to the listening on a file descriptor
   --  the task can be re-used for several file descriptors, following the
   --  following pattern:
   --    Listen (FD1), Fetch_Content (FD1); Listen (FD2);
   --    Fetch_Content (FD2), ...
   --
   --  The worker can be ended by calling Listen (GNATCOLL.OS.FS.Null_FD);

   task type Listener is
      entry Listen (FD : FS.File_Descriptor);
      --  Start listening a given file descriptor. If FD is Null_FD then
      --  stop the task.

      entry Fetch_Content (Content : out Unbounded_String);
      --  Fetch the captured content while listening on FD. Note that the
      --  method will block until end-of-file is reached.
   end Listener;

   -----------
   -- Clear --
   -----------

   procedure Clear (Ctxt : in out Context) is
   begin
      Ctxt.Actions.Clear;
      Ctxt.Graph.Clear;
      Ctxt.Nodes.Clear;
      Ctxt.Status := Success;
   end Clear;

   --------------------
   -- Collect_Action --
   --------------------

   function Collect_Action
     (Self    : in out Object;
      Action  : in out Actions.Object'Class;
      Handler : Collect_Handler;
      Context : access GPR2.Build.Actions_Scheduler.Context)
      return Collect_Status is
   begin
      --  This wrapper is required so the JSON scheduler can override
      --  Collect_Action while using internal fields.

      return Collect_Action (Self.Tree_Db, Action, Handler, Context);
   end Collect_Action;

   function Collect_Action
     (Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Action  : in out Actions.Object'Class;
      Handler : Collect_Handler;
      Context : access GPR2.Build.Actions_Scheduler.Context)
      return Collect_Status
   is

      function Internal return Collect_Status;
      --  Wrapper around real process used to simplify the code.
      --  If Internal returns Abort_Execution, a message is reported to
      --  the user.

      --------------
      -- Internal --
      --------------

      function Internal return Collect_Status is
      begin
         pragma
           Assert
             (Handler.Status not in Running | Pending,
              "The action '"
              & Action.UID.Image
              & "' is still running or pending. Cannot collect the job"
              & " before it finishes");

         if Length (Handler.Stdout) > 0 and then Action.Display_Output then
            Tree_Db.Reporter.Report
              (-Handler.Stdout, Level => GPR2.Message.Important);
         end if;

         if Length (Handler.Stderr) > 0 then
            Tree_Db.Reporter.Report
              (-Handler.Stderr,
               To_Stderr => True,
               Level     => GPR2.Message.Important);
         end if;

         case Handler.Status is
            when Exception_Raised            =>
               Traces.Trace
                 ("an exception was raised during the execution of "
                  & Action.UID.Image);
               return Abort_Execution;

            when Failed_To_Launch            =>
               Traces.Trace ("failed to launch " & Action.UID.Image);
               return Abort_Execution;

            when Failed_Cmd_Line_Computation =>
               Traces.Trace
                 ("command line computation failed for " & Action.UID.Image);
               return Abort_Execution;

            when Failed_Pre_Execution        =>
               Traces.Trace ("pre-execution failed for " & Action.UID.Image);
               return Abort_Execution;

            when Finished                    =>
               if Handler.Return_Code /= SUCCESS_RETURN_CODE then
                  Traces.Trace
                    (Action.UID.Image
                     & " returned status"
                     & Handler.Return_Code'Image);
                  return Abort_Execution;
               end if;

            when Skipped                     =>
               if Action in Actions.Process.Link.Object'Class then
                  declare
                     Link : constant Actions.Process.Link.Object'Class :=
                       Actions.Process.Link.Object'Class (Action);
                  begin
                     if not Link.Is_Library then
                        Tree_Db.Reporter.Report
                          ('"'
                           & String (Link.Output.Path.Simple_Name)
                           & """ up to date");
                     end if;
                  end;
               end if;

            when others                      =>
               null;
         end case;

         if Handler.Status in Skipped | Finished
           or else
             (Handler.Status = Deactivated and then Action.Valid_Signature)
         then

            if not Action.Post_Execution
                     ((if Handler.Status in Skipped | Deactivated
                       then Skipped
                       else Success),
                      Handler.Stdout,
                      Handler.Stderr)
            then
               Traces.Trace ("post-execution failed for " & Action.UID.Image);
               return Abort_Execution;
            end if;

            --  Propagate any newly created action
            if Handler.Status = Finished then
               if not Tree_Db.Propagate_Actions then
                  Traces.Trace
                    ("action propagation failed for " & Action.UID.Image);
                  return Abort_Execution;
               end if;

               if not Action.Write_Signature (Handler.Stdout, Handler.Stderr)
               then
                  Traces.Trace
                    ("signature writing failed for " & Action.UID.Image);
                  return Abort_Execution;
               end if;
            end if;

            --  Unlock depending actions. Note that deactivated action with an
            --  invalid signature will not be completed.

            Context.Graph.Complete_Visit (Context.Nodes (Action.UID));
         end if;

         return Continue_Execution;

      exception
         when E : others =>
            Tree_Db.Reporter.Report
              ("!!! Unexpected exception caught"
               & ASCII.LF
               & Ada.Exceptions.Exception_Information (E),
               To_Stderr => True,
               Level     => GPR2.Message.Important);
            return Abort_Execution;
      end Internal;

      Result : constant Collect_Status := Internal;
   begin
      if Result = Abort_Execution then
         declare
            Msg : constant String := Action.Failure_Message;
         begin
            if Msg'Length > 0 then
               Tree_Db.Reporter.Report
                 (Msg, To_Stderr => True, Level => GPR2.Message.Important);
            end if;
         end;
      end if;

      return Result;
   end Collect_Action;

   -------------
   -- Display --
   -------------

   procedure Display
     (Action : Action_Id'Class; Tree_Db : GPR2.Build.Tree_Db.Object_Access) is
   begin
      Tree_Db.Reporter.Report
        (Action.Image (With_View => False, Align_Class => True));
   end Display;

   --------------------------
   -- Display_Command_Line --
   --------------------------

   procedure Display_Command_Line
     (Action  : Actions.Process.Object'Class;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access)
   is
      Action_RF : constant GPR2.Build.Response_Files.Object :=
        Action.Response_File;
   begin
      --  ??? Both message level and Project tree verbosity
      --  don't cope with tooling messages that need quiet,
      --  normal or detailed info.
      --  Let's go for the default one *and* verbose one for now
      if Tree_Db.Reporter.User_Verbosity >= Verbose
        or else
          (Tree_Db.Reporter.User_Verbosity = Unset
           and then Tree_Db.Reporter.Verbosity >= Verbose)
      then
         Tree_Db.Reporter.Report (Image (Action.Command_Line.Argument_List));

         if Action_RF.Has_Secondary_Content then
            Tree_Db.Reporter.Report
              (RF_Message
                 (Action_RF.Secondary_Response_File,
                  Action_RF.Secondary_Response_File_Content));
         end if;

         if Action_RF.Has_Primary_Content then
            Tree_Db.Reporter.Report
              (RF_Message
                 (Action_RF.Primary_Response_File,
                  Action_RF.Primary_Response_File_Content));
         end if;
      else
         Display (Action.UID, Tree_Db);
      end if;
   end Display_Command_Line;

   --------------------------
   -- Effective_Job_Number --
   --------------------------

   function Effective_Job_Number (N : Natural) return Natural is
   begin
      if N > 0 then
         return N;
      else
         return Natural (System.Multiprocessors.Number_Of_CPUs);
      end if;
   end Effective_Job_Number;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Context : access GPR2.Build.Actions_Scheduler.Context;
      Options : GPR2.Build.Actions_Scheduler.Options'Class) is
   begin
      Internal_Execute (Self, Tree_Db, Context, Options);
   end Execute;

   -------------------------
   -- Execute_Next_Action --
   -------------------------

   function Execute_Next_Action
     (Tree_Db            : GPR2.Build.Tree_Db.Object_Access;
      Context            : access GPR2.Build.Actions_Scheduler.Context;
      Catch_Exceptions   : Boolean := True;
      Force_Execution    : Boolean := False;
      Keep_Temp_Files    : Boolean := False;
      No_Warnings_Replay : Boolean := False) return Action_Report
   is
      procedure Execute_Process
        (Status                : out Action_Status;
         Action                :
           in out GPR2.Build.Actions.Process.Object'Class;
         Stderr                : out Unbounded_String;
         Stdout                : out Unbounded_String;
         Execution_Return_Code : out Integer);

      procedure Execute_Thread
        (Status                : out Action_Status;
         Action                : in out GPR2.Build.Actions.Thread.Object'Class;
         Stderr                : out Unbounded_String;
         Stdout                : out Unbounded_String;
         Execution_Return_Code : out Integer);

      procedure Execute_Process
        (Status                : out Action_Status;
         Action                :
           in out GPR2.Build.Actions.Process.Object'Class;
         Stderr                : out Unbounded_String;
         Stdout                : out Unbounded_String;
         Execution_Return_Code : out Integer)
      is
         Proc_Handler : Process_Handler;
         P_Stdout     : FS.File_Descriptor;
         P_Stderr     : FS.File_Descriptor;
      begin
         Display_Command_Line (Action, Tree_Db);

         Launch_Process (Action, Proc_Handler, P_Stdout, P_Stderr);

         Status := Proc_Handler.Status;
         pragma Assert (Status in Running | Failed_To_Launch);

         if Status = Running then
            Execution_Return_Code := GOP.Wait (Proc_Handler.Handle);

            Stdout :=
              To_Unbounded_String
                (FS.Read (P_Stdout, Buffer_Size => 8 * 1024));
            Stderr :=
              To_Unbounded_String
                (FS.Read (P_Stderr, Buffer_Size => 8 * 1024));
            FS.Close (P_Stdout);
            FS.Close (P_Stderr);

            Status := Finished;
         elsif Status = Failed_To_Launch then
            Stderr := Proc_Handler.Error_Message;
         end if;
      end Execute_Process;

      procedure Execute_Thread
        (Status                : out Action_Status;
         Action                : in out GPR2.Build.Actions.Thread.Object'Class;
         Stderr                : out Unbounded_String;
         Stdout                : out Unbounded_String;
         Execution_Return_Code : out Integer) is
      begin
         Display (Action.UID, Tree_Db);

         if Catch_Exceptions then
            begin
               Execution_Return_Code := Action.Execute (Stdout, Stderr);
               Status := Finished;
            exception
               when E : others =>
                  Stderr :=
                    To_Unbounded_String
                      (Action.UID.Image
                       & " "
                       & Ada.Exceptions.Exception_Information (E));
                  Status := Exception_Raised;
            end;
         else
            Execution_Return_Code := Action.Execute (Stdout, Stderr);
            Status := Finished;
         end if;
      end Execute_Thread;

      Status            : Action_Status := Unknown;
      Dumb_Action_Slot  : constant Integer := 1;
      Handler           : Collect_Handler;
      Stderr            : Unbounded_String;
      Stdout            : Unbounded_String;
      Ret_Code          : Integer;
      Node              : GDG.Node_Id;
      Unused_Job_Status : Collect_Status;
   begin
      if not Context.Graph.Iterator_Started then
         Traces.Trace ("Starting the graph iterator");
         Context.Graph.Start_Iterator (Enable_Visiting_State => True);
      end if;

      if not Context.Graph.Next (Node) or else Node = GDG.No_Node then
         --  Either no more nodes in the graph to visit, or previous nodes
         --  were visited but not completed because of failing executions.
         return (Status => No_Action_To_Execute, others => <>);
      end if;

      declare
         UID            : constant Action_Id'Class := Context.Actions (Node);
         Act            : Actions.Object'Class := Tree_Db.Action (UID);
         Is_Proc_Action : constant Boolean :=
           Act in Actions.Process.Object'Class;
      begin
         Status :=
           Pre_Run_Status
             (Tree_Db.Action_Id_To_Reference (UID),
              Dumb_Action_Slot,
              Force_Execution);

         if Status = Ready_To_Run then
            declare
               Act_Ref : constant Build.Tree_Db.Action_Reference_Type :=
                 Tree_Db.Action_Id_To_Reference (UID);
            begin
               if not Act_Ref.Pre_Execution then
                  Status := Failed_Pre_Execution;
               else
                  if Is_Proc_Action then
                     Execute_Process
                       (Status                => Status,
                        Action                =>
                          Process.Object'Class (Act_Ref.Element.all),
                        Stderr                => Stderr,
                        Stdout                => Stdout,
                        Execution_Return_Code => Ret_Code);
                  else
                     Execute_Thread
                       (Status                => Status,
                        Action                =>
                          Thread.Object'Class (Act_Ref.Element.all),
                        Stderr                => Stderr,
                        Stdout                => Stdout,
                        Execution_Return_Code => Ret_Code);
                  end if;
               end if;
            end;

         elsif Status = Deactivated
           and then not Tree_Db.Action_Id_To_Reference (UID).Valid_Signature
         then
            Report_Deactivated_Invalid_Signature (Act, Tree_Db);
         end if;

         Handler :=
           (case Status is
              when Skipped                     =>
                (Status     => Skipped,
                 Stdout     => Act.Saved_Stdout,
                 Stderr     =>
                   (if No_Warnings_Replay
                    then Null_Unbounded_String
                    else Act.Saved_Stderr),
                 UID_Holder => Action_Id_Holder.To_Holder (UID),
                 others     => <>),
              when Deactivated                 =>
                (Status     => Deactivated,
                 Stdout     => Null_Unbounded_String,
                 Stderr     => Null_Unbounded_String,
                 UID_Holder => Action_Id_Holder.To_Holder (UID),
                 others     => <>),
              when Failed_Cmd_Line_Computation =>
                (Status     => Failed_Cmd_Line_Computation,
                 Stdout     => Null_Unbounded_String,
                 Stderr     => Null_Unbounded_String,
                 UID_Holder => Action_Id_Holder.To_Holder (UID),
                 others     => <>),
              when Failed_To_Launch            =>
                (Status     => Failed_To_Launch,
                 Stdout     => Stdout,
                 Stderr     => Stderr,
                 UID_Holder => Action_Id_Holder.To_Holder (UID),
                 others     => <>),
              when Exception_Raised            =>
                (Status     => Exception_Raised,
                 Stdout     => Stdout,
                 Stderr     => Stderr,
                 UID_Holder => Action_Id_Holder.To_Holder (UID),
                 others     => <>),
              when Finished                    =>
                (Status      => Finished,
                 Stdout      => Stdout,
                 Stderr      => Stderr,
                 UID_Holder  => Action_Id_Holder.To_Holder (UID),
                 Return_Code => Ret_Code,
                 others      => <>),
              when others                      =>
                (Status     => Unknown,
                 Stdout     => Null_Unbounded_String,
                 Stderr     => Null_Unbounded_String,
                 UID_Holder => Action_Id_Holder.To_Holder (UID),
                 others     => <>));

         --  Cleanup the temporary files that are local to the job
         if not Keep_Temp_Files then
            Act.Cleanup_Temp_Files (Scope => Actions.Local);
         end if;

         Unused_Job_Status := Collect_Action (Tree_Db, Act, Handler, Context);

         Tree_Db.Action_Id_To_Reference (UID) := Act;

         --  Cleanup the temporary files with global scope
         if not Keep_Temp_Files then
            Tree_Db.Clear_Temp_Files;
         end if;

         return
           (case Handler.Status is
              when Skipped                     =>
                (Status     => Skipped,
                 Stdout     => Act.Saved_Stdout,
                 Stderr     =>
                   (if No_Warnings_Replay
                    then Null_Unbounded_String
                    else Act.Saved_Stderr),
                 UID_Holder => Handler.UID_Holder),
              when Deactivated                 =>
                (Status     => Deactivated,
                 UID_Holder => Handler.UID_Holder,
                 others     => <>),
              when Failed_Cmd_Line_Computation =>
                (Status     => Failed_Cmd_Line_Computation,
                 UID_Holder => Handler.UID_Holder,
                 others     => <>),
              when Failed_To_Launch            =>
                (Status     => Failed_To_Launch,
                 Stdout     => Handler.Stdout,
                 Stderr     => Handler.Stderr,
                 UID_Holder => Handler.UID_Holder),
              when Exception_Raised            =>
                (Status     => Exception_Raised,
                 Stdout     => Handler.Stdout,
                 Stderr     => Handler.Stderr,
                 UID_Holder => Handler.UID_Holder),
              when Finished                    =>
                (Status      => Finished,
                 Stdout      => Handler.Stdout,
                 Stderr      => Handler.Stderr,
                 UID_Holder  => Handler.UID_Holder,
                 Return_Code => Handler.Return_Code),
              when others                      =>
                raise Actions_Scheduler_Error
                  with "invalid execution status for action" & UID.Image);
      end;
   end Execute_Next_Action;

   -----------
   -- Image --
   -----------

   function Image
     (Command : Argument_List; For_Script : Boolean := False) return String
   is
      Result : Unbounded_String;
      Quote  : constant Character := (if For_Script then ''' else '"');
   begin
      for Arg of Command loop
         if Length (Result) > 0 then
            Append (Result, " ");
         end if;

         if Ada.Strings.Fixed.Index (Arg, " ") > 0
           or else (For_Script and then Ada.Strings.Fixed.Index (Arg, "\") > 0)
         then
            Append (Result, Quote);
            Append (Result, Arg);
            Append (Result, Quote);
         else
            Append (Result, Arg);
         end if;
      end loop;

      return -Result;
   end Image;

   ----------------------
   -- Internal_Execute --
   ----------------------

   procedure Internal_Execute
     (Self    : in out Object;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access;
      Context : access GPR2.Build.Actions_Scheduler.Context;
      Options : GPR2.Build.Actions_Scheduler.Options'Class)
   is
      use FS;
      use type Ada.Containers.Count_Type;

      Max_Jobs : constant Natural := Effective_Job_Number (Options.Jobs);
      --  Effective max number of simultaneous jobs

      New_Action_To_Collect : Suspension_Object;
      --  Suspension object used to wake up the main loop when a new action is
      --  added to the collection queue.

      Script_FD  : GNATCOLL.OS.FS.File_Descriptor := FS.Null_FD;
      Script_Dir : Path_Name.Object;

      Nb_Executed        : Natural := 0;
      End_Of_Iteration   : Boolean := False;
      End_Due_To_Failure : Boolean := False;
      --  Set when End_Of_Iteration was triggered by a failure (exception,
      --  Stop_On_Fail), as opposed to simply running out of ready nodes.
      --  Only in the latter case may new nodes appear via Post_Execution.
      Active_Actions     : Natural := 0;

      task type Thread_Runner is
         entry Execute (UID : Action_Id'Class; Action_Slot : Natural);
         entry Stop;
      end Thread_Runner;
      --  Task dedicated to executing a thread action and adding it to the
      --  queue of actions to collect.

      type Thread_Runner_Access is access all Thread_Runner;
      Thread_Runners : array (1 .. Max_Jobs) of Thread_Runner_Access;

      task type Process_Runner is
         entry Execute (UID : Action_Id'Class; Action_Slot : Natural);
         entry Stop;
      end Process_Runner;
      --  Task dedicated to executing a process, waiting for its completion,
      --  and adding it to the queue of actions to collect.

      type Process_Runner_Access is access all Process_Runner;
      Process_Runners : array (1 .. Max_Jobs) of Process_Runner_Access;

      use type Actions.Process.Object;
      package Process_Action_Holders is new
        Ada.Containers.Indefinite_Holders (Actions.Process.Object'Class);

      use type Actions.Thread.Object;
      package Thread_Action_Holders is new
        Ada.Containers.Indefinite_Holders (Actions.Thread.Object'Class);

      type Slot_Entry is record
         Is_Free : Boolean := True;
      end record;

      type Serialized_Slot_Array is array (1 .. Max_Jobs) of Slot_Entry;

      Serialized_Slot : Serialized_Slot_Array := (others => (Is_Free => True));
      --  Unique slot identifier array to ensure that each action has
      --  exclusive access to a slot. A slot has several purposes:
      --  - Each slot has one dedicated thread runner as well as one process
      --    runner.
      --  - Each slot ID can be used by actions. For instance, it is used
      --    to compute the naming of temporary mapping files for compile
      --    actions.
      --  Note: this variable does not need shared protection as all accesses
      --  are serialized through the main task.

      function Nb_Active_Actions return Integer
      is (Active_Actions);
      --  Return the number of actions being executed

      function Available_Slot return Integer;
      --  Get an available slot if any without reserving it

      procedure Reserve_Slot (Action_Slot : Integer);
      --  Reserve the specified slot and increase the number of active
      --  actions.

      procedure Release_Slot (Action_Slot : Integer);
      --  Release the specified slot and decrease the number of active
      --  actions if the specified slot was busy.

      procedure Execute_Thread_Runner
        (Slot_Id : Integer; UID : Action_Id'Class);
      --  Launch Action using the thread runner allocated
      --  for the given Slot_Id. Creates the runner if it
      --  has not been allocated yet.

      procedure Execute_Process_Runner
        (Slot_Id : Integer; UID : Action_Id'Class);
      --  Launch Action using the process runner allocated
      --  for the given Slot_Id. Creates the runner if it
      --  has not been allocated yet.

      procedure Stop_And_Free_Runners;
      --  Stop all runners and free their memory

      procedure Initialize_Script_FD (Script_FD : in out File_Descriptor);
      --  Initialize the script file descriptor of the script file specified in
      --  the options.

      procedure Write_Script
        (Act        : Actions.Process.Object'Class;
         Script_FD  : FS.File_Descriptor;
         Script_Dir : in out Path_Name.Object);
      --  Write the command required to execute the specified process action
      --  in the provided file. Script_Dir is used to track the current working
      --  directory in the script to avoid unnecessary "cd" commands.

      procedure Report_Progress
        (Self : in out Object'Class; Total_Number_Of_Nodes : Natural);
      --  Report progress of the execution based on the number of executed
      --  actions and the total number of actions to execute.

      package Collect_Queue_Interfaces is new
        Ada.Containers.Synchronized_Queue_Interfaces (Collect_Handler);

      package Collect_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues
          (Collect_Queue_Interfaces);

      Collect_Queue : Collect_Queues.Queue;

      procedure Enqueue (Handler : Collect_Handler);
      --  Enqueue the handler and wake up the main loop

      --------------------
      -- Available_Slot --
      --------------------

      function Available_Slot return Integer is
      begin
         for J in Serialized_Slot'Range loop
            if Serialized_Slot (J).Is_Free then
               return J;
            end if;
         end loop;

         return -1;
      end Available_Slot;

      -------------
      -- Enqueue --
      -------------

      procedure Enqueue (Handler : Collect_Handler) is
      begin
         Collect_Queue.Enqueue (Handler);
         Set_True (New_Action_To_Collect);
      end Enqueue;

      ----------------------------
      -- Execute_Process_Runner --
      ----------------------------

      procedure Execute_Process_Runner
        (Slot_Id : Integer; UID : Action_Id'Class) is
      begin
         if Process_Runners (Slot_Id) = null then
            Process_Runners (Slot_Id) := new Process_Runner;
         end if;

         Process_Runners (Slot_Id).Execute (UID, Slot_Id);
      end Execute_Process_Runner;

      ---------------------------
      -- Execute_Thread_Runner --
      ---------------------------

      procedure Execute_Thread_Runner
        (Slot_Id : Integer; UID : Action_Id'Class) is
      begin
         if Thread_Runners (Slot_Id) = null then
            Thread_Runners (Slot_Id) := new Thread_Runner;
         end if;

         Thread_Runners (Slot_Id).Execute (UID, Slot_Id);
      end Execute_Thread_Runner;

      --------------------------
      -- Initialize_Script_FD --
      --------------------------

      procedure Initialize_Script_FD (Script_FD : in out FS.File_Descriptor) is
      begin
         Script_FD :=
           GNATCOLL.OS.FS.Open
             (Options.Script_File.String_Value, GNATCOLL.OS.FS.Write_Mode);

         if Script_FD = Invalid_FD then
            Tree_Db.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "could not create the script file '"
                  & Options.Script_File.String_Value
                  & '"',
                  Source_Reference.Create (Options.Script_File.Value, 0, 0)));
            Script_FD := Null_FD;
         end if;
      end Initialize_Script_FD;

      ------------------
      -- Release_Slot --
      ------------------

      procedure Release_Slot (Action_Slot : Integer) is
      begin
         if Action_Slot in Serialized_Slot'Range then
            if not Serialized_Slot (Action_Slot).Is_Free then
               Active_Actions := Active_Actions - 1;
               Serialized_Slot (Action_Slot) := (Is_Free => True);
            end if;
         end if;
      end Release_Slot;

      --------------------
      -- Report_Process --
      --------------------

      procedure Report_Progress
        (Self : in out Object'Class; Total_Number_Of_Nodes : Natural) is
      begin
         if Nb_Executed /= Self.Previous_Progress then
            Self.Previous_Progress := Nb_Executed;

            declare
               Percent : constant String :=
                 Natural'Image ((Nb_Executed * 100) / Total_Number_Of_Nodes);
            begin
               Self.Tree_Db.Reporter.Report
                 ("completed"
                  & Nb_Executed'Image
                  & " out of"
                  & Total_Number_Of_Nodes'Image
                  & " ("
                  & Percent (Percent'First + 1 .. Percent'Last)
                  & "%)...",
                  Level => GPR2.Message.Important);
            end;
         end if;
      end Report_Progress;

      ------------------
      -- Reserve_Slot --
      ------------------

      procedure Reserve_Slot (Action_Slot : Integer) is
      begin
         pragma Assert (Serialized_Slot (Action_Slot).Is_Free);
         Active_Actions := Active_Actions + 1;
         Serialized_Slot (Action_Slot) := (Is_Free => False);
      end Reserve_Slot;

      ----------------------------
      -- Stop_And_Free_Runners --
      ---------------------------

      procedure Stop_And_Free_Runners is
         procedure Free is new
           Ada.Unchecked_Deallocation (Process_Runner, Process_Runner_Access);
         procedure Free is new
           Ada.Unchecked_Deallocation (Thread_Runner, Thread_Runner_Access);
      begin
         for Runner of Thread_Runners loop
            if Runner /= null then
               Runner.Stop;
            end if;
         end loop;

         for Runner of Process_Runners loop
            if Runner /= null then
               Runner.Stop;
            end if;
         end loop;

         --  Wait for tasks to actually terminate before freeing their memory:
         --  Runner.Stop returns as soon as the rendezvous completes, but the
         --  task body continues running (exiting the loop, finalizing local
         --  listener tasks) until it reaches its end.  Freeing the task object
         --  before that point corrupts the still-executing task body.

         for Runner of Thread_Runners loop
            if Runner /= null then
               while not Runner.all'Terminated loop
                  delay 0.001;
               end loop;
               Free (Runner);
            end if;
         end loop;

         for Runner of Process_Runners loop
            if Runner /= null then
               while not Runner.all'Terminated loop
                  delay 0.001;
               end loop;
               Free (Runner);
            end if;
         end loop;
      end Stop_And_Free_Runners;

      --------------------
      -- Process_Runner --
      --------------------

      task body Process_Runner is
         UID_Holder                       : Actions.Action_Id_Holder.Holder;
         Act_Holder                       : Process_Action_Holders.Holder;
         Stdout, Stderr                   : Unbounded_String;
         Exit_Code                        : Integer;
         Proc_Handler                     : Process_Handler;
         P_Stdout                         : FS.File_Descriptor;
         P_Stderr                         : FS.File_Descriptor;
         Stdout_Listener, Stderr_Listener : Listener;
         Received_Stop_Signal             : Boolean := False;
         Status                           : Action_Status := Unknown;
         Act_Slot                         : Natural;

      begin
         Main_Loop : loop
            select
               accept Stop do
                  Received_Stop_Signal := True;

                  Stdout_Listener.Listen (Null_FD);
                  Stderr_Listener.Listen (Null_FD);
               end Stop;
            or
               accept Execute (UID : Action_Id'Class; Action_Slot : Natural) do
                  Reserve_Slot (Action_Slot);
                  UID_Holder.Replace_Element (UID);
                  Act_Slot := Action_Slot;

                  --  Read the action inside the rendezvous: this runs on
                  --  the caller (main task), serializing access to the
                  --  Actions map and avoiding race conditions. Note that
                  --  the rendezvous must only be performed from a single
                  --  thread (in our case, the main thread) in order to
                  --  avoid race conditions.

                  Act_Holder.Replace_Element
                    (Process.Object'Class (Self.Tree_Db.Action (UID)));

                  Display_Command_Line (Act_Holder.Element, Self.Tree_Db);

               end Execute;
            end select;

            exit Main_Loop when Received_Stop_Signal;

            declare
               Act : Process.Object'Class := Act_Holder.Element;
            begin
               Launch_Process (Act, Proc_Handler, P_Stdout, P_Stderr);

               Status := Proc_Handler.Status;
               pragma Assert (Status in Running | Failed_To_Launch);

               if Status = Running then
                  Stdout_Listener.Listen (P_Stdout);
                  Stderr_Listener.Listen (P_Stderr);

                  Exit_Code := GOP.Wait (Proc_Handler.Handle);

                  --  Fetch captured stdout and stderr if necessary
                  Stdout_Listener.Fetch_Content (Stdout);
                  Stderr_Listener.Fetch_Content (Stderr);

                  Enqueue
                    ((Status      => Finished,
                      Return_Code => Exit_Code,
                      Stdout      => Stdout,
                      Stderr      => Stderr,
                      UID_Holder  => UID_Holder,
                      Action_Slot => Act_Slot));
               elsif Status = Failed_To_Launch then
                  Enqueue
                    ((Status      => Failed_To_Launch,
                      Stderr      => Proc_Handler.Error_Message,
                      UID_Holder  => UID_Holder,
                      Action_Slot => Act_Slot,
                      others      => <>));
               end if;
            end;
         end loop Main_Loop;

         --  Wait for listener tasks to fully terminate before this task
         --  completes, to avoid a deadlock on the GNAT global task lock during
         --  task termination.

         while not Stdout_Listener'Terminated loop
            delay 0.001;
         end loop;

         while not Stderr_Listener'Terminated loop
            delay 0.001;
         end loop;

      exception
         when E : others =>
            --  The runner died, we supposedly cannot access the Tree or
            --  Reporter, attempt to trace errors in order to investigate
            --  potential issues.
            --  Note that the potential behavior or such an error is the
            --  action scheduler indefinitely hanging (stuck waiting for this
            --  runner to properly terminate itself) or finalize issues of
            --  the underlying tools.
            Traces.Trace ("!!! Process_Runner error");
            Traces.Trace (Ada.Exceptions.Exception_Information (E));
      end Process_Runner;

      -------------------
      -- Thread_Runner --
      -------------------

      task body Thread_Runner is
         UID_Holder            : Actions.Action_Id_Holder.Holder;
         Act_Holder            : Thread_Action_Holders.Holder;
         Act_Slot              : Natural;
         Execution_Return_Code : Integer;
         Handler               : Collect_Handler;
         Received_Stop_Signal  : Boolean := False;
      begin
         Main_Loop : loop
            select
               accept Stop do
                  Received_Stop_Signal := True;
               end Stop;
            or
               accept Execute (UID : Action_Id'Class; Action_Slot : Natural) do
                  UID_Holder.Replace_Element (UID);
                  Act_Slot := Action_Slot;
                  Reserve_Slot (Action_Slot);

                  Display (UID, Tree_Db);

                  --  Read the action inside the rendezvous: this runs on
                  --  the caller (main task), serializing access to the
                  --  Actions map and avoiding race conditions. Note that
                  --  the rendezvous must only be performed from a single
                  --  thread (in our case, the main thread) in order to
                  --  avoid race conditions.

                  Act_Holder.Replace_Element
                    (Actions.Thread.Object'Class (Tree_Db.Action (UID)));
               end Execute;
            end select;

            exit Main_Loop when Received_Stop_Signal;

            declare
               Stdout, Stderr : Unbounded_String;
               Action         : Actions.Thread.Object'Class :=
                 Act_Holder.Element;
            begin
               Execution_Return_Code := Action.Execute (Stdout, Stderr);
               Handler :=
                 (Status      => Finished,
                  Return_Code => Execution_Return_Code,
                  Stdout      => Stdout,
                  Stderr      => Stderr,
                  UID_Holder  => UID_Holder,
                  Action_Slot => Act_Slot);
            exception
               when E : others =>
                  Stderr :=
                    To_Unbounded_String
                      (Action.UID.Image
                       & " "
                       & Ada.Exceptions.Exception_Information (E));
                  Handler :=
                    (Status      => Exception_Raised,
                     Stdout      => Stdout,
                     Stderr      => Stderr,
                     UID_Holder  => UID_Holder,
                     Action_Slot => Act_Slot);
            end;

            Enqueue (Handler);
         end loop Main_Loop;
      exception
         when E : others =>
            --  The runner died, attempt to trace errors in order to
            --  investigate potential issues.
            --  Note that the potential behavior or such an error is the
            --  action scheduler indefinitely hanging (stuck waiting for this
            --  runner to properly terminate itself) or finalize issues of
            --  the underlying tools.
            Traces.Trace ("!!! Thread_Runner error");
            Traces.Trace (Ada.Exceptions.Exception_Information (E));
      end Thread_Runner;

      ------------------
      -- Write_Script --
      ------------------

      procedure Write_Script
        (Act        : Actions.Process.Object'Class;
         Script_FD  : FS.File_Descriptor;
         Script_Dir : in out Path_Name.Object)
      is
         use Path_Name;

         Cd_Args : GNATCOLL.OS.Process.Argument_List;
      begin
         --  Update the working directory
         if not Script_Dir.Is_Defined
           or else Script_Dir /= Act.Working_Directory
         then
            Cd_Args.Append ("cd");
            Cd_Args.Append (Act.Working_Directory.String_Value);
            GNATCOLL.OS.FS.Write (Script_FD, Image (Cd_Args, True) & ASCII.LF);
            Script_Dir := Act.Working_Directory;
         end if;

         --  Setup env variables for the run:

         for C in Act.Command_Line.Environment_Variables.Iterate loop
            GNATCOLL.OS.FS.Write
              (Script_FD,
               Env_Dicts.Key (C) & "=""" & Env_Dicts.Element (C) & """ ");
         end loop;

         --  and execute the action:

         GNATCOLL.OS.FS.Write
           (Script_FD,
            Image (Act.Command_Line.Argument_List, True) & ASCII.LF);
      end Write_Script;

      Node        : GDG.Node_Id;
      Action_Slot : Natural;
      --  Identifier of the slot assigned to the currently visited action.
      --  Used for both process and thread actions to identify the runner
      --  assigned to the action and can be used to compute the name of
      --  temporary files.

      Status : Action_Status := Unknown;

   begin
      Self.Tree_Db := Tree_Db;

      if Options.Script_File.Is_Defined then
         Initialize_Script_FD (Script_FD);
      end if;

      Self.Make_JS.Initialize_Protocol;

      if Self.Make_JS.Dry_Run then
         return;
      end if;

      if Options.Force_Jobserver and then Self.Make_JS.Has_Protocol_Error then
         Tree_Db.Reporter.Report
           ("error: jobserver fifo protocol not supported, please use "
            & "--jobserver-style=pipe on your make command");
         return;
      end if;

      Context.Graph.Start_Iterator (Enable_Visiting_State => True);

      loop
         --  Launch as many process as possible
         Next_Node_Loop : while Nb_Active_Actions < Max_Jobs
           and then not End_Of_Iteration
         loop
            begin
               if Status /= Pending and then not Context.Graph.Next (Node)
               then
                  Traces.Trace ("End of iteration: status /= pending " &
                                "and no more nodes available");
                  End_Of_Iteration := True;
                  exit Next_Node_Loop;
               end if;
            exception
               when E : GNATCOLL.Directed_Graph.DG_Error =>
                  pragma Annotate (Xcov, Exempt_On, "defensive code");
                  Tree_Db.Reporter.Report
                    ("error: internal error in the actions scheduler ("
                     & Ada.Exceptions.Exception_Message (E)
                     & ")");
                  Traces.Trace ("!!! Internal error in the DAG");
                  Traces.Trace (Ada.Exceptions.Exception_Information (E));
                  End_Of_Iteration   := True;
                  End_Due_To_Failure := True;
                  exit Next_Node_Loop;
                  pragma Annotate (Xcov, Exempt_Off);
            end;

            if Node = GDG.No_Node then

               --  two possibilities:
               --  * The currently visited node has not finished being
               --    processed. In that case, Nb_Active_Actions will be
               --    superior to 0.
               --  * The visited node has finished, but will never complete to
               --    prevent successors execution. It can be the case for
               --    invalid deactivated actions.
               --  In all cases, we need to exit the graph iteration

               if Nb_Active_Actions = 0 then
                  End_Of_Iteration := True;
               end if;

               exit Next_Node_Loop;
            end if;

            declare
               UID            : constant Action_Id'Class :=
                 Context.Actions (Node);
               Is_Proc_Action : constant Boolean :=
                 Tree_Db.Action (UID) in Actions.Process.Object'Class;
            begin
               Action_Slot := Available_Slot;
               pragma Assert (Action_Slot /= -1);

               --  An action can be in pending status if it has been
               --  processed in a previous iteration but could not be launched
               --  because of missing jobserver token. In this case, we do not
               --  want to re-evaluate the action status.

               if Status = Unknown then
                  Status :=
                    Pre_Run_Status
                      (Tree_Db.Action_Id_To_Reference (UID),
                       Action_Slot,
                       Options.Force);
               end if;

               --  If an action needs to be run, we first check if
               --  one jobserver token is available.

               if Status in Pending | Ready_To_Run then
                  --  We are waiting for a token to become available
                  if Self.Make_JS.Is_Available
                    and then not Self.Make_JS.Request_Token
                  then
                     --  No token available, we need to wait for one
                     --  before launching the job
                     Status := Pending;
                     exit Next_Node_Loop;
                  else
                     Status := Ready_To_Run;
                  end if;
               end if;

               if Status = Ready_To_Run then
                  declare
                     Act_Ref : constant Build.Tree_Db.Action_Reference_Type :=
                       Tree_Db.Action_Id_To_Reference (UID);
                  begin
                     if Act_Ref.Pre_Execution then
                        if Is_Proc_Action then
                           Execute_Process_Runner (Action_Slot, UID);
                        else
                           Execute_Thread_Runner (Action_Slot, UID);
                        end if;

                        Status := Running;

                        if Options.Show_Progress then
                           --  Update progress report if requested
                           Report_Progress
                             (Self, Natural (Context.Nodes.Length));
                        end if;
                     else
                        if Self.Make_JS.Is_Available then
                           Self.Make_JS.Release_Token;
                        end if;

                        Status := Failed_Pre_Execution;
                     end if;
                  end;

               elsif Status = Deactivated
                 and then
                   not Tree_Db.Action_Id_To_Reference (UID).Valid_Signature
               then
                  Report_Deactivated_Invalid_Signature
                    (Tree_Db.Action (UID), Tree_Db);
               end if;
            exception
               when E : Actions_Scheduler_Error =>
                  pragma Annotate (Xcov, Exempt_On, "defensive code");
                  End_Of_Iteration   := True;
                  End_Due_To_Failure := True;
                  Tree_Db.Reporter.Report
                    ("Fatal error: " & Ada.Exceptions.Exception_Message (E),
                     To_Stderr => True);
               when E : others =>
                  End_Of_Iteration   := True;
                  End_Due_To_Failure := True;
                  Tree_Db.Reporter.Report
                    ("Unexpected exception:", To_Stderr => True);
                  Tree_Db.Reporter.Report
                    (Ada.Exceptions.Exception_Information (E),
                     To_Stderr => True);
                  pragma Annotate (Xcov, Exempt_Off);
            end;

            if Status
               in Skipped
                | Deactivated
                | Failed_Cmd_Line_Computation
                | Failed_Pre_Execution
            then
               --  We need to call collect the job. However we were not able
               --  to do it in the above context since a reference was held on
               --  the action, so any modification to the DAG may have raised
               --  a tampering error.

               declare
                  UID        : constant Actions.Action_Id'Class :=
                    Context.Actions (Node);
                  Act        : Actions.Object'Class :=
                    Self.Tree_Db.Action (UID);
                  Handler    : Collect_Handler;
                  Job_Status : Collect_Status;
               begin
                  Handler :=
                    (case Status is
                       when Skipped                     =>
                         (Status     => Skipped,
                          Stdout     => Act.Saved_Stdout,
                          Stderr     =>
                            (if Options.No_Warnings_Replay
                             then Null_Unbounded_String
                             else Act.Saved_Stderr),
                          UID_Holder => Action_Id_Holder.To_Holder (UID),
                          others     => <>),
                       when Deactivated                 =>
                         (Status     => Deactivated,
                          Stdout     => Null_Unbounded_String,
                          Stderr     => Null_Unbounded_String,
                          UID_Holder => Action_Id_Holder.To_Holder (UID),
                          others     => <>),
                       when Failed_Cmd_Line_Computation =>
                         (Status     => Failed_Cmd_Line_Computation,
                          Stdout     => Null_Unbounded_String,
                          Stderr     => Null_Unbounded_String,
                          UID_Holder => Action_Id_Holder.To_Holder (UID),
                          others     => <>),
                       when others                      =>
                         (Status     => Failed_Pre_Execution,
                          Stdout     => Null_Unbounded_String,
                          Stderr     => Null_Unbounded_String,
                          UID_Holder => Action_Id_Holder.To_Holder (UID),
                          others     => <>));

                  Nb_Executed := Nb_Executed + 1;

                  --  Cleanup the temporary files that are local to the job
                  if not Options.Keep_Temp_Files then
                     Act.Cleanup_Temp_Files (Scope => Actions.Local);
                  end if;

                  Job_Status :=
                    Object'Class (Self).Collect_Action (Act, Handler, Context);

                  Self.Tree_Db.Action_Id_To_Reference (UID) := Act;

                  if Job_Status = Abort_Execution then

                     --  Failing to compute the command line is considered as
                     --  an execution failure.

                     if Status = Failed_Cmd_Line_Computation then
                        Context.Status := Failed;
                     elsif Context.Status /= Failed then
                        Context.Status := Errors;
                     end if;

                     if Options.Stop_On_Fail then
                        End_Of_Iteration   := True;
                        End_Due_To_Failure := True;
                        exit Next_Node_Loop;
                     end if;
                  end if;
               end;
            end if;

            --  Reset status for the next node
            Status := Unknown;
         end loop Next_Node_Loop;

         --  If End_Of_Iteration is True, wait for active actions to finish
         --  before exiting.

         exit when End_Of_Iteration and then Nb_Active_Actions = 0;

         --  The maximum number of runnable jobs have been launched, so now
         --  wait for them to finish.

         Suspend_Until_True (New_Action_To_Collect);

         while Collect_Queue.Current_Use > 0 loop
            declare
               Handler    : Collect_Handler;
               Job_Status : Collect_Status;
            begin
               Collect_Queue.Dequeue (Handler);

               declare
                  Act : Actions.Object'Class :=
                    Self.Tree_Db.Action (Handler.UID_Holder.Element);
               begin

                  --  Write the command to the script file if needed.
                  --  This is done here (in the main task) rather than in the
                  --  process runner to avoid concurrent access to Script_FD.

                  if Script_FD /= Null_FD
                    and then Handler.Status = Finished
                    and then Act in Actions.Process.Object'Class
                  then
                     Write_Script
                       (Actions.Process.Object'Class (Act),
                        Script_FD,
                        Script_Dir);
                  end if;

                  Job_Status :=
                    Object'Class (Self).Collect_Action (Act, Handler, Context);
                  Self.Tree_Db.Action_Id_To_Reference
                    (Handler.UID_Holder.Element) :=
                    Act;
               end;

               --  Cleanup the temporary files that are local to the job
               if not Options.Keep_Temp_Files then
                  Self.Tree_Db.Action_Id_To_Reference
                    (Handler.UID_Holder.Element)
                    .Cleanup_Temp_Files (Scope => Actions.Local);
               end if;

               Release_Slot (Action_Slot => Handler.Action_Slot);
               Nb_Executed := Nb_Executed + 1;

               if Self.Make_JS.Is_Available then
                  Self.Make_JS.Release_Token;
               end if;

               --  Report the progress if requested
               if Options.Show_Progress then
                  Report_Progress (Self, Natural (Context.Nodes.Length));
               end if;

               if Job_Status = Abort_Execution then
                  if Handler.Status
                     in Failed_To_Launch | Failed_Cmd_Line_Computation
                  then
                     Context.Status := Failed;
                  elsif Context.Status /= Failed then
                     Context.Status := Errors;
                  end if;

                  if Options.Stop_On_Fail then
                     End_Of_Iteration   := True;
                     End_Due_To_Failure := True;
                  end if;

               elsif not End_Due_To_Failure then
                  --  The action completed successfully and its Post_Execution
                  --  may have added new actions to the graph. Reset
                  --  End_Of_Iteration so the scheduler re-enters the graph
                  --  iterator to pick up any newly available nodes.
                  End_Of_Iteration := False;
               end if;
            end;
         end loop;
      end loop;

      --  Close the script file if needed
      if Script_FD /= Null_FD then
         GNATCOLL.OS.FS.Close (Script_FD);
      end if;

      --  Cleanup the temporary files with global scope
      if not Options.Keep_Temp_Files then
         Tree_Db.Clear_Temp_Files;
      end if;

      Stop_And_Free_Runners;
   end Internal_Execute;

   --------------------
   -- Launch_Process --
   --------------------

   procedure Launch_Process
     (Job            : in out Actions.Process.Object'Class;
      Proc_Handler   : in out Process_Handler;
      Capture_Stdout : out GNATCOLL.OS.FS.File_Descriptor;
      Capture_Stderr : out GNATCOLL.OS.FS.File_Descriptor)
   is
      package FS renames GNATCOLL.OS.FS;

      P_Wo : FS.File_Descriptor;
      P_Ro : FS.File_Descriptor;
      P_We : FS.File_Descriptor;
      P_Re : FS.File_Descriptor;
   begin
      FS.Open_Pipe (P_Ro, P_Wo);
      FS.Open_Pipe (P_Re, P_We);

      begin
         Proc_Handler :=
           (Status => Running,
            Handle =>
              Start
                (Args        => Job.Command_Line.Argument_List,
                 Env         => Job.Command_Line.Environment_Variables,
                 Cwd         => Job.Working_Directory.String_Value,
                 Stdout      => P_Wo,
                 Stderr      => P_We,
                 Inherit_Env => True));

      exception
         when Ex : GNATCOLL.OS.OS_Error =>
            FS.Close (P_Ro);
            FS.Close (P_Re);

            Proc_Handler :=
              (Status        => Failed_To_Launch,
               Error_Message =>
                 To_Unbounded_String
                   ("Command '"
                    & Image (Job.Command_Line.Argument_List)
                    & "' failed: "
                    & Ada.Exceptions.Exception_Message (Ex)));
            return;
      end;

      FS.Close (P_Wo);
      FS.Close (P_We);

      Capture_Stdout := P_Ro;
      Capture_Stderr := P_Re;
   end Launch_Process;

   --------------
   -- Listener --
   --------------

   task body Listener is
      use FS;
      To_Trim   : constant Ada.Strings.Maps.Character_Set :=
        Ada.Strings.Maps.To_Set (" " & ASCII.CR & ASCII.LF);
      Listen_FD : File_Descriptor;
      Result    : Unbounded_String;
   begin
      loop
         --  Fetch FD to listen too
         accept Listen (FD : FS.File_Descriptor) do
            Listen_FD := FD;
         end Listen;

         --  If null FD then exit
         exit when Listen_FD = Null_FD;

         if Listen_FD /= FS.Invalid_FD then
            --  If a valid FD then fetch the full content
            Result := FS.Read (Listen_FD, Buffer_Size => 8 * 1024);

            --  Close the file descriptor
            FS.Close (Listen_FD);

            --  And then make the result available
            accept Fetch_Content (Content : out Unbounded_String) do
               Content :=
                 Trim
                   (Result,
                    Left  => Ada.Strings.Maps.Null_Set,
                    Right => To_Trim);
            end Fetch_Content;

         else
            --  Handle nicely if FD is an invalid process
            accept Fetch_Content (Content : out Unbounded_String) do
               Content := To_Unbounded_String ("");
            end Fetch_Content;
         end if;

      end loop;
   exception
      when E : others =>
         --  The listener died, attempt to trace errors in order to investigate
         --  potential issues.
         --  Note that the potential behavior or such an error is the
         --  action scheduler indefinitely hanging (stuck waiting for this
         --  runner to properly terminate itself) or finalize issues of
         --  the underlying tools.
         Traces.Trace ("!!! Listener error");
         Traces.Trace (Ada.Exceptions.Exception_Information (E));
   end Listener;

   --------------------
   -- Pre_Run_Status --
   --------------------

   function Pre_Run_Status
     (Action  : in out Actions.Object'Class;
      Slot_Id : Positive;
      Force   : Boolean) return Pre_Run_Action_Status is
   begin
      if Action.View.Is_Externally_Built then
         if Traces.Is_Active then
            pragma Annotate (Xcov, Exempt_On, "debug code");
            Traces.Trace ("job externally built: " & Action.UID.Image);
            pragma Annotate (Xcov, Exempt_Off);
         end if;

         return Deactivated;
      end if;

      --  Load and check the job's signature

      Action.Load_Signature;

      if Action.Is_Deactivated then
         --  Note: we need to check for deactivated jobs *after* the
         --  signature is computed to understand if the deactivated
         --  action has all its output correct (so that we can unblock
         --  depending non-deactivated actions).

         if Traces.Is_Active then
            pragma Annotate (Xcov, Exempt_On, "debug code");
            Traces.Trace ("job is deactivated: " & Action.UID.Image);
            pragma Annotate (Xcov, Exempt_Off);
         end if;

         return Deactivated;
      end if;

      if not Force and then Action.Valid_Signature then
         if Traces.Is_Active then
            pragma Annotate (Xcov, Exempt_On, "debug code");
            Traces.Trace
              ("Signature is valid, do not execute the job '"
               & Action.UID.Image
               & "'");
            pragma Annotate (Xcov, Exempt_Off);
         end if;

         return Skipped;
      end if;

      if Action in Actions.Process.Object'Class then
         begin
            Actions.Process.Object'Class (Action).Update_Command_Line
              (Slot_Id);

            if Actions.Process.Object'Class (Action)
                 .Command_Line
                 .Argument_List
                 .Is_Empty
            then
               if Traces.Is_Active then
                  pragma Annotate (Xcov, Exempt_On, "debug code");
                  Traces.Trace
                    ("job arguments is empty for '" & Action.UID.Image & "'");
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               return Failed_Cmd_Line_Computation;
            end if;
         exception
            when Action_Error =>
               return Failed_Cmd_Line_Computation;
         end;
      end if;

      return Ready_To_Run;
   end Pre_Run_Status;

   ------------------------------------------
   -- Report_Deactivated_Invalid_Signature --
   ------------------------------------------

   procedure Report_Deactivated_Invalid_Signature
     (Act     : Actions.Object'Class;
      Tree_Db : GPR2.Build.Tree_Db.Object_Access)
   is
      function Find_Activated_And_Unskipped_Successor_Actions
        return Action_Id_Sets.Set;
      --  For each artifact produced by the specified action, find the first
      --  transitive activated actions that depend on it. If a successor
      --  is deactivated, then the search continues until an activated action
      --  is found. If no action is found, then an empty set is returned.

      ----------------------------------------------------
      -- Find_Activated_And_Unskipped_Successor_Actions --
      ----------------------------------------------------

      function Find_Activated_And_Unskipped_Successor_Actions
        return Action_Id_Sets.Set
      is
         --  For each artifact produced by Act, find the first transitive
         --  activated successor actions. If a successor is also deactivated,
         --  the search continues until an activated action is found.
         Result             : Action_Id_Sets.Set := Action_Id_Sets.Empty_Set;
         Actions_To_Process : Action_Id_Sets.Set;
      begin
         Actions_To_Process.Include (Act.UID);

         while not Actions_To_Process.Is_Empty loop
            declare
               Current_Action : constant Action_Id'Class :=
                 Actions_To_Process.First_Element;
            begin
               Actions_To_Process.Exclude (Current_Action);
               for Artifact of Tree_Db.Outputs (Action => Current_Action) loop
                  for Action of Tree_Db.Successors (Artifact) loop
                     if Action.Is_Deactivated then
                        Actions_To_Process.Include (Action.UID);
                     else
                        Result.Include (Action.UID);
                     end if;
                  end loop;
               end loop;
            end;
         end loop;

         return Result;
      end Find_Activated_And_Unskipped_Successor_Actions;

      Impacted_Successors : constant Action_Id_Sets.Set :=
        Find_Activated_And_Unskipped_Successor_Actions;
   begin
      if not Impacted_Successors.Is_Empty then
         Tree_Db.Reporter.Report
           ("Action "
            & Act.UID.Image
            & " has been deactivated, but its"
            & " signature is invalid. As a result, the"
            & " following dependent action(s) will not"
            & " be executed:",
            To_Stderr => True);

         for Successor_ID of Impacted_Successors loop
            Tree_Db.Reporter.Report
              ("   * " & Successor_ID.Image, To_Stderr => True);
         end loop;
      end if;
   end Report_Deactivated_Invalid_Signature;

   ----------------
   -- RF_Message --
   ----------------

   function RF_Message
     (Resp_File_Path : Path_Name.Object; Command : Unbounded_String)
      return String
   is
      Msg : Unbounded_String;
   begin
      Append (Msg, "Response file: @");
      Append (Msg, String (Resp_File_Path.Simple_Name));
      Append (Msg, ": {");
      Append (Msg, Command);
      Append (Msg, "}");

      return To_String (Msg);
   end RF_Message;

end GPR2.Build.Actions_Scheduler;
