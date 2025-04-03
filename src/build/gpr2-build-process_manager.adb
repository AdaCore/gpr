--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

pragma Warnings (Off);
with GPR2.Build.Options;
with System.Multiprocessors;
pragma Warnings (On);

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.Directed_Graph; use GNATCOLL.Directed_Graph;

with GPR2.Build.Actions; use GPR2.Build.Actions;
with GPR2.Build.Actions.Link;
with GPR2.Build.Tree_Db;
with GPR2.Reporter;
with GPR2.Source_Reference;
with GPR2.Message;

package body GPR2.Build.Process_Manager is

   package GDG renames GNATCOLL.Directed_Graph;
   package GOP renames GNATCOLL.OS.Process;

   PROCESS_STATUS_OK : constant Integer := 0;

   function Effective_Job_Number (N : Natural) return Natural;
   --  If N = 0 return the number of CPUs otherwise return N.

   function Image (Command : Argument_List) return String;
   --  Return the representation of the command

   --------------
   -- Listener --
   --------------

   --  A listener is a task dedicated to the listening on a file descriptor
   --  the task can be re-used for several file descriptors, following the
   --  following pattern:
   --    Listen (FD1), Fetch_Content (Outpu2); Listen (FD2);
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

   type Listener_Access is access all Listener;

   procedure Free is new Ada.Unchecked_Deallocation
      (Listener, Listener_Access);

   type Proc_State is record
      Stdout_Listener : Listener_Access := null;
      Stdout_Active   : Boolean := False;
      Stderr_Listener : Listener_Access := null;
      Stderr_Active   : Boolean := False;
      Node            : GDG.Node_Id := GDG.No_Node;
   end record;
   --  Processes do require two listeners.

   -----------
   -- Clear --
   -----------

   procedure Clear (Ctxt : in out Process_Execution_Context) is
   begin
      Ctxt.Actions.Clear;
      Ctxt.Graph.Clear;
      Ctxt.Nodes.Clear;
      Ctxt.Status := Success;
   end Clear;

   -----------------
   -- Collect_Job --
   -----------------

   function Collect_Job
     (Self           : in out Object;
      Job            : in out Actions.Object'Class;
      Proc_Handler   : Process_Handler;
      Stdout, Stderr : Unbounded_String)
      return Collect_Status
   is
      Failed_Status : Boolean;
   begin
      pragma Assert
        (Proc_Handler.Status /= Running,
         "The process linked to the action '" & Job.UID.Image &
           "' is still running. Cannot collect the job before it finishes");

      if Length (Stdout) > 0 then
         Self.Tree_Db.Reporter.Report
           (-Stdout, Level => GPR2.Message.Important);
      end if;

      if Length (Stderr) > 0 then
         Self.Tree_Db.Reporter.Report
           (-Stderr, To_Stderr => True, Level => GPR2.Message.Important);
      end if;

      case Proc_Handler.Status is
         when Failed_To_Launch =>
            return Abort_Execution;

         when Finished =>
            if Self.Traces.Is_Active then
               Self.Traces.Trace
                 ("Job '" & Job.UID.Image & "' returned. Status:" &
                    Proc_Handler.Process_Status'Img);
            end if;

            if Proc_Handler.Process_Status /= PROCESS_STATUS_OK then
               Self.Tree_Db.Reporter.Report
                 (Message.Create
                    (Message.Warning,
                     Job.UID.Image & " failed with status" &
                       Proc_Handler.Process_Status'Image,
                     Source_Reference.Create
                       (Job.View.Path_Name.Value, 0, 0)));
            end if;

         when Skipped =>
            if Job in Actions.Link.Object'Class then
               declare
                  Link : constant Actions.Link.Object'Class :=
                           Actions.Link.Object'Class (Job);
               begin
                  if not Link.Is_Library then
                     Self.Tree_Db.Reporter.Report
                       ('"' & String (Link.Output.Path.Simple_Name) &
                          """ up to date");
                  end if;
               end;
            end if;

         when Running | Deactivated | Pending =>
            null;
      end case;

      Failed_Status :=
        (Proc_Handler.Status = Finished
         and then Proc_Handler.Process_Status /= PROCESS_STATUS_OK)
        or else (Proc_Handler.Status = Skipped and then
                   not Job.Valid_Signature);

      if Failed_Status then
         return Abort_Execution;

      elsif Proc_Handler.Status /= Deactivated
        or else Job.Valid_Signature
      then
         if not Job.Post_Command
           ((if Proc_Handler.Status in Skipped | Deactivated
             then Skipped else Success))
         then
            return Abort_Execution;
         end if;

         --  Propagate any newly created action
         if Proc_Handler.Status = Finished then
            if not Self.Tree_Db.Propagate_Actions then
               return Abort_Execution;
            end if;

            if not Job.Write_Signature (Stdout, Stderr) then
               return Abort_Execution;
            end if;
         end if;
      end if;

      --  We do not want to manipulate reference types during post commands
      --  procedure as it would prevent actions addition / deletion.

      return Continue_Execution;

   exception
      when E : others =>
         Self.Tree_Db.Reporter.Report
           ("!!! Unexpected exception caught" & ASCII.LF &
              Ada.Exceptions.Exception_Information (E),
            To_Stderr => True,
            Level     => GPR2.Message.Important);
         return Abort_Execution;
   end Collect_Job;

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
     (Self            : in out Object;
      Tree_Db         : GPR2.Build.Tree_Db.Object_Access;
      Context         : access Process_Execution_Context;
      Options         : PM_Options)
   is
      Max_Jobs        : constant Natural :=
                          Effective_Job_Number (Options.Jobs);
      --  Effective max number of silmutaneous jobs

      JS              : Build.Jobserver.Object'Class :=
                          Build.Jobserver.Initialize;
      --  The potential jobserver to get token on

      Active_Procs    : GOP.Process_Array (1 .. Max_Jobs) :=
                          (others => GOP.Invalid_Handle);
      --  Associate a job slot to a process in the graph. Current active PID
      --  are in the 1 .. Active_Jobs range.

      Serialized_Slot : array (1 .. Max_Jobs) of Boolean := (others => False);
      --  Slot id used to ensure each action has access to an id ensuring that
      --  no two actions with the same id can be executed at the same time.

      Slot_Ids        : array (1 .. Max_Jobs) of Natural :=
                          (others => 0);
      --  Translates a slot in Active_Procs as a slot in Serialized_Slots

      States          : array (1 .. Max_Jobs) of Proc_State;
      --  State associated with each active processes

      procedure Allocate_Listeners
        (Proc_Id : Natural; Stdout_FD, Stderr_FD : FS.File_Descriptor);
      --  Allocate listeners

      ------------------------
      -- Allocate_Listeners --
      ------------------------

      procedure Allocate_Listeners
         (Proc_Id : Natural; Stdout_FD, Stderr_FD : FS.File_Descriptor)
      is
      begin
         if Stdout_FD = FS.Invalid_FD or else Stderr_FD = FS.Invalid_FD then
            pragma Annotate (Xcov, Exempt_On, "defensive code");
            raise Process_Manager_Error with
              "Error when spawning a subprocess: cannot redirect I/O";
            pragma Annotate (Xcov, Exempt_Off);
         end if;

         --  Allocate listener for stdout
         if States (Proc_Id).Stdout_Listener = null then
            States (Proc_Id).Stdout_Listener := new Listener;
         end if;
         States (Proc_Id).Stdout_Active := True;
         States (Proc_Id).Stdout_Listener.Listen (Stdout_FD);

         --  Likewise for stderr
         if States (Proc_Id).Stderr_Listener = null then
            States (Proc_Id).Stderr_Listener := new Listener;
         end if;
         States (Proc_Id).Stderr_Active := True;
         States (Proc_Id).Stderr_Listener.Listen (Stderr_FD);
      end Allocate_Listeners;

      Active_Jobs : Natural := 0;
      --  Current number of active jobs

      Node             : GDG.Node_Id;
      Proc_Handler_L   : Process_Handler;
      --  Process handler used to launch a job.
      --  In a Pending status, this process handler must remain untouched to
      --  properly allow to re-launch the job and not proceed the iteration.
      Proc_Handler_T   : Process_Handler;
      --  Process handler used to terminate job.
      --  A separate handler is needed in the case of Proc_Handler_L status
      --  being Pending and we have to enter Wait_For_Processes before actually
      --  suceeding to re-launch the job.
      --  Otherwise the process handler is overwritten and the iteration
      --  proceeds and a node is lost in the iteration.
      P_Stdout         : FS.File_Descriptor;
      P_Stderr         : FS.File_Descriptor;
      Proc_Id          : Integer;
      Job_Status       : Collect_Status;
      End_Of_Iteration : Boolean := False;
      Available_Slot   : Natural;
      --  This slot allows to get some Id where we know the previous action
      --  having used it is done. So serialized calls can be performed on this
      --  basis if needed.

      Stdout, Stderr   : Unbounded_String;
      Executed         : Natural := 0;

      Script_FD        : GNATCOLL.OS.FS.File_Descriptor := Null_FD;
      Script_Dir       : Path_Name.Object;

   begin
      Self.Tree_Db      := Tree_Db;
      Self.Stats        := Empty_Stats;

      if Options.Script_File.Is_Defined then
         Script_FD := GNATCOLL.OS.FS.Open
           (Options.Script_File.String_Value,
            GNATCOLL.OS.FS.Write_Mode);

         if Script_FD = Invalid_FD then
            Tree_Db.Reporter.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  "could not create the script file '" &
                    Options.Script_File.String_Value & '"',
                  Source_Reference.Create
                    (Options.Script_File.Value, 0, 0)));
            Script_FD := Null_FD;
         end if;
      end if;

      Context.Graph.Start_Iterator (Enable_Visiting_State => True);

      loop
         --  Launch as many process as possible
         while Active_Jobs < Max_Jobs and then not End_Of_Iteration loop
            begin
               if not (Proc_Handler_L.Status = Pending) then
                  End_Of_Iteration := not Context.Graph.Next (Node);
               end if;
            exception
               when E : GNATCOLL.Directed_Graph.DG_Error =>
                  pragma Annotate (Xcov, Exempt_On, "defensive code");
                  Tree_Db.Reporter.Report
                    ("error: internal error in the process manager (" &
                       Ada.Exceptions.Exception_Message (E) & ")");
                  Self.Traces.Trace ("!!! Internal error in the DAG");
                  Self.Traces.Trace
                    (Ada.Exceptions.Exception_Information (E));
                  End_Of_Iteration := True;
                  pragma Annotate (Xcov, Exempt_Off);
            end;

            exit when End_Of_Iteration or else Node = GDG.No_Node;

            declare
               Act : constant Build.Tree_Db.Action_Reference_Type :=
                       Tree_Db.Action_Id_To_Reference
                         (Context.Actions (Node));
               Cd_Args : GNATCOLL.OS.Process.Argument_List;
               use type Path_Name.Object;
            begin
               for J in Serialized_Slot'Range loop
                  if not Serialized_Slot (J) then
                     Available_Slot := J;

                     exit;
                  end if;
               end loop;

               Self.Launch_Job
                 (JS, Act, Available_Slot, Options.Force, Proc_Handler_L,
                  P_Stdout, P_Stderr);

               if not (Proc_Handler_L.Status = Pending) then
                  Self.Stats.Total_Jobs := Self.Stats.Total_Jobs + 1;
               end if;

               if Proc_Handler_L.Status = Running then

                  --  Add the executing command to the script:

                  if Script_FD /= Null_FD then
                     --  Update the working directory
                     if not Script_Dir.Is_Defined
                       or else Script_Dir /= Act.Working_Directory
                     then
                        Cd_Args.Append ("cd");
                        Cd_Args.Append (Act.Working_Directory.String_Value);
                        GNATCOLL.OS.FS.Write
                          (Script_FD,
                           Image (Cd_Args) & ASCII.LF);
                        Script_Dir := Act.Working_Directory;
                     end if;

                     --  Setup env variables for the run:

                     for C in
                       Act.Command_Line.Environment_Variables.Iterate
                     loop
                        GNATCOLL.OS.FS.Write
                          (Script_FD,
                           Env_Dicts.Key (C) & "=""" &
                             Env_Dicts.Element (C) & """ ");
                     end loop;

                     --  and execute the action:

                     GNATCOLL.OS.FS.Write
                       (Script_FD,
                        Image (Act.Command_Line.Argument_List) & ASCII.LF);
                  end if;

                  --  If we have a Jobserver, associate the pre-ordered token
                  --  to the process id.
                  if JS.Active
                    and then not JS.Register_Token (Proc_Handler_L.Handle)
                  then
                     Tree_Db.Reporter.Report
                       ("error: could not register identified token");
                  end if;

                  Active_Jobs := Active_Jobs + 1;
                  Active_Procs (Active_Jobs) := Proc_Handler_L.Handle;
                  States (Active_Jobs).Node  := Node;
                  Allocate_Listeners (Active_Jobs, P_Stdout, P_Stderr);
                  Serialized_Slot (Available_Slot) := True;
                  Slot_Ids (Active_Jobs) := Available_Slot;

                  if Active_Jobs > Self.Max_Active_Jobs then
                     Self.Stats.Max_Active_Jobs := Active_Jobs;
                  end if;

               elsif Proc_Handler_L.Status = Pending then
                  --  We asked for a token but did not have a positive response
                  --  and we internally have an active job, try to wait for it
                  --  to end to free a token.
                  exit when
                    (JS.Active and then JS.Busy) and then Active_Jobs > 0;

               else
                  Executed := Executed + 1;

                  if Proc_Handler_L.Status = Finished then
                     pragma Annotate (Xcov, Exempt_On, "Defensive code");
                     Self.Traces.Trace
                       ("Error: Process handler status shall not be " &
                          "'Finished' at this stage");
                     raise Process_Manager_Error with
                       "Invalid process manager internal state, aborting";
                     pragma Annotate (Xcov, Exempt_Off);
                  end if;

                  if Proc_Handler_L.Status in Skipped | Deactivated
                    and then Act.Valid_Signature
                  then
                     --  Only consider the visit complete for valid skipped
                     --  actions, else this will enable the dependent actions
                     --  that won't have the proper inputs to complete
                     Context.Graph.Complete_Visit (Node);
                  end if;

                  --  Cleanup the temporary files that are local to the job
                  if not Options.Keep_Temp_Files then
                     Act.Cleanup_Temp_Files (Scope => Actions.Local);
                  end if;

                  Job_Status :=
                    Collect_Job
                      (Object'Class (Self),
                       Job          => Act,
                       Proc_Handler => Proc_Handler_L,
                       Stdout       => Act.Saved_Stdout,
                       Stderr       =>
                         (if Proc_Handler_L.Status = Failed_To_Launch
                          then Proc_Handler_L.Error_Message
                          else Act.Saved_Stderr));

                  if Job_Status = Abort_Execution then
                     if Proc_Handler_L.Status in Skipped | Deactivated then
                        if Context.Status /= Failed then
                           Context.Status := Errors;
                        end if;
                     else
                        Context.Status := Failed;
                     end if;

                     if Options.Stop_On_Fail then
                        End_Of_Iteration := True;
                        exit;
                     end if;
                  end if;
               end if;

            exception
               when E : Process_Manager_Error =>
                  pragma Annotate (Xcov, Exempt_On, "defensive code");
                  End_Of_Iteration := True;
                  Tree_Db.Reporter.Report
                    ("Fatal error: " &
                       Ada.Exceptions.Exception_Message (E),
                     To_Stderr => True);
               when E : others =>
                  End_Of_Iteration := True;
                  Tree_Db.Reporter.Report
                    ("Unexpected exception:",
                     To_Stderr => True);
                  Tree_Db.Reporter.Report
                    (Ada.Exceptions.Exception_Information (E),
                     To_Stderr => True);
                  pragma Annotate (Xcov, Exempt_Off);
            end;
         end loop;

         --  Defensive code : If we leave the process launching loop with an
         --  unidentified token, release it. This should be reachable only if a
         --  process was spawned but with a non-running status, in this case
         --  the process manager will stop and we have to release the token to
         --  the jobserver.
         if JS.Active and then JS.Has_Unidentified_Token then
            if not JS.Unregister_Token (Identified => False) then
               Self.Traces.Trace
                 ("error: could not unregister unidentifier token");
            end if;
         end if;

         --  Exit when no active jobs

         exit when Active_Jobs = 0;

         --  Wait for a job to finish

         --  ??? Set a timeout as infinite is not working well on linux
         Proc_Id := GOP.Wait_For_Processes
           (Active_Procs (1 .. Active_Jobs), Timeout => 3600.0);

         if Proc_Id > 0 then
            --  The process stopped, release the associated token
            if JS.Active
              and then not JS.Unregister_Token (Active_Procs (Proc_Id))
            then
               Tree_Db.Reporter.Report
                 ("error: could not unregister jobserver token");
            end if;

            --  A process has finished. Call wait to finalize it and get
            --  the final process status.
            Proc_Handler_T :=
              (Status         => Finished,
               Process_Status => GOP.Wait (Active_Procs (Proc_Id)));

            --  Fetch captured stdout and stderr if necessary

            States (Proc_Id).Stdout_Listener.Fetch_Content (Stdout);
            States (Proc_Id).Stdout_Active := False;

            States (Proc_Id).Stderr_Listener.Fetch_Content (Stderr);
            States (Proc_Id).Stderr_Active := False;

            declare
               UID : constant Actions.Action_Id'Class :=
                       Context.Actions (States (Proc_Id).Node);
               Act : Actions.Object'Class := Self.Tree_Db.Action (UID);
            begin
               --  Call collect
               Job_Status := Collect_Job
                 (Object'Class (Self),
                  Job          => Act,
                  Proc_Handler => Proc_Handler_T,
                  Stdout       => Stdout,
                  Stderr       => Stderr);

               --  Cleanup the temporary files that are local to the job
               if not Options.Keep_Temp_Files then
                  Act.Cleanup_Temp_Files (Scope => Actions.Local);
               end if;

               --  Push back the potentially modified action to the tree_db
               Tree_Db.Action_Id_To_Reference (UID) := Act;

               --  Report the progress if requested
               Executed := Executed + 1;

               if Options.Show_Progress then
                  declare
                     Percent : constant String :=
                                 Natural'Image
                                   ((Executed * 100) /
                                      Natural (Context.Nodes.Length));
                  begin
                     Tree_Db.Reporter.Report
                       ("completed" & Executed'Image & " out of" &
                          Context.Nodes.Length'Image & " (" &
                          Percent (Percent'First + 1 .. Percent'Last) & "%)",
                        Level => GPR2.Message.Important);
                  end;
               end if;

            end;

            if Job_Status = Continue_Execution then
               --  Mark as visited only successful executions
               Context.Graph.Complete_Visit (States (Proc_Id).Node);
            else
               if Context.Status /= Failed then
                  Context.Status := Errors;
               end if;

               if Options.Stop_On_Fail then
                  --  Adjust execution depending on returned value
                  End_Of_Iteration := True;
               end if;
            end if;

            --  Remove job from the list of active jobs.
            Active_Jobs := Active_Jobs - 1;

            if Active_Jobs > 0 and then Proc_Id <= Active_Jobs then
               declare
                  Tmp : constant Proc_State := States (Proc_Id);
               begin
                  Active_Procs (Proc_Id) := Active_Procs (Active_Jobs + 1);

                  --  Real swap of state is necessarty in order not to lose
                  --  track of already allocated listeners.
                  States (Proc_Id) := States (Active_Jobs + 1);
                  States (Active_Jobs + 1) := Tmp;

                  --  Available slot for serialization is now free
                  Serialized_Slot (Slot_Ids (Proc_Id)) := False;
                  --  Adjust the translation table
                  Slot_Ids (Proc_Id) := Slot_Ids (Active_Jobs + 1);
               end;
            end if;
         end if;


         exit when End_Of_Iteration and then Active_Jobs = 0;
      end loop;

      --  End of the process manager execution
      if JS.Active then
         --  Defensive code : We reached the end of the process manager
         --  execution and we still have registered token, release them in
         --  order to not block any other processes.
         if JS.Has_Registered_Tokens then
            Self.Traces.Trace
              ("error: Unregister remaining tokens");
            JS.Unregister_Tokens;
         end if;
         --  Finalize the jobserver token reading task
         JS.Finalize;
      end if;

      --  Close the script file if needed
      if Script_FD /= Null_FD then
         GNATCOLL.OS.FS.Close (Script_FD);
      end if;

      --  Cleanup the temporary files with global scope
      if not Options.Keep_Temp_Files then
         Tree_Db.Clear_Temp_Files;
      end if;

      --  End the allocated listeners.
      for State of States loop
         if State.Stdout_Listener /= null then
            State.Stdout_Listener.Listen (FS.Null_FD);
            Free (State.Stdout_Listener);
         end if;

         if State.Stderr_Listener /= null then
            State.Stderr_Listener.Listen (FS.Null_FD);
            Free (State.Stderr_Listener);
         end if;
      end loop;

      Execution_Post_Process (Object'Class (Self));
   end Execute;

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

         if Ada.Strings.Fixed.Index (Arg, " ") > 0 then
            Append (Result, '"');
            Append (Result, Arg);
            Append (Result, '"');
         else
            Append (Result, Arg);
         end if;
      end loop;

      return -Result;
   end Image;

   ----------------
   -- Launch_Job --
   ----------------

   procedure Launch_Job
     (Self           : in out Object;
      JS             : in out Build.Jobserver.Object'Class;
      Job            : in out Actions.Object'Class;
      Slot_Id        :        Positive;
      Force          :        Boolean;
      Proc_Handler   : in out Process_Handler;
      Capture_Stdout :    out File_Descriptor;
      Capture_Stderr :    out File_Descriptor)
   is
      package FS renames GNATCOLL.OS.FS;
      use GPR2.Reporter;

      procedure Display (Action : Action_Id'Class);
      procedure Display (Command : Argument_List);

      -------------
      -- Display --
      -------------

      procedure Display (Action : Action_Id'Class) is
         Res              : Unbounded_String;
         Action_Class_Max : constant Natural := 18;

      begin
         Append (Res, '[');

         if Action.Language /= No_Language then
            Append (Res, Image (Action.Language));
            Append (Res, ' ');
         end if;

         Append (Res, Action.Action_Class);
         Append (Res, ']');

         if Length (Res) < Action_Class_Max then
            Append (Res, (Action_Class_Max - Length (Res)) * ' ');
         else
            Append (Res, ' ');
         end if;

         Append (Res, Action.Action_Parameter);
         Self.Tree_Db.Reporter.Report (-Res);
      end Display;

      -------------
      -- Display --
      -------------

      procedure Display (Command : Argument_List) is
      begin
         Self.Tree_Db.Reporter.Report (Image (Command));
      end Display;

      P_Wo : FS.File_Descriptor;
      P_Ro : FS.File_Descriptor;
      P_We : FS.File_Descriptor;
      P_Re : FS.File_Descriptor;

   begin
      if Proc_Handler.Status /= Pending then
         if Job.View.Is_Externally_Built then
            if Self.Traces.Is_Active then
               pragma Annotate (Xcov, Exempt_On, "debug code");
               Self.Traces.Trace
                 ("job externally built: " & Job.UID.Image);
               pragma Annotate (Xcov, Exempt_Off);
            end if;

            Proc_Handler := Process_Handler'(Status => Deactivated);

            return;
         end if;

         begin
            --  Load and check the job's signature

            Job.Load_Signature;

            if Job.Is_Deactivated then
               --  Note: we need to check for deactivated jobs *after* the
               --  signature is computed to understand if the deactivated
               --  action has all its output correct (so that we can unblock
               --  depending non-deactivated actions).

               if Self.Traces.Is_Active then
                  pragma Annotate (Xcov, Exempt_On, "debug code");
                  Self.Traces.Trace
                    ("job is deactivated: " & Job.UID.Image);
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               Proc_Handler := Process_Handler'(Status => Deactivated);

               return;

            elsif Job.Skip then
               if Self.Traces.Is_Active then
                  pragma Annotate (Xcov, Exempt_On, "debug code");
                  Self.Traces.Trace
                    ("job asked to be skipped: " & Job.UID.Image);
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               Proc_Handler := Process_Handler'(Status => Skipped);

               return;
            end if;

            if not Force and then Job.Valid_Signature then
               if Self.Traces.Is_Active then
                  pragma Annotate (Xcov, Exempt_On, "debug code");
                  Self.Traces.Trace
                    ("Signature is valid, do not execute the job '" &
                       Job.UID.Image & "'");
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               Proc_Handler := Process_Handler'(Status => Skipped);

               return;
            end if;

            Job.Update_Command_Line (Slot_Id);

            if Job.Command_Line.Argument_List.Is_Empty then
               if Self.Traces.Is_Active then
                  pragma Annotate (Xcov, Exempt_On, "debug code");
                  Self.Traces.Trace
                    ("job arguments is empty, skipping '"  & Job.UID.Image &
                       "'");
                  pragma Annotate (Xcov, Exempt_Off);
               end if;

               Proc_Handler := Process_Handler'(Status => Skipped);

               return;
            end if;

            if not Job.Pre_Command then
               raise Action_Error;
            end if;

         exception
            when Action_Error =>
               Proc_Handler :=
                 Process_Handler'
                   (Status        => Failed_To_Launch,
                    Error_Message => To_Unbounded_String
                      ("Command '" & Image (Job.UID) & "' failed."));

               return;

            when E : others =>
               Self.Tree_Db.Reporter.Report
                 ("Unexpected exception:",
                  To_Stderr => True);
               Self.Tree_Db.Reporter.Report
                 (Ada.Exceptions.Exception_Information (E),
                  To_Stderr => True);
               Proc_Handler :=
                 Process_Handler'
                   (Status        => Failed_To_Launch,
                    Error_Message => Null_Unbounded_String);

               return;
         end;
      end if;

      if not JS.Active or else JS.Preorder_Token then
         FS.Open_Pipe (P_Ro, P_Wo);
         FS.Open_Pipe (P_Re, P_We);

         begin
            --  ??? Both message level and Project tree verbosity don't cope
            --  with tooling messages that need quiet/normal/detailed info.
            --  Let's go for the default one *and* verbose one for now
            if Self.Tree_Db.Reporter.User_Verbosity >= Verbose
              or else (Self.Tree_Db.Reporter.User_Verbosity = Unset
                       and then Self.Tree_Db.Reporter.Verbosity >= Verbose)
            then
               Display (Job.Command_Line.Argument_List);
            else
               Display (Job.UID);
            end if;

            Proc_Handler :=
              (Status => Running,
               Handle => Start
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

               Self.Tree_Db.Reporter.Report
                 (GPR2.Message.Create
                    (GPR2.Message.Error,
                     Job.Command_Line.Argument_List.First_Element & ": " &
                       Ada.Exceptions.Exception_Message (Ex),
                     GPR2.Source_Reference.Create
                       (Job.View.Path_Name.Value, 0, 0)));

               Proc_Handler :=
                 (Status        => Failed_To_Launch,
                  Error_Message => To_Unbounded_String
                    ("Command '" & Image (Job.Command_Line.Argument_List) &
                       "' failed: " &
                       Ada.Exceptions.Exception_Message (Ex)));
         end;

         FS.Close (P_Wo);
         FS.Close (P_We);

         Capture_Stdout := P_Ro;
         Capture_Stderr := P_Re;
      else
         Proc_Handler := Process_Handler'(Status => Pending);
      end if;
   end Launch_Job;

   --------------
   -- Listener --
   --------------

   task body Listener is
      Listen_FD : FS.File_Descriptor;
      Result    : Unbounded_String;
      To_Trim   : constant Ada.Strings.Maps.Character_Set :=
                    Ada.Strings.Maps.To_Set (" " & ASCII.CR & ASCII.LF);
   begin
      loop
         --  Fetch FD to listen too
         accept Listen (FD : FS.File_Descriptor) do
            Listen_FD := FD;
         end Listen;

         --  If null FD then exit
         exit when Listen_FD = FS.Null_FD;

         if Listen_FD /= FS.Invalid_FD then
            --  If a valid FD then fetch the full content
            Result := FS.Read (Listen_FD, Buffer_Size => 8 * 1024);

            --  Close the file descriptor
            FS.Close (Listen_FD);

            --  And then make the result available
            accept Fetch_Content (Content : out Unbounded_String) do
               Content := Trim
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
   end Listener;

   ---------------------
   -- Max_Active_Jobs --
   ---------------------

   function Max_Active_Jobs (Self : Object) return Natural is
   begin
      return Self.Stats.Max_Active_Jobs;
   end Max_Active_Jobs;

   ----------------
   -- Total_Jobs --
   ----------------

   function Total_Jobs (Self : Object) return Natural is
   begin
      return Self.Stats.Total_Jobs;
   end Total_Jobs;

end GPR2.Build.Process_Manager;
