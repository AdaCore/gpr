--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

pragma Warnings (Off);
with System.Multiprocessors;
pragma Warnings (On);

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.Directed_Graph; use GNATCOLL.Directed_Graph;

with GPR2.Build.Actions; use GPR2.Build.Actions;
with GPR2.Path_Name;
with GPR2.Reporter;
with GPR2.Source_Reference;
with GPR2.Message;

package body GPR2.Build.Process_Manager is

   package GDG renames GNATCOLL.Directed_Graph;
   package GOP renames GNATCOLL.OS.Process;

   function Effective_Job_Number (N : Natural) return Natural;
   --  If N = 0 return the number of CPUs otherwise return N.

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
   begin
      pragma Assert
        (Proc_Handler.Status /= Running,
         "The process linked to the action '" & Job.UID.Image &
           "' is still running. Cannot collect the job before it finishes");

      if Length (Stdout) > 0 then
         Self.Tree_Db.Report (-Stdout);
      end if;

      if Length (Stderr) > 0 then
         Self.Tree_Db.Report (-Stderr, To_Stderr => True);
      end if;

      if Proc_Handler.Status = Failed_To_Launch
        and then Self.Stop_On_Fail
      then
         return Abort_Execution;
      end if;

      if Proc_Handler.Status = Finished then
         if Self.Traces.Is_Active then
            Self.Traces.Trace
              ("Job '" & Job.UID.Image & "' returned. Status:" &
                 Proc_Handler.Process_Status'Img);
         end if;

         if Proc_Handler.Process_Status /= PROCESS_STATUS_OK then
            Self.Tree_Db.Report
              (Message.Create
                 (Message.Warning,
                  Job.UID.Image & " failed with status" &
                    Proc_Handler.Process_Status'Image,
                  Source_Reference.Create (Job.View.Path_Name.Value, 0, 0)));
         end if;
      end if;

      if (Proc_Handler.Status = Finished
          and then Proc_Handler.Process_Status = PROCESS_STATUS_OK)
        or else Proc_Handler.Status = Skipped
      then
         if not Job.Post_Command
           ((if Proc_Handler.Status = Skipped then Skipped else Success))
         then
            return Abort_Execution;
         end if;

         --  Propagate any newly created action
         if Proc_Handler.Status = Finished
           and then not Self.Tree_Db.Propagate_Actions
         then
            return Abort_Execution;
         end if;
      end if;

      if Proc_Handler.Status = Finished
        and then Proc_Handler.Process_Status = PROCESS_STATUS_OK
      then
         Job.Compute_Signature (Stdout, Stderr);
      end if;

      if Proc_Handler.Status = Finished
        and then Proc_Handler.Process_Status /= PROCESS_STATUS_OK
        and then Self.Stop_On_Fail
      then
         return Abort_Execution;
      end if;

      --  We do not want to manipulate reference types during post commands
      --  procedure as it would prevent actions addition / deletion.

      return Continue_Execution;
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
      Jobs            : Natural := 0;
      Stop_On_Fail    : Boolean := True;
      Keep_Temp_Files : Boolean := False)
   is
      Max_Jobs        : constant Natural := Effective_Job_Number (Jobs);
      --  Effective max number of silmutaneous jobs

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
         --  Allocate listener for stdout
         if Stdout_FD /= FS.Invalid_FD then
            if States (Proc_Id).Stdout_Listener = null then
               States (Proc_Id).Stdout_Listener := new Listener;
            end if;
            States (Proc_Id).Stdout_Active := True;
            States (Proc_Id).Stdout_Listener.Listen (Stdout_FD);
         end if;

         --  Likewise for stderr
         if Stderr_FD /= FS.Invalid_FD then
            if States (Proc_Id).Stderr_Listener = null then
               States (Proc_Id).Stderr_Listener := new Listener;
            end if;
            States (Proc_Id).Stderr_Active := True;
            States (Proc_Id).Stderr_Listener.Listen (Stderr_FD);
         end if;
      end Allocate_Listeners;

      Active_Jobs : Natural := 0;
      --  Current number of active jobs

      Node             : GDG.Node_Id;
      Proc_Handler     : Process_Handler;
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
      Graph            : constant access GDG.Directed_Graph :=
                           Tree_Db.Actions_Graph_Access;

   begin
      Self.Tree_Db      := Tree_Db;
      Self.Stop_On_Fail := Stop_On_Fail;
      Self.Stats        := Empty_Stats;

      Graph.Start_Iterator (Enable_Visiting_State => True);

      loop
         if not End_Of_Iteration then
            --  Launch as many process as possible
            while Active_Jobs < Max_Jobs loop
               begin
                  End_Of_Iteration := not Graph.Next (Node);
               exception
                  when E : GNATCOLL.Directed_Graph.DG_Error =>
                     Tree_Db.Report
                       ("error: internal error in the process manager (" &
                          Ada.Exceptions.Exception_Message (E) & ")");
                     Self.Traces.Trace ("!!! Internal error in the DAG");
                     Self.Traces.Trace
                       (Ada.Exceptions.Exception_Information (E));
                     End_Of_Iteration := True;
               end;

               exit when End_Of_Iteration or else Node = GDG.No_Node;

               declare
                  Act : constant Build.Tree_Db.Action_Reference_Type :=
                          Tree_Db.Action_Id_To_Reference
                            (Tree_Db.Action_Id (Node));
               begin
                  for J in Serialized_Slot'Range loop
                     if not Serialized_Slot (J) then
                        Available_Slot := J;

                        exit;
                     end if;
                  end loop;

                  Self.Launch_Job
                    (Act, Available_Slot, Proc_Handler, P_Stdout, P_Stderr);
                  Self.Stats.Total_Jobs := Self.Stats.Total_Jobs + 1;

                  if Proc_Handler.Status = Running then
                     Active_Jobs := Active_Jobs + 1;
                     Active_Procs (Active_Jobs) := Proc_Handler.Handle;
                     States (Active_Jobs).Node  := Node;
                     Allocate_Listeners (Active_Jobs, P_Stdout, P_Stderr);
                     Serialized_Slot (Available_Slot) := True;
                     Slot_Ids (Active_Jobs) := Available_Slot;

                     if Active_Jobs > Self.Max_Active_Jobs then
                        Self.Stats.Max_Active_Jobs := Active_Jobs;
                     end if;

                  else

                     Graph.Complete_Visit (Node);

                     pragma Assert
                       (Proc_Handler.Status /= Finished,
                        "Process handler status shall not be 'Finished' " &
                          "at this stage");

                     Job_Status :=
                       Collect_Job
                         (Object'Class (Self),
                          Job          => Act,
                          Proc_Handler => Proc_Handler,
                          Stdout       => Act.Saved_Stdout,
                          Stderr       =>
                            (if Proc_Handler.Status = Failed_To_Launch
                             then Proc_Handler.Error_Message
                             else Act.Saved_Stderr));

                     if Job_Status = Abort_Execution then
                        End_Of_Iteration := True;
                        exit;

                     elsif Job_Status = Retry_Job then
                        --  ??? add API to pass node from visiting to non
                        --  visited state

                        raise Program_Error;
                     end if;
                  end if;
               end;
            end loop;
         end if;

         --  Exit when no active jobs

         exit when Active_Jobs = 0;

         --  Wait for a job to finish

         --  ??? Set a timeout as infinite is not working well on linux
         Proc_Id := GOP.Wait_For_Processes
           (Active_Procs (1 .. Active_Jobs), Timeout => 3600.0);

         if Proc_Id > 0 then
            Graph.Complete_Visit (States (Proc_Id).Node);

            --  A process has finished. Call wait to finalize it and get
            --  the final process status.
            Proc_Handler :=
              (Status         => Finished,
               Process_Status => GOP.Wait (Active_Procs (Proc_Id)));

            --  Fetch captured stdout and stderr if necessary

            if States (Proc_Id).Stdout_Active then
               States (Proc_Id).Stdout_Listener.Fetch_Content (Stdout);
               States (Proc_Id).Stdout_Active := False;

            else
               Stdout := Null_Unbounded_String;
            end if;

            if States (Proc_Id).Stderr_Active then
               States (Proc_Id).Stderr_Listener.Fetch_Content (Stderr);
               States (Proc_Id).Stderr_Active := False;
            else
               Stderr := To_Unbounded_String ("");
            end if;

            declare
               UID : constant Actions.Action_Id'Class :=
                       Tree_Db.Action_Id (States (Proc_Id).Node);
               Act : Actions.Object'Class := Self.Tree_Db.Action (UID);
            begin
               --  Call collect
               Job_Status := Collect_Job
                 (Object'Class (Self),
                  Job          => Act,
                  Proc_Handler => Proc_Handler,
                  Stdout       => Stdout,
                  Stderr       => Stderr);

               --  Cleanup the temporary files that are local to the job
               if not Keep_Temp_Files then
                  Act.Cleanup_Temp_Files (Scope => Actions.Local);
               end if;

               --  Push back the potentially modified action to the tree_db
               Tree_Db.Action_Id_To_Reference (UID) := Act;
            end;

            --  Adjust execution depending on returned value
            if Job_Status = Abort_Execution then
               End_Of_Iteration := True;

            elsif Job_Status = Retry_Job then
               --  ??? add API to pass node from visiting to non visited
               --  state
               raise Program_Error;
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

      --  Cleanup the temporary files with global scope
      if not Keep_Temp_Files then
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

   ----------------
   -- Launch_Job --
   ----------------

   procedure Launch_Job
     (Self           : in out Object;
      Job            : in out Actions.Object'Class;
      Slot_Id        :        Positive;
      Proc_Handler   :    out Process_Handler;
      Capture_Stdout :    out File_Descriptor;
      Capture_Stderr :    out File_Descriptor)
   is
      package FS renames GNATCOLL.OS.FS;
      use GPR2.Reporter;

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

      Args : Argument_List;
      Env  : Environment_Dict;
      Cwd  : GPR2.Path_Name.Object;

   begin
      if Job.Valid_Signature then
         if Self.Traces.Is_Active then
            Self.Traces.Trace
              ("Signature is valid, do not execute the job '" &
                 Job.UID.Image & "'");
         end if;

         Proc_Handler := Process_Handler'(Status => Skipped);

         return;
      end if;

      if Job.Skip then
         if Self.Traces.Is_Active then
            Self.Traces.Trace
              ("job asked to be skipped: " & Job.UID.Image);
         end if;

         Proc_Handler := Process_Handler'(Status => Skipped);

         return;

      else
         Job.Compute_Command (Args, Env, Slot_Id);
         Cwd := Job.Working_Directory;


         if Args.Is_Empty then
            if Self.Traces.Is_Active then
               Self.Traces.Trace
                 ("job arguments is empty, skipping '"  & Job.UID.Image & "'");
            end if;

            Proc_Handler := Process_Handler'(Status => Skipped);

            return;
         end if;
      end if;

      if Self.Traces.Is_Active then
         Self.Traces.Trace
           ("Signature is invalid. Execute the job " &
              Job.UID.Image & ", command: " &
              Image (Args));
      end if;

      FS.Open_Pipe (P_Ro, P_Wo);
      FS.Open_Pipe (P_Re, P_We);

      begin
         --  ??? Both message level and Project tree verbosity don't cope with
         --  tooling messages that need quiet/normal/detailed info. Let's go
         --  for the default one *and* verbose one for now
         if Self.Tree_Db.Reporter_Verbosity >= Verbose then
            Self.Tree_Db.Report (Image (Args));
         else
            Self.Tree_Db.Report (Job.UID.Image);
         end if;

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

            Self.Tree_Db.Report
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  Args.First_Element & ": " &
                    Ada.Exceptions.Exception_Message (Ex),
                  GPR2.Source_Reference.Create
                    (Job.View.Path_Name.Value, 0, 0)));

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

   --------------
   -- Listener --
   --------------

   task body Listener is
      Listen_FD : FS.File_Descriptor;
      Result    : Unbounded_String;
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
               Content := Trim (Result, Ada.Strings.Right);
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
