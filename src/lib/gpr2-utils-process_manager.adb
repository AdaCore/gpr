--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

pragma Warnings (Off);
with System.Multiprocessors;
pragma Warnings (On);
with Ada.Unchecked_Deallocation;

package body GPR2.Utils.Process_Manager is

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
      Node            : DG.Node_Id := DG.No_Node;
   end record;
   --  Processes do require two listeners.

   --------------
   -- Listener --
   --------------

   task body Listener is
      use type FS.File_Descriptor;

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
               Content := Result;
            end Fetch_Content;

         else
            --  Handle nicely if FD is an invalid process
            accept Fetch_Content (Content : out Unbounded_String) do
               Content := To_Unbounded_String ("");
            end Fetch_Content;
         end if;

      end loop;
   end Listener;

   ----------------
   -- Clear_Data --
   ----------------

   procedure Clear_Data (Self : in out Process_Manager) is
   begin
      Self.Data := No_Data;
   end Clear_Data;

   -----------------
   -- Collect_Job --
   -----------------

   function Collect_Job
      (Self           : in out Process_Manager;
       Job            : DG.Node_Id;
       Process_Status : Integer;
       Stdout, Stderr : Unbounded_String)
      return Collect_Status
   is
   begin
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
     (Self  : in out Process_Manager'Class;
      Graph : in out DG.Directed_Graph;
      Jobs  : Natural := 0)
   is
      use type DG.Node_Id;
      use type Proc.Process_Handle;
      use type FS.File_Descriptor;

      Max_Jobs : constant Natural := Effective_Job_Number (Jobs);
      --  Effective max number of silmutaneous jobs

      Active_Procs : Proc.Process_Array (1 .. Max_Jobs) :=
         (others => Proc.Invalid_Handle);
      --  Associate a job slot to a process in the graph. Current active PID
      --  are in the 1 .. Active_Jobs range.

      States : array (1 .. Max_Jobs) of Proc_State;
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

      Node     : DG.Node_Id;
      PID      : Proc.Process_Handle;
      P_Stdout : FS.File_Descriptor;
      P_Stderr : FS.File_Descriptor;
      Proc_Id    : Integer;
      Job_Status : Collect_Status;
      Proc_Status : Integer;
      End_Of_Iteration : Boolean := False;
      Stdout, Stderr : Unbounded_String;
   begin
      Self.Clear_Data;
      Graph.Start_Iterator (Enable_Visiting_State => True);

      loop
         if not End_Of_Iteration then

            --  Launch as many process as possible
            while Active_Jobs < Max_Jobs loop
               End_Of_Iteration := not Graph.Next (Node);
               exit when Node = DG.No_Node;

               begin
                  Self.Launch_Job (Node, PID, P_Stdout, P_Stderr);
                  Self.Data.Total_Jobs := Self.Data.Total_Jobs + 1;
               exception
                  when GNATCOLL.OS.OS_Error =>
                     PID := Proc.Invalid_Handle;
               end;

               if PID /= Proc.Invalid_Handle then
                  --  If the process was launched successfully add it to the
                  --  list of active jobs
                  Active_Jobs := Active_Jobs + 1;
                  Active_Procs (Active_Jobs) := PID;
                  States (Active_Jobs).Node := Node;
                  Allocate_Listeners (Active_Jobs, P_Stdout, P_Stderr);
               else
                  --  Call collect on that job with a process status of 127
                  Job_Status := Self.Collect_Job
                     (Job            => Node,
                      Process_Status => 127,
                      Stdout => To_Unbounded_String (""),
                      Stderr => To_Unbounded_String (""));
                  if Job_Status = Abort_Execution then
                     End_Of_Iteration := True;
                  elsif Job_Status = Retry_Job then
                     --  ??? add API to pass node from visiting to non visited
                     --  state
                     raise Program_Error;
                  else
                     Graph.Complete_Visit (Node);
                  end if;
               end if;

               if Active_Jobs > Self.Max_Active_Jobs then
                  Self.Data.Max_Active_Jobs := Active_Jobs;
               end if;

            end loop;
         end if;

         --  Wait for a job to finish
         if Active_Jobs > 0 then

            --  ??? Set a timeout as infinite is not working well on linux
            Proc_Id := Proc.Wait_For_Processes
               (Active_Procs (1 .. Active_Jobs), Timeout => 3600.0);

            if Proc_Id > 0 then

               --  A process has finished. Call wait to finalize it and get
               --  the final process status.
               Proc_Status := Proc.Wait (Active_Procs (Proc_Id));

               --  Fetch captured stdout and stderr if necessary
               if States (Proc_Id).Stdout_Active then
                  States (Proc_Id).Stdout_Listener.Fetch_Content (Stdout);
                  States (Proc_Id).Stdout_Active := False;
               else
                  Stdout := To_Unbounded_String ("");
               end if;

               if States (Proc_Id).Stderr_Active then
                  States (Proc_Id).Stderr_Listener.Fetch_Content (Stderr);
                  States (Proc_Id).Stderr_Active := False;
               else
                  Stderr := To_Unbounded_String ("");
               end if;

               --  Call collect
               Job_Status := Self.Collect_Job
                  (Job            => States (Proc_Id).Node,
                   Process_Status => Proc_Status,
                   Stdout         => Stdout,
                   Stderr         => Stderr);

               --  Adjust execution depending on returned value
               if Job_Status = Abort_Execution then
                  End_Of_Iteration := True;
               elsif Job_Status = Retry_Job then
                  --  ??? add API to pass node from visiting to non visited
                  --  state
                  raise Program_Error;
               else
                  Graph.Complete_Visit (States (Proc_Id).Node);
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
                  end;
               end if;

            end if;
         end if;

         exit when End_Of_Iteration and then Active_Jobs = 0;
      end loop;

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
   end Execute;

   ---------------------
   -- Max_Active_Jobs --
   ---------------------

   function Max_Active_Jobs (Self : Process_Manager) return Natural is
   begin
      return Self.Data.Max_Active_Jobs;
   end Max_Active_Jobs;

   ----------------
   -- Total_Jobs --
   ----------------

   function Total_Jobs (Self : Process_Manager) return Natural is
   begin
      return Self.Data.Total_Jobs;
   end Total_Jobs;

end GPR2.Utils.Process_Manager;
