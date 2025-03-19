--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;

with GPR2.Build.Jobserver.No_Method;
with GPR2.Build.Jobserver.Pipe;
with GPR2.Build.Jobserver.Semaphore;

package body GPR2.Build.Jobserver is

   package Task_Manager is
      type Task_Token_Status is (Unknown, Available, Unavailable);

      type Task_State is (Idle, Busy, Terminated);

      type Token_Process_State is (Idle, Pending);

      --  Allows to safely check the token reading task state :
      --     Idle       : The task is ready to be asked to read a token
      --     Busy       : The task is in the process of reading a token
      --     Terminated : The task stopped
      protected Task_State_Object is
         procedure Set (State : Task_State);
         function Get return Task_State;
      private
         S : Task_State := Idle;
      end Task_State_Object;

      --  Allows to safely check the token status :
      --     Unknown     : Default value whenever the task is asked to read a
      --                   token.
      --     Available   : The token is available. The jobserver allowed a
      --                   process to be spawned
      --     Unavailable : The token is unavailable. The jobserver did not
      --                   allow a process to be spawned.
      --  Note : The Task_Token_Status flow changes depending on the jobserver
      --  implementation.
      --     Blocking (Simple_Pipe)    : Unknown --- <blocked> ---> Available
      --     Non-Blocking (Named_Pipe) : Unknown ---> Available or
      --                                 Unknown ---> Unavailable
      protected Task_Token_Status_Object is
         procedure Set (Status : Task_Token_Status);
         function Get return Task_Token_Status;
      private
         S : Task_Token_Status := Unknown;
      end Task_Token_Status_Object;

      --  Allows to safely check the token process state :
      --     Idle    : No token were asked
      --     Pending : A token has been asked
      protected Token_Process_State_Object is
         procedure Set (State : Token_Process_State);
         function Get return Token_Process_State;
      private
         S : Token_Process_State := Idle;
      end Token_Process_State_Object;

      --  Allows to safely check the token value
      protected Token_Value_Object is
         procedure Set (Char : Character);
         function Get return Character;
      private
         C : Character := ASCII.NUL;
      end Token_Value_Object;

      --  Allows to ensure the informations are synced
      protected Sync_Proc_Task_Object is
         procedure Set (Value : Boolean);
         function Synced return Boolean;
      private
         V : Boolean := True;
      end Sync_Proc_Task_Object;

      --  Allows to safely check the task life cycle
      --  The task stays in the entry and waits to be activated :
      --     True  : The task will read a token
      --     False : The task will terminate
      protected Task_Life_Circle_Object is
         procedure Set (Alive : Boolean);
         entry Get (Alive : out Boolean);
      private
         Value  : Boolean := False;
         Is_Set : Boolean := False;
      end Task_Life_Circle_Object;

      task type Jobserver_Task (Self : access Object'Class) is
      end Jobserver_Task;

      procedure Launch_Task (Self : access Object'Class);
   end Task_Manager;

   package JS_No_Method renames GPR2.Build.Jobserver.No_Method;
   package JS_Pipe      renames GPR2.Build.Jobserver.Pipe;
   package JS_Semaphore renames GPR2.Build.Jobserver.Semaphore;
   package TM           renames Task_Manager;

   package body Task_Manager is

      JS_Task : access Jobserver_Task;
      pragma Unreferenced (JS_Task);

      protected body Task_State_Object is
         procedure Set (State : Task_State) is
         begin
            S := State;
         end Set;
         function Get return Task_State is (S);
      end Task_State_Object;

      protected body Task_Token_Status_Object is
         procedure Set (Status : Task_Token_Status) is
         begin
            S := Status;
         end Set;
         function Get return Task_Token_Status is (S);
      end Task_Token_Status_Object;

      protected body Token_Process_State_Object is
         procedure Set (State : Token_Process_State) is
         begin
            S := State;
         end Set;
         function Get return Token_Process_State is (S);
      end Token_Process_State_Object;

      protected body Token_Value_Object is
         procedure Set (Char : Character) is
         begin
            C := Char;
         end Set;
         function Get return Character is (C);
      end Token_Value_Object;

      protected body Sync_Proc_Task_Object is
         procedure Set (Value : Boolean) is
         begin
            V := Value;
         end Set;
         function Synced return Boolean is (V);
      end Sync_Proc_Task_Object;

      protected body Task_Life_Circle_Object is
         procedure Set (Alive : Boolean) is
         begin
            Value  := Alive;
            Is_Set := True;
         end Set;
         entry Get (Alive : out Boolean) when Is_Set is
         begin
            Alive  := Value;
            Is_Set := False;
         end Get;
      end Task_Life_Circle_Object;

      --------------------
      -- Jobserver_Task --
      --------------------

      task body Jobserver_Task is
         Job_Done : Boolean := False;
      begin
         loop
            --  Loop until Job_Done :
            --     Task activated with Alive = False (asked to terminate)
            --     Task encounters and error : Self.Is_Integrous = False
            exit when Job_Done;
            declare
               Alive : Boolean;
            begin
               --  Waits to be activated
               Task_Life_Circle_Object.Get (Alive);
               --  Task have been activated, the Task_State is now "Busy"
               Task_State_Object.Set (Busy);
               --  Task_Token_Status is now Unknown
               Task_Token_Status_Object.Set (Unknown);
               --  raise the Sync_Proc_Task to inform all task data are synced
               Sync_Proc_Task_Object.Set (True);

               Self.Traces.Trace
                 ("Jobserver_Task unlocked ; Alive = "
                  & Alive'Img);

               if Alive then
                  --  Check if the jobserver reading parameters are still
                  --  integrous.
                  if not Self.Is_Integrous then
                     --  Terminate the task
                     Job_Done := True;
                  end if;

                  --  If the read parameters are still integrous
                  if not Job_Done then
                     declare
                        Token : Character := ASCII.NUL;
                     begin
                        --  Read a token
                        if Self.Register (Token) then
                           --  Task_Token_Status is now Available
                           Task_Token_Status_Object.Set (Available);
                           --  Store the token in Token_Value
                           Token_Value_Object.Set (Token);
                        else
                           --  Read failed, Task_Token_Status is now
                           --  Unavailable
                           Task_Token_Status_Object.Set (Unavailable);
                        end if;
                     end;
                  end if;

                  Self.Traces.Trace
                    ("Jobserver_Task ended ; Token_Status = "
                     & Task_Token_Status_Object.Get'Img);
               else
                  --  Asked to be terminated
                  Job_Done := True;
               end if;
            end;
            if not Job_Done then
               --  End of the reading process, Task_State is now Idle
               Task_State_Object.Set (Idle);
            else
               --  The task will be terminated
               Task_State_Object.Set (Terminated);
            end if;
         end loop;
      exception
         when E : others =>
            Self.Traces.Trace
              ("??? : "
               & Ada.Exceptions.Exception_Information (E));
      end Jobserver_Task;

      ------------------
      --  Launch_Task --
      ------------------

      procedure Launch_Task (Self : access Object'Class) is
      begin
         JS_Task := new Jobserver_Task (Self.Self);
      end Launch_Task;

   end Task_Manager;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : Object) is
      pragma Unreferenced (Self);
   begin
      --  Ask for the task to stop
      TM.Task_Life_Circle_Object.Set (Alive => False);
   end Finalize;

   -------------------------
   -- Has_Available_Token --
   -------------------------

   function Has_Unidentified_Token (Self : Object) return Boolean is
   begin
      return Self.Tokens.Contains (Unidentified);
   end Has_Unidentified_Token;

   ----------------
   -- Initialize --
   ----------------

   function Initialize return Object'Class
   is
      Makeflags             : constant String := Value ("MAKEFLAGS", "");
      JS_Auth               : constant String := "--jobserver-auth=";
      Simple_Pipe_Delimiter : constant String := ",";
      Named_Pipe_Delimiter  : constant String := "fifo:";
      Dry_Run_Delimiter     : constant String := "n";

      Idx : Natural := 0;

      Traces : constant GT.Trace_Handle := GT.Create ("JOBSERVER - INIT");
   begin
      Traces.Trace ("Makeflags : " & '"' & Makeflags & '"');

      --  There is no MAKEFLAGS envvar, no jobserver to connect to
      if Makeflags = "" then
         return JS_No_Method.Create;
      end if;

      Idx := Index (Makeflags, " ");
      Idx := Index (Makeflags (Makeflags'First .. Idx - 1), Dry_Run_Delimiter);

      --  "n" detected in the MAKEFLAGS envvar, we should not do anything
      if Idx /= 0 then
         return JS_No_Method.Create (Dry_Run => True);
      end if;

      --  Detect the last "--jobserver-auth=" in MAKEFLAGS envvar
      Idx := Index (Makeflags, JS_Auth, Going => Ada.Strings.Backward);

      --  in GNU make anterior to 4.2 "--jobserver-auth=" did not exist and
      --  another switch was used. We are only compatible with
      --  "--jobserver-auth=".
      if Idx = 0 then
         raise JS_Initialize_Error with "warning: GNU make jobserver MAKEFLAGS"
           & " detected but no --jobserver-auth switch to determine how to "
           & " connect. Make sure you are using GNU make 4.2 or later.";
      end if;

      --  Try all jobserver methods
      for CM in Valid_Jobserver_Method loop
         case CM is
            when JM_Named_Pipe =>
               declare
                  Idx_Tmp  : Natural := Idx;
                  Idx0_Tmp : Natural := 0;
               begin
                  Idx_Tmp := Idx_Tmp + JS_Auth'Length;

                  --  Look for "fifo:" in the MAKEFLAGS envvar to detect a
                  --  Named_Pipe.
                  Idx0_Tmp :=
                    Index (Makeflags, Named_Pipe_Delimiter, From => Idx_Tmp);

                  if Idx0_Tmp /= 0 then
                     --  Get the name of the named pipe
                     Idx_Tmp := Idx0_Tmp + Named_Pipe_Delimiter'Length;
                     Idx0_Tmp := Index (Makeflags, " ", From => Idx_Tmp);

                     if Idx0_Tmp = 0 then
                        Idx0_Tmp := Makeflags'Last;
                     else
                        Idx0_Tmp := Idx0_Tmp - 1;
                     end if;

                     --  If the named pipe exists
                     if Ada.Directories.Exists
                       (Makeflags (Idx_Tmp .. Idx0_Tmp))
                     then
                        --  create a Name_Pipe jobserver object
                        return
                          JS_Pipe.Create (Makeflags (Idx_Tmp .. Idx0_Tmp));
                     else
                        raise JS_Initialize_Error with "Invalid named pipe "
                          & "descriptor to perform a connection to the "
                          & "jobserver. Make sure you prefixed your gprbuild "
                          & "command with a """ & '+' & """ in your makefile.";
                     end if;
                  end if;
               end;

            when JM_Simple_Pipe =>
               declare
                  Idx_Tmp              : Natural := Idx;
                  Idx0_Tmp             : Natural := 0;
                  R, W                 : Integer;
                  R_FD, W_FD           : GOF.File_Descriptor;

                  --  eng/toolchain/gnatcoll-core#116 : No way of importing
                  --  known File_Descriptor
                  function Convert is new Ada.Unchecked_Conversion
                    (Integer, GOF.File_Descriptor);
               begin
                  Idx_Tmp := Idx_Tmp + JS_Auth'Length;

                  --  Look for "," in the MAKEFLAGS envvar to detect a
                  --  Simple_Pipe.
                  Idx0_Tmp :=
                    Index (Makeflags, Simple_Pipe_Delimiter, From => Idx_Tmp);

                  if Idx0_Tmp /= 0 then
                     --  Get both file descriptors of the simple pipe
                     R := Integer'Value (Makeflags (Idx_Tmp .. Idx0_Tmp - 1));

                     Idx_Tmp := Idx0_Tmp + Simple_Pipe_Delimiter'Length;
                     Idx0_Tmp := Index (Makeflags, " ", From => Idx_Tmp);

                     if Idx0_Tmp = 0 then
                        W :=
                          Integer'Value
                            (Makeflags (Idx_Tmp .. Makeflags'Last));
                     else
                        W :=
                          Integer'Value (Makeflags (Idx_Tmp .. Idx0_Tmp - 1));
                     end if;

                     --  Check the validity of the file descriptors
                     if R < 0 or else W < 0 then
                        raise JS_Initialize_Error with "Invalid file "
                          & "descriptor to perform a connection to the "
                          & "jobserver. Make sure you prefixed your gprbuild "
                          & "command with a """ & '+' & """ in your makefile.";
                     end if;

                     R_FD := Convert (R);
                     W_FD := Convert (W);

                     --  Check if the file descriptors aren't regular files
                     if GOS.Is_File (GOS.Fstat (R_FD))
                       or else GOS.Is_File (GOS.Fstat (W_FD))
                     then
                        raise JS_Initialize_Error with "Unable to connect to "
                          & "the jobserver. Make sure you prefixed your "
                          & "gprbuild command with a """ & '+' & """ in your "
                          & "makefile.";
                     end if;

                     --  Create a Simple_Pipe jobserver object
                     return JS_Pipe.Create (R_FD, W_FD);
                  end if;
               end;

            when JM_Semaphore =>
               declare
                  Idx_Tmp  : Natural := Idx;
                  Idx0_Tmp : Natural := 0;
               begin
                  --  Simply read the semaphore name after the
                  --  "--jobserver-auth=" switch.
                  Idx_Tmp := Idx_Tmp + JS_Auth'Length;
                  Idx0_Tmp := Index (Makeflags, " ", From => Idx_Tmp);

                  if Idx0_Tmp = 0 then
                     Idx0_Tmp := Makeflags'Last;
                  else
                     Idx0_Tmp := Idx0_Tmp - 1;
                  end if;

                  --  Create a Semaphore jobserver object
                  return
                    (JS_Semaphore.Create (Makeflags (Idx_Tmp .. Idx0_Tmp)
                     & ASCII.NUL));
               end;
         end case;
      end loop;

      --  Create a dummy if at this point no other jobserver object was created
      return JS_No_Method.Create;
   end Initialize;

   --------------------
   -- Preorder_Token --
   --------------------

   function Preorder_Token (Self : in out Object'Class) return Boolean
   is
      use type TM.Task_State;
      use type TM.Token_Process_State;
      use type TM.Task_Token_Status;

      Preorder_Condition : constant Boolean :=
                             (TM.Token_Process_State_Object.Get = TM.Idle
                              or else
                                (TM.Sync_Proc_Task_Object.Synced
                                 and then
                                 TM.Task_State_Object.Get = TM.Idle
                                 and then
                                 TM.Task_Token_Status_Object.Get =
                                   TM.Unavailable));
      Result             : Boolean;
   begin
      --  First call to Preorder_Token, launch the token reading task
      if not Self.Task_Launched then
         Self.Self := Self'Unchecked_Access;
         TM.Launch_Task (Self.Self);
         Self.Task_Launched := True;
      end if;

      if TM.Task_State_Object.Get = TM.Terminated then
         raise JS_Access_Error with "Cannot preorder a token, the token "
           & "reading task is terminated";
      end if;

      Self.Traces.Trace ("Preorder_Token");
      Self.Traces.Trace
        ("   [ Proc ] " & TM.Token_Process_State_Object.Get'Img & " ; Auth = "
         & Boolean'Image (Preorder_Condition));
      Self.Traces.Trace
        ("   [ Task ] " & TM.Task_State_Object.Get'Img & " - "
         & TM.Task_Token_Status_Object.Get'Img);

      --  If :
      --  Token_Process_State = "Idle" (We are not already asking for a token)
      --  => Try
      --  Or :
      --  Sync_Proc_Task    = True          (had to read a token)
      --  Task_State        = "Idle"        (finished reading a token)
      --  Task_Token_Status = "Unavailable"
      --  (jobserver did not gave the permission)
      --  => Retry
      if Preorder_Condition
      then
         --  Reset the Sync Proc/Task flag
         TM.Sync_Proc_Task_Object.Set (False);
         --  Ask for the task to read a token
         TM.Task_Life_Circle_Object.Set (Alive => True);
         --  Switch the Token_Process_State to "Pending"
         TM.Token_Process_State_Object.Set (TM.Pending);
         Self.Traces.Trace
           ("   [ Proc ] New process state : "
            & TM.Token_Process_State_Object.Get'Img);
      end if;

      --  If :
      --  Sync_Proc_Task    = True        (had to read a token)
      --  Task_State        = "Idle"      (finished reading a token)
      --  Task_Token_Status = "Available" (jobserver gave the permission)
      Result := (TM.Sync_Proc_Task_Object.Synced
                 and then TM.Task_State_Object.Get = TM.Idle
                 and then TM.Task_Token_Status_Object.Get = TM.Available);

      --  Store the not-yet identified token if we successfuly have an
      --  available token.
      if Result then
         Self.Tokens.Insert (Unidentified, TM.Token_Value_Object.Get);
         Self.Busy_Count := 0;
      else
         Self.Busy_Count := Self.Busy_Count + 1;
      end if;

      Self.Traces.Trace
        ("   [ Proc ] Token availability : " & Result'Img);

      return Result;
   end Preorder_Token;

   --------------------
   -- Register_Token --
   --------------------

   function Register_Token
     (Self    : in out Object'Class;
      Proc_Id : GOP.Process_Handle) return Boolean
   is
      use type TM.Task_Token_Status;

      Key : constant Process_Handle_Identifier := (True, Proc_Id);
   begin
      Self.Traces.Trace ("Register_Token_Id");
      Self.Traces.Trace
        ("   [ Proc ] " & TM.Token_Process_State_Object.Get'Img);
      Self.Traces.Trace
        ("   [ Task ] " & TM.Task_State_Object.Get'Img & " - "
         & TM.Task_Token_Status_Object.Get'Img);

      --  If the token is available
      if TM.Task_Token_Status_Object.Get = TM.Available then
         declare
            Token : constant Character := Self.Tokens.Element (Unidentified);
            --  Read it from the stored pre-ordered unidentified token
         begin
            --  Delete the unidentified token
            Self.Tokens.Delete (Unidentified);

            --  Store the token with the identified process id
            Self.Tokens.Insert (Key, Token);

            --  Reset the token process state to "Idle"
            TM.Token_Process_State_Object.Set (TM.Idle);

            Self.Traces.Trace
              ("   [ Proc ] Proc_Id :"
               & Proc_Id'Img & " ; Token : " & Token);
            Self.Traces.Trace
              ("   [ Proc ] New process state : "
               & TM.Token_Process_State_Object.Get'Img);

            return True;
         end;
      end if;

      return False;
   end Register_Token;

   ----------------------
   -- Unregister_Token --
   ----------------------

   function Unregister_Token
     (Self       : in out Object'Class;
      Proc_Id    : GOP.Process_Handle := GOP.Invalid_Handle;
      Identified : Boolean            := True) return Boolean
   is
      Key : constant Process_Handle_Identifier := (Identified, Proc_Id);
   begin
      Self.Traces.Trace ("Unregister_Token");

      if not Self.Release (Self.Tokens.Element (Key)) then
         return False;
      end if;

      Self.Traces.Trace
        ("   [ Proc ] Proc_Id :"
         & Proc_Id'Img & " ; Token : " & Self.Tokens.Element (Key));

      Self.Tokens.Delete (Key);
      return True;
   end Unregister_Token;

   -----------------------
   -- Unregister_Tokens --
   -----------------------

   procedure Unregister_Tokens (Self : in out Object'Class) is
      Cursor : Token_Id.Cursor;
   begin
      Self.Traces.Trace ("Unregister_Tokens");

      while not Self.Tokens.Is_Empty loop
         Cursor := Self.Tokens.First;

         if not Self.Release (Token_Id.Element (Position => Cursor)) then
            raise JS_Access_Error with "Failed to release a token";
         end if;

         Self.Traces.Trace
           ("Unregister remaining token for"
            & Token_Id.Key (Position => Cursor).Process_Handle'Img);

         Self.Tokens.Delete (Position => Cursor);
      end loop;
   end Unregister_Tokens;

end GPR2.Build.Jobserver;
