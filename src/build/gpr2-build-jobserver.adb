--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with GNAT.Regpat;

with GNATCOLL.Traces;

with GPR2.Build.Jobserver_Protocol.Pipe;
with GPR2.Build.Jobserver_Protocol.Semaphore;

package body GPR2.Build.Jobserver is

   Traces : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create
                ("GPR.BUILD.JOBSERVER", GNATCOLL.Traces.Off);

   ------------------
   -- Token_Reader --
   ------------------

   task body Token_Reader is
      Reason : Wake_Up_Reason;
   begin
      Self.Task_Launched := True;

      --  Note: don't use traces in the core processing loop, this lowers
      --  the priority of the task for buffered output

      loop
         Self.Current_Token.Wake_Up (Reason);

         exit when Reason = Terminate_Task;

         --  Check if the jobserver reading parameters are still
         --  integrous.
         if not Self.Protocol.Element.Is_Available then
            --  Terminate the task
            exit;
         end if;

         declare
            Token : Character := ASCII.NUL;
         begin
            --  Read a token
            if Self.Protocol.Element.Get_Token (Token) then
               Self.Current_Token.Set (Token);
            end if;
         end;
      end loop;

      Traces.Trace ("task terminated");
      Self.Task_Launched := False;
   end Token_Reader;

   ------------------
   -- Token_Holder --
   ------------------

   protected body Token_Holder is
      -------------
      -- Wake_Up --
      -------------

      entry Wake_Up (Reason : out Wake_Up_Reason)
        when Has_Event is
      begin
         Has_Event := False;
         Reason    := Event;
      end Wake_Up;

      ---------------------
      -- Ask_Termination --
      ---------------------

      procedure Ask_Termination is
      begin
         Event     := Terminate_Task;
         Has_Event := True;
      end Ask_Termination;

      ---------
      -- Get --
      ---------

      procedure Get
        (Token     : out Character;
         Available : out Boolean)
      is
      begin
         Available := Is_Set;

         if Is_Set then
            Token := Token_Holder.Token;
            Is_Set := False;
         elsif not Is_Processing then
            Traces.Trace ("Will actually ask a token");
            Event         := Fetch_Token;
            Has_Event     := True;
            Is_Processing := True;
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      entry Set (Char : Character)
        when not Is_Set is
      begin
         Token         := Char;
         Is_Set        := True;
         Is_Processing := False;
      end Set;
   end Token_Holder;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Object) is
      Token : Character;
      Avail : Boolean;

      procedure Free is new
        Ada.Unchecked_Deallocation (Token_Reader, Token_Reader_Access);
   begin
      if Self.Task_Launched then
         Self.Current_Token.Ask_Termination;
      end if;

      if not Self.Protocol.Is_Empty then
         for J in Self.Tokens.First_Index + 1 .. Self.Tokens.Last_Index loop
            Self.Protocol.Element.Release_Token (Self.Tokens (J));
         end loop;

         Self.Tokens.Clear;

         Self.Current_Token.Get (Token, Avail);

         if Avail then
            Self.Protocol.Element.Release_Token (Token);
         end if;

         Self.Protocol.Reference.Finalize;
         Self.Protocol.Clear;
      end if;

      if Self.Reader /= null then
         Free (Self.Reader);
      end if;
   end Finalize;

   -------------------------
   -- Initialize_Protocol --
   -------------------------

   procedure Initialize_Protocol (Self : in out Object)
   is
      Makeflags            : constant String := Value ("MAKEFLAGS", "");
      JS_Auth              : constant String := "--jobserver-auth=";
      Named_Pipe_Delimiter : constant String := "fifo:";
      Dry_Run_Delimiter    : constant String := "n";
      Simple_Pipe_Re       : constant GNAT.Regpat.Pattern_Matcher :=
                                GNAT.Regpat.Compile ("(-?[0-9]+),(-?[0-9]+)");
      Simple_Pipe_Match    : GNAT.Regpat.Match_Array (1 .. 2);

      Idx                  : Natural := 0;
   begin
      pragma Assert
        (not Self.Task_Launched,
         "Internal error: jobserver initialized twice");

      Self.Self := Self'Unrestricted_Access;

      Traces.Trace ("Makeflags : " & '"' & Makeflags & '"');

      --  There is no MAKEFLAGS envvar, no jobserver to connect to
      if Makeflags = "" then
         return;
      end if;

      Idx := Index (Makeflags, " ");
      Idx := Index (Makeflags (Makeflags'First .. Idx - 1), Dry_Run_Delimiter);

      --  "n" detected in the MAKEFLAGS envvar, we should not do anything
      Self.Dry_Run := Idx /= 0;

      if Self.Dry_Run then
         return;
      end if;

      --  Detect the last "--jobserver-auth=" in MAKEFLAGS envvar
      Idx := Index (Makeflags, JS_Auth, Going => Ada.Strings.Backward);

      --  in GNU make anterior to 4.2 "--jobserver-auth=" did not exist and
      --  another switch was used. We are only compatible with
      --  "--jobserver-auth=".
      if Idx = 0 then
         Traces.Trace
           ("MAKEFLAGS doesn't have " & JS_Auth & " in """ & Makeflags & '"');
         return;
      end if;

      --  Try all jobserver methods
      declare
         Idx_End : constant Natural :=
                     Index (Makeflags (Idx .. Makeflags'Last), " ");
         Param   : constant String :=
                     Makeflags
                       (Idx + JS_Auth'Length ..
                          (if Idx_End > 0 then Idx_End else Makeflags'Last));

         Match   : constant Boolean :=
                     GNAT.Regpat.Match (Simple_Pipe_Re, Param);
      begin
         --  Check named pipe
         if GNATCOLL.Utils.Starts_With (Param, Named_Pipe_Delimiter) then
            Self.Error := True;

         --  Check simple pipe
         elsif Match then
            GNAT.Regpat.Match (Simple_Pipe_Re, Param, Simple_Pipe_Match);

            declare
               R        : constant Integer :=
                            Integer'Value
                              (Param
                                 (Simple_Pipe_Match (1).First ..
                                    Simple_Pipe_Match (1).Last));
               W        : constant Integer :=
                            Integer'Value
                              (Param
                                 (Simple_Pipe_Match (2).First ..
                                    Simple_Pipe_Match (2).Last));
               Protocol : Jobserver_Protocol.Pipe.Object;
            begin
               Protocol := Jobserver_Protocol.Pipe.Initialize (R, W);
               Self.Protocol.Replace_Element (Protocol);
            end;

         elsif GPR2.On_Windows then
            declare
               Protocol : constant Jobserver_Protocol.Semaphore.Object :=
                            Jobserver_Protocol.Semaphore.Initialize (Param);
            begin
               Self.Protocol.Replace_Element (Protocol);
            end;
         end if;
      end;

      if not Self.Protocol.Is_Empty
        and then not Self.Protocol.Element.Is_Available
      then
         Self.Protocol.Reference.Finalize;
         Self.Protocol.Clear;

         return;
      end if;
   end Initialize_Protocol;

   -------------------
   -- Release_Token --
   -------------------

   procedure Release_Token (Self : in out Object) is
      Token : constant Character := Self.Tokens.Last_Element;
   begin
      Self.Tokens.Delete_Last;
      Traces.Trace ("Release_Token:" &
                      Self.Tokens.Length'Image & " remaining");

      if not Self.Tokens.Is_Empty then
         --  Don't release the last token, it's the one that corresponds to
         --  the current application
         Self.Protocol.Element.Release_Token (Token);
      end if;
   end Release_Token;

   -------------------
   -- Request_Token --
   -------------------

   function Request_Token (Self : in out Object) return Boolean is
      Token : Character;
      Avail : Boolean;
   begin

      if Self.Tokens.Is_Empty then
         Traces.Trace ("Request_Token: first token");
         --  First token is always available
         Self.Tokens.Append (ASCII.NUL);

         return True;
      end if;

      if not Self.Task_Launched then
         Self.Reader := new Token_Reader (Self.Self);
      end if;

      for J in 1 .. 2 loop
         Self.Current_Token.Get (Token, Avail);

         if Avail then
            Self.Tokens.Append (Token);
            Traces.Trace
              ("Request_Token: have one," &
                 Self.Tokens.Length'Image & " in total");
            exit;
         else
            Traces.Trace ("Request_Token, none for now (" & J'Image & " try)");
         end if;
      end loop;

      return Avail;
   end Request_Token;

end GPR2.Build.Jobserver;
