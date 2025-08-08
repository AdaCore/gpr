--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Vectors;
private with Ada.Finalization;

private with GPR2.Build.Jobserver_Protocol;

package GPR2.Build.Jobserver is

   pragma Elaborate_Body;

   type Object is tagged limited private;

   function Is_Available (Self : Object) return Boolean;
   --  Tell if a make job server is available

   procedure Initialize_Protocol (Self : in out Object);
   --  Initialize the jobserver protocol

   function Dry_Run (Self : Object) return Boolean;
   --  make was called with -n: we shouln't execute anything

   function Request_Token (Self : in out Object) return Boolean
     with Pre => Self.Is_Available;
   --  Read a token from the jobserver :
   --     True  : A token is available, a process can be spawned.
   --     Flase : No available token, no process must be spawned.
   --  At this point the token is unidentified, meaning we took a token but it
   --  is not associated to a process yet.
   --  In the case of actually spawning a process, Register_Token should be
   --  called in order to associate the two.

   procedure Release_Token (Self : in out Object);
   --  Releases a token that was given by Request_Token.

   function Has_Protocol_Error (Self : Object) return Boolean;
   --  Returns whether or not there is an internal error.
   --  This could be and error at the initialization or the jobserver or during
   --  protocol usage.

private

   package Protocol_Holder is new Ada.Containers.Indefinite_Holders
     (Jobserver_Protocol.Object'Class, Jobserver_Protocol."=");

   package Token_Vector is new Ada.Containers.Vectors
     (Positive, Character);

   task type Token_Reader (Self : access Object) is
      pragma Priority (System.Default_Priority + 1);
   end Token_Reader;

   type Wake_Up_Reason is
     (Terminate_Task,
      Fetch_Token);

   protected type Token_Holder is
      entry Wake_Up (Reason : out Wake_Up_Reason);
      --  Tell the Token_Reader task to go fetch a new token

      procedure Ask_Termination;
      --  Tell the task to terminate

      procedure Get
        (Token     : out Character;
         Available : out Boolean);
      --  If a token is available, then Token will be set to its value and
      --  available will be true, else a request will be done for a new token
      --  and available will be False.

      entry Set (Char : Character; Validity : Boolean);
      --  Used by the Token_Reader to store a newly available token and its
      --  validity.

   private
      Token         : Character := ASCII.NUL;
      Is_Available  : Boolean := False;
      Is_Processing : Boolean := False;
      Has_Event     : Boolean := False;
      Event         : Wake_Up_Reason;
   end Token_Holder;

   type Token_Reader_Access is access Token_Reader;

   type Object is new Ada.Finalization.Limited_Controlled with record
      Self          : access Object;
      --  Allows easy self-referencing

      Dry_Run       : Boolean := False;
      --  make was called with the -n switch

      Current_Token : Token_Holder;

      Reader        : Token_Reader_Access;
      --  The task responsible for waiting for a new token

      Task_Launched : Boolean := False with Atomic;
      --  Is the token reading task launched

      Protocol      : Protocol_Holder.Holder;

      Tokens        : Token_Vector.Vector;
      --  Tokens / Process id map

      Error         : Boolean := False;
   end record;

   overriding procedure Finalize (Self : in out Object);

   function Dry_Run (Self : Object) return Boolean is (Self.Dry_Run);

   function Is_Available (Self : Object) return Boolean is
     (not Self.Protocol.Is_Empty and then Self.Protocol.Element.Is_Available);

   function Has_Protocol_Error (Self : Object) return Boolean is (Self.Error);

end GPR2.Build.Jobserver;
