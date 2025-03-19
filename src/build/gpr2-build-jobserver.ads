--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.OS.FS;
with GNATCOLL.OS.Process;
with GNATCOLL.OS.Stat;
with GNATCOLL.Traces;

package GPR2.Build.Jobserver is

   JS_Access_Error     : exception;
   --  Error exception raised when jobserver's read or write fails
   JS_Initialize_Error : exception;
   --  Error exception raised when jobserver's initialization fails

   package GOF renames GNATCOLL.OS.FS;
   package GOP renames GNATCOLL.OS.Process;
   package GOS renames GNATCOLL.OS.Stat;

   type Object is abstract tagged private;

   type Jobserver_Method is
     (JM_Undefined, JM_Named_Pipe, JM_Simple_Pipe, JM_Semaphore);

   function Initialize return Object'Class;
   --  Initialize a Jobserver object, the resulting object contains the proper
   --  way to interface with the current jobserver. Otherwise a
   --  Jobserver.No_Method dummy object is returned.

   function Register
     (Self : Object;
      Char : out Character) return Boolean is abstract;
   --  Used to properly read into the jobserver file depending on the object
   --  implementation.

   function Release
     (Self : Object;
      Char : Character) return Boolean is abstract;
   --  Used to properly write into the jobserver file depending on the object
   --  implementation.

   function Is_Integrous (Self : Object) return Boolean is abstract;
   --  Used to check if the object implementation is integrous at the moment
   --  of the call.

   function Preorder_Token (Self : in out Object'Class) return Boolean;
   --  Read a token from the jobserver :
   --     True  : A token is available, a process can be spawned.
   --     Flase : No available token, no process must be spawned.
   --  At this point the token is unidentified, meaning we took a token but it
   --  is not associated to a process yet.
   --  In the case of actually spawning a process, Register_Token should be
   --  called in order to associate the two.

   function Register_Token
     (Self : in out Object'Class; Proc_Id : GOP.Process_Handle) return Boolean;
   --  Associate the current token to the process id.
   --     True  : The token has properly been registered to the process id.
   --     False : Register_Token has been called without any token available.

   function Unregister_Token
     (Self       : in out Object'Class;
      Proc_Id    : GOP.Process_Handle := GOP.Invalid_Handle;
      Identified : Boolean            := True) return Boolean;
   --  Write the process id associated token back to the jobserver.
   --  Calling Unregister_Token (Identified => False) allows to write and
   --  unidentified token back to the jobserver. This can be useful when we
   --  pre-ordered a token and the process we tried to launch failed for some
   --  reason.

   procedure Unregister_Tokens (Self : in out Object'Class);
   --  This is a safeguard, allows to unregister all registered tokens, the
   --  unidentified token included.

   function Dry_Run (Self : Object) return Boolean;
   --  Is the jobserver in a "dry_run" mode

   function Active (Self : Object) return Boolean;
   --  Is the jobserver active, meaning we have a proper implementation of it
   --  available.

   function Has_Registered_Tokens (Self : Object) return Boolean;
   --  Do we have registered tokens available

   function Has_Unidentified_Token (Self : Object) return Boolean;
   --  Do we have an unidentified token available

   procedure Finalize (Self : Object);
   --  Finalize the token reading task

   function Busy (Self : Object) return Boolean;
   --  Returns whether or not Self.Busy_Count > Max_Busy_Count

   function Kind (Self : Object) return Jobserver_Method;
   --  Returns what kind of jobserver is used

private

   package GT renames GNATCOLL.Traces;

   subtype Valid_Jobserver_Method
     is Jobserver_Method range JM_Named_Pipe .. JM_Semaphore;

   type Process_Handle_Identifier is record
      Identified     : Boolean;
      Process_Handle : GOP.Process_Handle;
   end record;

   Unidentified   : constant Process_Handle_Identifier :=
                      (Identified     => False,
                       Process_Handle => GOP.Invalid_Handle);

   Max_Busy_Count : constant := 20;

   function Hash
     (Key : Process_Handle_Identifier) return Ada.Containers.Hash_Type;
   function Hash
     (Key : Process_Handle_Identifier) return Ada.Containers.Hash_Type is
     (if Key.Identified
      then Ada.Containers.Hash_Type (Key.Process_Handle)
      else 0);

   package Token_Id is new Ada.Containers.Indefinite_Hashed_Maps
     (Process_Handle_Identifier, Character, Hash, "=");

   type Object is abstract tagged record
      Self          : access Object := null;
      --  Self reference in order to initalize the token reading task to
      --  properly dispatch the jobserver reading implementation.

      Task_Launched : Boolean := False;
      --  Is the token reading task launched

      Busy_Count    : Integer := 0;
      --  Counts the number of time a preorder resulted in False before
      --  returning True

      Kind          : Jobserver_Method := JM_Undefined;
      --  Jobserver kind

      Tokens        : Token_Id.Map     := Token_Id.Empty_Map;
      --  Tokens / Process id map

      Traces        : GT.Trace_Handle  := GT.Create ("JOBSERVER");
   end record;

   function Dry_Run (Self : Object) return Boolean is (False);

   function Active (Self : Object) return Boolean is
     (Self.Kind in Valid_Jobserver_Method);

   function Has_Registered_Tokens (Self : Object) return Boolean is
     (not Self.Tokens.Is_Empty);

   function Busy (Self : Object) return Boolean is
     (Self.Busy_Count > Max_Busy_Count);

   function Kind (Self : Object) return Jobserver_Method is (Self.Kind);

end GPR2.Build.Jobserver;
