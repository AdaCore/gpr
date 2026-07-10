--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Reports log messages

with GPR2.Message;
with GPR2.Log;

package GPR2.Reporter is

   type Object is abstract tagged null record;
   --  Used to report messages to the end user. This allows customisation
   --  of message reporting depending on the libgpr2 user constraints.

   type Verbosity_Level is
     (Quiet, No_Warnings, Regular, Verbose, Very_Verbose);

   type User_Verbosity_Level is
     (Unset, Quiet, Important_Only, Regular, Verbose);

   procedure Report
     (Self           : in out Object'Class;
      Messages       : GPR2.Log.Object;
      Warn_If_Errors : Boolean := False);
  --  Report messages from the provided log based on the reporter's verbosity.
  --  If Warn_If_Errors is unset and the log contains error messages, then the
  --  warnings are not displayed.

   procedure Report
     (Self    : in out Object'Class;
      Message : GPR2.Message.Object;
      Binary  : Boolean := False);
   --  Report the message based on the reporter's verbosity.
   --  If Binary is set, the message content is written verbatim (binary mode)
   --  so that any end-of-line already present in it is preserved as-is; see
   --  Internal_Report for the details.

   procedure Report
     (Self      : in out Object'Class;
      Message   : String;
      To_Stderr : Boolean := False;
      Level     : GPR2.Message.User_Level_Value := GPR2.Message.Regular;
      Binary    : Boolean := False);
   --  A wrapper around the Report procedure that creates an end-user
   --  GPR2.Message.Object and reports it.
   --  Binary is forwarded to the Report procedure above.

   function Verbosity (Self : Object) return Verbosity_Level is abstract;
   --  Obtain the reporter's verbosity level

   function User_Verbosity (Self : Object) return User_Verbosity_Level;

   --------------------------
   -- INTERNAL SUBPROGRAMS --
   --------------------------

   procedure Internal_Report
     (Self    : in out Object;
      Message : GPR2.Message.Object;
      Binary  : Boolean := False) is abstract;
   --  Internal message reporting function to be implemented by the reporter.
   --  When Binary is set, a reporter that writes to a file descriptor is
   --  expected to emit the message content in binary mode
   --  (no end-of-line translation on windows), so that content that already
   --  contains "\r\n" sequences is not altered.

private

   function User_Verbosity (Self : Object) return User_Verbosity_Level is
     (Unset);

end GPR2.Reporter;
