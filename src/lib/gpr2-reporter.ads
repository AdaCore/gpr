--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Reports log messages

with GPR2.Message;
with GPR2.Log;

package GPR2.Reporter is

   type Object is interface;
   --  Used to report messages to the end user. This allows customisation
   --  of message reporting depending on the libgpr2 user constraints.

   type Verbosity_Level is
     (Quiet, No_Warnings, Regular, Verbose, Very_Verbose);

   procedure Report
     (Self           : in out Object'Class;
      Messages       : GPR2.Log.Object;
      Warn_If_Errors : Boolean := False);
  --  Report messages from the provided log based on the reporter's verbosity.
  --  If Warn_If_Errors is unset and the log contains error messages, then the
  --  warnings are not displayed.

   procedure Report
     (Self : in out Object'Class; Message : GPR2.Message.Object);
   --  Report the message based on the reporter's verbosity

   procedure Report (Self : in out Object'Class; Message : String);
   --  A wrapper around the Report procedure that creates an end-user
   --  GPR2.Message.Object and reports it.

   function Verbosity (Self : Object) return Verbosity_Level is abstract;
   --  Obtain the reporter's verbosity level

   --------------------------
   -- INTERNAL SUBPROGRAMS --
   --------------------------

   procedure Internal_Report
     (Self : in out Object; Message : GPR2.Message.Object) is abstract;
   --  Internal message reporting function to be implemented by the reporter.

end GPR2.Reporter;
