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

   procedure Report (Self : Object'Class; Messages : GPR2.Log.Object);
  --  Report messages from the provided log based on the reporter's verbosity.
  --  If the log contains error messages and the Error flag is set to True,
  --  only errors will be reported.

   procedure Report (Self : Object'Class; Message : GPR2.Message.Object);
   --  Report the message based on the reporter's verbosity

   procedure Report (Self : Object'Class; Message : String);
   --  Report the provided string if the reporter's verbosity is not set to
   --  Quiet. This is used to report informational messages, such as the use
   --  of implicit projects or the generation of configuration files.

   function Verbosity (Self : Object) return Verbosity_Level is abstract;
   --  Obtain the reporter's verbosity level

   --------------------------
   -- INTERNAL SUBPROGRAMS --
   --------------------------

   procedure Internal_Report
     (Self : Object; Message : GPR2.Message.Object) is abstract;
   --  Internal message reporting function to be implemented by the reporter.

   procedure Internal_Report
     (Self : Object; Message : String) is abstract;
   --  Internal string message reporting function to be implemented by the
   --  reporter.

end GPR2.Reporter;
