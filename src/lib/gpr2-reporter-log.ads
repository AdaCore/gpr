--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--
--  Log reporter that stores messages in an internal log

with GPR2.Message; use GPR2.Message;

package GPR2.Reporter.Log is

   type Object is new GPR2.Reporter.Object with private;

   overriding procedure Internal_Report
     (Self    : in out Object;
      Message : GPR2.Message.Object);

   overriding
   function Verbosity (Self : Object) return Verbosity_Level;

   function Create (Verbosity : Verbosity_Level := Regular) return Object;
   --  Create a reporter with the specified verbosity which is used to.
   --  determines which messages to store.

   procedure Set_Verbosity (Self : in out Object; Verbosity : Verbosity_Level);
   --  Sets the verbosity of the reporter

   function Log (Self : Object) return GPR2.Log.Object;
   --  Return a copy of the internal log. This log contains message that
   --  have been stored according to the reporter verbosity.

   procedure Clear_Log (Self : in out Object);
   --  Clear the internal reporter log

private

   type Object is new GPR2.Reporter.Object with record
      Log       : GPR2.Log.Object;
      --  Messages storage location

      Verbosity : GPR2.Reporter.Verbosity_Level := Regular;
      --  Used to determine whether a message should be displayed
   end record;

   function Log (Self : Object) return GPR2.Log.Object is
     (Self.Log);

end GPR2.Reporter.Log;
