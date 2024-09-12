--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--
--  Console reporter that outputs messages to standard streams.

with GPR2.Message; use GPR2.Message;

package GPR2.Reporter.Console is

   type Object is new GPR2.Reporter.Object with private;

   overriding procedure Internal_Report
     (Self    : in out Object;
      Message : GPR2.Message.Object);

   overriding
   function Verbosity (Self : Object) return Verbosity_Level;

   function Create (Verbosity           : Verbosity_Level := Regular;
                    Use_Full_Pathname   : Boolean := False;
                    Level_Report_Format : Level_Format := Long) return Object;
   --  Create a reporter with the specified properties.
   --  * Verbosity: Determines which messages to display.
   --  * Use_Full_Pathname: Specifies whether the reporter uses full path names
   --    when displaying GPR2 messages.
   --  * Level_Report_Format: Defines the format for GPR2 message levels.

   function Full_Pathname (Self : in out Object) return Boolean;
   --  Returns True if full path names are used by the reporter to display
   --  gpr2 messages.

   function Level_Report_Format (Self : in out Object) return Level_Format;
   --  Returns the level format used by the reporter to display gpr2 messages

   procedure Set_Verbosity (Self : in out Object; Verbosity : Verbosity_Level);
   --  Sets the verbosity of the reporter

   procedure Set_Full_Pathname
     (Self : in out Object; Use_Full_Pathname : Boolean);
   --  Indicate whether full path names should be used in GPR2 messages by
   --  the reporter.

   procedure Set_Level_Report_Format
     (Self : in out Object; Level_Report_Format : Level_Format);
   --  Set the level format used by the reporter to display gpr2 messages

private

   type Object is new GPR2.Reporter.Object with record
      Full_Path : Boolean := False;
      --  Indicates whether full path names should be used in GPR2 messages

      Level_Fmt : Level_Format := Long;
      --  Level format used in GPR2 messages

      Verbosity : GPR2.Reporter.Verbosity_Level := Regular;
      --  Used to determine whether a message should be displayed
   end record;

   function Full_Pathname (Self : in out Object) return Boolean is
     (Self.Full_Path);

   function Level_Report_Format (Self : in out Object) return Level_Format is
     (Self.Level_Fmt);

end GPR2.Reporter.Console;
