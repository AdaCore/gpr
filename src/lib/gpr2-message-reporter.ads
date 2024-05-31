--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  Reports log messages

package GPR2.Message.Reporter is

   type Object is interface;
   --  Used to report GPR2 messages to the end user. This allows customisation
   --  of message reporting depending on the libgpr2 user constraints.
   --
   --  Message reporter is global to libgpr2, and defaults to text reporter.
   --
   --  The default reporter can be configured via a call to
   --  Config_Default_Reporter. It can also be replaced by your own
   --  variant via a call to `Register_Reporter`

   procedure Report (Self : Object; Message : GPR2.Message.Object) is abstract;
   --  Used to report Message, to be implemnted by the reporter.

   procedure Report
     (Self     : Object;
      Message  : String) is abstract;
   --  Used to report information messages as string, such as usage if
   --  implicit projects or generation of config files.

   procedure Register_Reporter (Instance : Object'Class)
     with Post => Active_Reporter = Instance;
   --  Use a specific reporter "Instance" instead of the default console
   --  reporter.

   function Active_Reporter return Object'Class;
   --  Retrieve the active reporter

   procedure Configure_Default_Reporter
     (Use_Full_Pathname   : Boolean := False;
      Level_Report_Format : Level_Format := Long);
   --  Change the default console reporter configuration.

private

   type Text_Reporter is new Object with record
      Full_Path : Boolean := False;
      Level_Fmt : Level_Format := Long;
   end record;

   overriding procedure Report
     (Self    : Text_Reporter;
      Message : GPR2.Message.Object);

   overriding procedure Report
     (Self     : Text_Reporter;
      Message  : String);

end GPR2.Message.Reporter;
