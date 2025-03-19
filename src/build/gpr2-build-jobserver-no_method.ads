--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package GPR2.Build.Jobserver.No_Method is

   type Object is new Jobserver.Object with private;

   function Create (Dry_Run : Boolean := False) return Object;

   overriding function Register
     (Self : Object;
      Char : out Character) return Boolean;
   --  Dummy : Should not be called

   overriding function Release
     (Self : Object;
      Char : Character) return Boolean;
   --  Dummy : Should not be called

   overriding function Dry_Run (Self : Object) return Boolean;
   --  Override : return the actual value of "dry run" mode of the jobserver

   overriding function Is_Integrous (Self : Object) return Boolean;
   --  Dummy : Should not be called

private

   type Object is new GPR2.Build.Jobserver.Object with record
      Dry_Run : Boolean := False;
      --  Is the jobserver in "dry run" mode
   end record;

   overriding
   function Dry_Run (Self : Object) return Boolean is (Self.Dry_Run);

   overriding function Is_Integrous (Self : Object) return Boolean is (True);
   --  Dummy : Should not be called

end GPR2.Build.Jobserver.No_Method;
