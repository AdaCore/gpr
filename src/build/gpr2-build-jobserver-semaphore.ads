--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package GPR2.Build.Jobserver.Semaphore is

   type Object is new Jobserver.Object with private;

   function Create (Name : String) return Object;

   overriding function Register
     (Self : Object;
      Char : out Character) return Boolean;
   --  Used to properly read into the jobserver file depending on the object
   --  implementation.

   overriding function Release
     (Self : Object;
      Char : Character) return Boolean;
   --  Used to properly write into the jobserver file depending on the object
   --  implementation.

   overriding function Is_Integrous (Self : Object) return Boolean;
   --  Used to check if the object implementation is integrous at the moment
   --  of the call.

private

   type Object is new GPR2.Build.Jobserver.Object with record
      Semaphore_Handle : Integer;
      --  Semaphore to read from and write to
   end record;

   overriding function Is_Integrous (Self : Object) return Boolean is (True);
   --  No particular reason to continuously check if the semaphore is integrous

end GPR2.Build.Jobserver.Semaphore;
