--
--  Copyright (C) 2024-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package GPR2.Build.Jobserver_Protocol.Semaphore is

   type Object is new Jobserver_Protocol.Object with private;

   overriding function Initialize (Param : String) return Object;
   --  Initialize a semaphore

   overriding function Is_Available (Self : Object) return Boolean;

   overriding function Get_Token
     (Self  : Object;
      Token : out Character) return Boolean;

   overriding procedure Release_Token
     (Self  : Object;
      Token : Character);

   overriding procedure Finalize (Self : in out Object);

private

   type Object is new Jobserver_Protocol.Object with record
      Handle : Integer := 0;
      --  The semaphore handle
   end record;

end GPR2.Build.Jobserver_Protocol.Semaphore;
