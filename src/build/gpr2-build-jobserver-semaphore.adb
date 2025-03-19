--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Jobserver.Semaphore is

   function Create (Name : String) return Object is
      JS : constant Object := (Semaphore_Handle => 0,
                               others           => <>);
   begin
      JS.Traces.Trace ("Connection method : " & JS.Kind'Img);
      return JS;
   end Create;

   --------------
   -- Register --
   --------------

   overriding
   function Register (Self : Object; Char : out Character) return Boolean is
   begin
      --  Dummy : No semaphore implementation on UNIX
      Char := ASCII.NUL;
      return False;
   end Register;

   -------------
   -- Release --
   -------------

   overriding
   function Release (Self : Object; Char : Character) return Boolean is
   begin
      --  Dummy : No semaphore implementation on UNIX
      return False;
   end Release;

end GPR2.Build.Jobserver.Semaphore;
