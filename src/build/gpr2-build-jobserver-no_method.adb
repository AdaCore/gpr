--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Jobserver.No_Method is

   function Create (Dry_Run : Boolean := False) return Object is
      JS : constant Object := (Dry_Run => Dry_Run,
                               others  => <>);
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
      Char := ASCII.NUL;
      return False;
   end Register;

   -------------
   -- Release --
   -------------

   overriding
   function Release (Self : Object; Char : Character) return Boolean is
     (False);

end GPR2.Build.Jobserver.No_Method;
