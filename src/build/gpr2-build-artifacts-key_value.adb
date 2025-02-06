--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Artifacts.Key_Value is

   -----------------
   -- Unserialize --
   -----------------

   overriding procedure Unserialize
     (Val  : out Object;
      Repr : String;
      Chk  : String;
      Ctxt : GPR2.Project.View.Object)
   is
   begin
      Val := (Key   => +Repr,
              Value => +Chk,
              Ctxt  => Ctxt);
   end Unserialize;

end GPR2.Build.Artifacts.Key_Value;
