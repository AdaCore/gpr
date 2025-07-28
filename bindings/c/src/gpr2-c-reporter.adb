--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

---------------------
-- GPR2.C.Reporter --
---------------------

package body GPR2.C.Reporter is

   ---------------------
   -- Internal_Report --
   ---------------------

   overriding
   procedure Internal_Report
     (Self : in out Object; Message : GPR2.Message.Object) is
   begin
      null;
   end Internal_Report;

   ---------------
   -- Verbosity --
   ---------------

   overriding
   function Verbosity (Self : Object) return GPR2.Reporter.Verbosity_Level is
   begin
      return GPR2.Reporter.Regular;
   end Verbosity;

end GPR2.C.Reporter;
