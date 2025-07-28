--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Reporter;
private with GPR2.Message;

package GPR2.C.Reporter is

   type Object is new GPR2.Reporter.Object with private;

private

   type Object is new GPR2.Reporter.Object with null record;

   overriding
   function Verbosity (Self : Object) return GPR2.Reporter.Verbosity_Level;

   overriding
   procedure Internal_Report
     (Self : in out Object; Message : GPR2.Message.Object);

end GPR2.C.Reporter;
