--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Unit_Info is

   ----------------------
   -- Set_Parsed_State --
   ----------------------

   procedure Set_Parsed_State (Self : in out Object; State : Boolean) is
   begin
      Self.Is_Parsed := State;
   end Set_Parsed_State;

end GPR2.Build.Unit_Info;
