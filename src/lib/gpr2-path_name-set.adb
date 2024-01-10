--
--  Copyright (C) 2020-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Path_Name.Set is

   ------------
   -- To_Set --
   ------------

   function To_Set (Item : Path_Name.Object) return Object is
   begin
      return Result : Object do
         Result.Append (Item);
      end return;
   end To_Set;

end GPR2.Path_Name.Set;
