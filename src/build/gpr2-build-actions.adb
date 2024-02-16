--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Tree_Db;

package body GPR2.Build.Actions is

   ------------
   -- Attach --
   ------------

   procedure Attach (Self : in out Object;
                     Db   : in  out GPR2.Build.Tree_Db.Object)
   is
   begin
      Self.Tree := Db.Ref;
   end Attach;

end GPR2.Build.Actions;
