--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with System.Address_Image;

package body GPR2.Project.Tree.C is

   --------
   -- Id --
   --------

   function Id
     (Tree : GPR2.Project.Tree.Object)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      if Tree.Tree = null then
         return Ada.Strings.Unbounded.Null_Unbounded_String;

      else
         return
           Ada.Strings.Unbounded.To_Unbounded_String
             (System.Address_Image (Tree.Tree.all'Address));
      end if;
   end Id;

end GPR2.Project.Tree.C;
