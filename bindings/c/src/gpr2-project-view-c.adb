--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with System.Address_Image;

package body GPR2.Project.View.C is

   --------
   -- Id --
   --------

   function Id
     (View : GPR2.Project.View.Object)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      if View.Is_Null then
         return Ada.Strings.Unbounded.Null_Unbounded_String;

      else
         return
           Ada.Strings.Unbounded.To_Unbounded_String
             (System.Address_Image (View.Get.Element.all'Address));
      end if;
   end Id;

end GPR2.Project.View.C;
