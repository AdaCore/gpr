--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Values;

package body GPR2.C.JSON.Codecs.Path_Names is

   ------------
   -- Encode --
   ------------

   function Encode
     (Object : GPR2.Path_Name.Set.Object)
      return GPR2.C.JSON.Arrays.JSON_Array is
   begin
      return Result : GPR2.C.JSON.Arrays.JSON_Array do
         for Element of Object loop
            Result.Append
              (GPR2.C.JSON.Values.To_JSON_Value (String (Element.Value)));
         end loop;
      end return;
   end Encode;

end GPR2.C.JSON.Codecs.Path_Names;
