--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.C.JSON.Codecs.Sources is

   ------------
   -- Encode --
   ------------

   function Encode
     (Object : GPR2.Build.Source.Object)
      return GPR2.C.JSON.Objects.JSON_Object is
   begin
      return Result : GPR2.C.JSON.Objects.JSON_Object do
         Result.Insert ("language", String (GPR2.Name (Object.Language)));
         Result.Insert ("path_name", String (Object.Path_Name.Value));
      end return;
   end Encode;

end GPR2.C.JSON.Codecs.Sources;
