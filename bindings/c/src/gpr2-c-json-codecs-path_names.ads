--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package provides serialization of Source into JSON.

with GPR2.C.JSON.Arrays;
with GPR2.Path_Name.Set;

package GPR2.C.JSON.Codecs.Path_Names is

   function Encode
     (Object : GPR2.Path_Name.Set.Object)
      return GPR2.C.JSON.Arrays.JSON_Array;

end GPR2.C.JSON.Codecs.Path_Names;
