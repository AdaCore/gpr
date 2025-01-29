--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package provides deserialization of Options from JSON.

with GPR2.C.JSON.Values;
with GPR2.Options;

package GPR2.C.JSON.Codecs.Options is

   function Decode
     (Value : GPR2.C.JSON.Values.JSON_Value) return GPR2.Options.Object;

end GPR2.C.JSON.Codecs.Options;
