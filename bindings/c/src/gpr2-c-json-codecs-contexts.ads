--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package provides serialization of Source into JSON.

with GPR2.C.JSON.Objects;
with GPR2.C.JSON.Values;
with GPR2.Context;

package GPR2.C.JSON.Codecs.Contexts is

   function Decode
     (Value : GPR2.C.JSON.Values.JSON_Value) return GPR2.Context.Object;

   function Encode
     (Object : GPR2.Context.Object) return GPR2.C.JSON.Objects.JSON_Object;

end GPR2.C.JSON.Codecs.Contexts;
