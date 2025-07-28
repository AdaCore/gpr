--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package provides serialization of Message into JSON.

with GPR2.C.JSON.Objects;
with GPR2.Message;

package GPR2.C.JSON.Codecs.Messages is

   function Encode
     (Object : GPR2.Message.Object) return GPR2.C.JSON.Objects.JSON_Object;

end GPR2.C.JSON.Codecs.Messages;
