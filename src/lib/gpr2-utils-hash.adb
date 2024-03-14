--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Utils.Hash is

   function Hash (Str : String) return B3_Hash_Digest is
      Context : Blake3.Blake3_Context;
   begin
      Blake3.Init_Hash_Context (Context);
      Blake3.Update_Hash_Context (Context, Str);
      return B3_Hash_Digest (Context.Hash_Digest);
   end Hash;

end GPR2.Utils.Hash;
