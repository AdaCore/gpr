--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.Hash.Blake3;

package body GPR2.Utils.Hash is

------------------
-- Hash_Content --
------------------

   function Hash_Content (Cnt : String) return Hash_Digest
   is
      use GNATCOLL.Hash.Blake3;

      Ctxt : Blake3_Context;
   begin
      Init_Hash_Context (Ctxt);
      Update_Hash_Context (Ctxt, Cnt);
      return GNATCOLL.Hash.Blake3.Hash_Digest (Ctxt);
   end Hash_Content;

end GPR2.Utils.Hash;
