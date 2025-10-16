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

   ----------
   -- Load --
   ----------

   function Load (Path : Path_Name.Object) return Object is
   begin
      if Path.Is_Defined and then Path.Exists then
         return Result : Object do
            Result.File_Index :=
              GNATCOLL.File_Indexes.Load_Index (Path.String_Value);
         end return;
      else
         return Object'(others => <>);
      end if;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Self : Object;
                   Path : Path_Name.Object) is
   begin
      if Path.Is_Defined then
         GNATCOLL.File_Indexes.Save_Index (Self.File_Index, Path.String_Value);
      end if;
   end Save;

end GPR2.Utils.Hash;
