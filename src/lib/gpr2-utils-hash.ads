--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.File_Indexes;

package GPR2.Utils.Hash is

   subtype Hash_Digest is GNATCOLL.File_Indexes.File_Index_Digest;

   function Hash (Path : Filename_Type) return Hash_Digest;

private

   File_Index : GNATCOLL.File_Indexes.File_Index;

   function Hash (Path : Filename_Type) return Hash_Digest is
     (GNATCOLL.File_Indexes.Hash (File_Index, String (Path)));

end GPR2.Utils.Hash;