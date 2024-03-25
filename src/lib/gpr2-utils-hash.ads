--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.UTF_Encoding;

with GNATCOLL.File_Indexes;
with GNATCOLL.OS.FSUtil;
with GNATCOLL.Hash.Blake3;

package GPR2.Utils.Hash is

   package FSUtil renames GNATCOLL.OS.FSUtil;
   package UTF8   renames Ada.Strings.UTF_Encoding;
   package Blake3 renames GNATCOLL.Hash.Blake3;

   type Hash_Digest is new FSUtil.SHA1_Digest;
   type B3_Hash_Digest is new Blake3.Blake3_Digest;

   function Hash (Path : Filename_Type) return Hash_Digest;

   function Hash (Str : String) return B3_Hash_Digest;

private

   File_Index : GNATCOLL.File_Indexes.File_Index;

   function Hash (Path : Filename_Type) return Hash_Digest is
     (Hash_Digest (GNATCOLL.File_Indexes.Hash (File_Index, String (Path))));

end GPR2.Utils.Hash;
