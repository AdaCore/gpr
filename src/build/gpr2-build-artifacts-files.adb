--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.File_Indexes;

package body GPR2.Build.Artifacts.Files is

   File_Table : GNATCOLL.File_Indexes.File_Index;

   --------------
   -- Checksum --
   --------------

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest
   is
   begin
      return Utils.Hash.Hash_Digest
        (GNATCOLL.File_Indexes.Hash (File_Table, Self.Path.String_Value));
   end Checksum;

end GPR2.Build.Artifacts.Files;
