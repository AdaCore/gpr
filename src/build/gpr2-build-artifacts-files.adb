--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Utils.Hash;

package body GPR2.Build.Artifacts.Files is

   --------------
   -- Checksum --
   --------------

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest
   is
   begin
      return Utils.Hash.Hash (Self.Path.Value);
   end Checksum;

   -----------------
   -- Unserialize --
   -----------------

   overriding procedure Unserialize
     (S : String;
      Val : out Object) is
   begin
      Val := (Path => Path_Name.Create_File (Filename_Type (S)));
   end Unserialize;

end GPR2.Build.Artifacts.Files;
