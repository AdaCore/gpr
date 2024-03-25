--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.Artifacts.Files is

   --------------
   -- Checksum --
   --------------

   overriding function Checksum
     (Self : Object) return Utils.Hash.Hash_Digest
   is
   begin
      return Utils.Hash.Hash (Path => Self.Path.Name);
   end Checksum;

   ------------
   -- Create --
   ------------

   function Create (Path : GPR2.Path_Name.Object) return Object is
   begin
      return (UID  => Utils.Hash.Hash (Str => String (Path.Name)),
              Path => Path);
   end Create;

end GPR2.Build.Artifacts.Files;
