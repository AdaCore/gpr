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
     (Self : Object) return GNAT.SHA256.Message_Digest
   is
      Context : GNAT.SHA256.Context := GNAT.SHA256.Initial_Context;
   begin
      GNAT.SHA256.Update (Context, "TODO");
      return GNAT.SHA256.Digest (Context);
      --  ??? TODO
   end Checksum;

   ------------
   -- Create --
   ------------

   function Create (Path : GPR2.Path_Name.Object) return Object is
   begin
      return (Path   => Path);
   end Create;

end GPR2.Build.Artifacts.Files;
