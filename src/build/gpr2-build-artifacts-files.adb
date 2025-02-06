--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


package body GPR2.Build.Artifacts.Files is

   -----------------
   -- Unserialize --
   -----------------

   overriding procedure Unserialize
     (Val  : out Object;
      Repr : String;
      Chk  : String;
      Ctxt : GPR2.Project.View.Object)
   is
      pragma Unreferenced (Chk);
   begin
      Val := (Path => Path_Name.Create_File (Filename_Type (Repr)));
   end Unserialize;

end GPR2.Build.Artifacts.Files;
