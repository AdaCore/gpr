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
     (S : String;
      Val : out Object) is
   begin
      Val := (Path => Path_Name.Create_File (Filename_Type (S)));
   end Unserialize;

end GPR2.Build.Artifacts.Files;
