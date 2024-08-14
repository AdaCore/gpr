--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Path_Name;

package GPR2.Build.Artifacts.Library is

   type Object is new GPR2.Build.Artifacts.Files.Object with null record;

   overriding function Create (Path : GPR2.Path_Name.Object) return Object is
      (Files.Create (Path) with null record);

end GPR2.Build.Artifacts.Library;
