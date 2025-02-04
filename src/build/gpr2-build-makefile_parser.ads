--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.Makefile_Parser is

   function Dependencies
     (Makefile  : GPR2.Path_Name.Object;
      Object    : GPR2.Path_Name.Object;
      Dep_Names : in out GPR2.Containers.Filename_Set;
      Strict    : Boolean := False) return Boolean;
   --  Parse the dependency file names and store them in the provided
   --  'Dep_Names' vector. Returns True upon success.

end GPR2.Build.Makefile_Parser;
