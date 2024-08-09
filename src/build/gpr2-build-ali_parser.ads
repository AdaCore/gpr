--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name;

package GPR2.Build.ALI_Parser is

   procedure Dependencies
     (ALI_File  : GPR2.Path_Name.Object;
      Dep_Names : in out GPR2.Containers.Filename_Set;
      Messages  : in out GPR2.Log.Object);
   --  Parse the the dependency files names and store them in the provided
   --  "Dep_Names" vector. Errors of parsing are stored in "Messages".

   procedure Imports
     (ALI_File : GPR2.Path_Name.Object;
      Imports  : in out GPR2.Containers.Name_Set;
      Messages : in out GPR2.Log.Object);
   --  Parse the explicit imports lines beginning with the 'W' character.

end GPR2.Build.ALI_Parser;
