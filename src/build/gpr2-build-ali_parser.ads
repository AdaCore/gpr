--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.ALI_Parser is

   function Dependencies
     (ALI_File  : GPR2.Path_Name.Object;
      Dep_Names : in out GPR2.Containers.Filename_Set) return Boolean;
   --  Parse the dependency file names and store them in the provided
   --  'Dep_Names' vector. Returns True upon success.

   function Imports
     (ALI_File : GPR2.Path_Name.Object;
      Imports  : in out GPR2.Containers.Name_Set) return Boolean;
   --  Parse the explicit import lines that begin with the 'W' character.
   --  Returns True upon success.

   function Version (ALI_File : GPR2.Path_Name.Object) return String;
   --  Parse the ALI file to obtain the version, and return the
   --  "vXX.XXXXXXX".

   function Switches
     (ALI_File : GPR2.Path_Name.Object) return GPR2.Containers.Value_List;

end GPR2.Build.ALI_Parser;
