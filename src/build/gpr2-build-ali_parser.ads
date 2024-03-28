--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Equal_Case_Insensitive;
with GPR2.Log;
with GPR2.Path_Name;

package GPR2.Build.ALI_Parser is

   package Dep_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String,
      "="          => Ada.Strings.Equal_Case_Insensitive);

   procedure Dependencies
     (ALI_File  : GPR2.Path_Name.Object;
      Dep_Names : in out Dep_Vectors.Vector;
      Messages  : in out GPR2.Log.Object);
   --  Parse the the dependency files names and store them in the provided
   --  "Dep_Names" vector. Errors of parsing are stored in "Messages".

end GPR2.Build.ALI_Parser;
