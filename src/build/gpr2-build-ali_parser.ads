--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--


with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded;
with GPR2.Log;
with GPR2.Path_Name;

package GPR2.Build.ALI_Parser is

   package UB renames Ada.Strings.Unbounded;

   package Dep_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String,
      "="          => Ada.Strings.Equal_Case_Insensitive);

   type Import_Info is record
      Unit_Name : UB.Unbounded_String;
      Source    : UB.Unbounded_String;
      ALI       : UB.Unbounded_String;
   end record;

   overriding function "=" (Left, Right : Import_Info) return Boolean;

   package Import_Info_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Import_Info,
      "="          => GPR2.Build.ALI_Parser."=");

   procedure Dependencies
     (ALI_File  : GPR2.Path_Name.Object;
      Dep_Names : in out Dep_Vectors.Vector;
      Messages  : in out GPR2.Log.Object);
   --  Parse the the dependency files names and store them in the provided
   --  "Dep_Names" vector. Errors of parsing are stored in "Messages".

   procedure Imports
     (ALI_File : GPR2.Path_Name.Object;
      Imports  : in out Import_Info_Vectors.Vector;
      Messages : in out GPR2.Log.Object);
   --  Parse the explicit imports lines beginning with the 'W' character.

end GPR2.Build.ALI_Parser;
