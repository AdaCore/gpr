--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.ALI_Parser is

   type Unit_Flags_Kind is
     (Elaborate_Body_Desirable, --  BD
      Body_Needed_For_SAL,      --  BN
      Dynamic_Elab,             --  DE
      Elaborate_Body,           --  EB
      Set_Elab_Entity,          --  EE
      Is_Generic,               --  GE
      Init_Scalars,             --  IS
      No_Elab,                  --  NE
      Has_Finalizer,            --  PF
      Preelab,                  --  PR
      Pure,                     --  PU
      RCI,                      --  RC
      Remote_Types,             --  RT
      Has_RACW,                 --  RA
      Serious_Errors,           --  SE
      Shared_Passive);          --  SP

   type Spec_Body is (U_Spec, U_Body);

   type Unit_Flags_Set is array (Unit_Flags_Kind) of Boolean
      with Default_Component_Value => False;
   type Units_Flags_Set is array (Spec_Body) of Unit_Flags_Set;

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

   function Unit_Flags
     (ALI_File : GPR2.Path_Name.Object) return Units_Flags_Set;
   --  The flags for the spec/body of the given unit

end GPR2.Build.ALI_Parser;
