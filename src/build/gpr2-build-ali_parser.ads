--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Vectors;

with GPR2.Build.Dependencies;
with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.ALI_Parser is

   type Spec_Body is (U_Spec, U_Body);

   type Library_Unit_Kind is (Unknown, Is_Package, Is_Subprogram);

   type Unit_Attribute is
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

   type Unit_Attributes is array (Unit_Attribute) of Boolean
     with Default_Component_Value => False;

   type Unit_Info (Name_Length : Natural) is record
      Name       : Name_Type (1 .. Name_Length);
      --  Unit name from the U record, without the %s or %b suffix

      Part       : Spec_Body;
      --  Specification or body, as identified by the unit name suffix

      Kind       : Library_Unit_Kind := Unknown;
      --  Package or subprogram, as identified by the PK or SU attribute.
      --  Unknown when neither attribute is recorded.

      Attributes : Unit_Attributes := (others => False);
      --  Attributes recorded for this unit part; unrecorded attributes
      --  remain False
   end record;
   --  Information from one ALI unit record, identifying its name and part

   package Unit_Info_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Unit_Info);

   subtype Unit_Info_Vector is Unit_Info_Vectors.Vector;

   type Object is tagged private;
   --  Object used to store the information extracted from .ali files

   function Create
     (Path : GPR2.Path_Name.Object) return Object;
   --  Create a new instance for the ali file located at "Path".

   function Is_Defined (Self : Object) return Boolean;

   function Parse (Self : in out Object) return Boolean;
   --  Tries to parse the .ali file pointed to by Self. Note that Is_Parsed
   --  won't necessarily return True after this call, if the .ali file could
   --  not be found or contains errors.
   --  Return true if the file was successfully parsed, false otherwise.

   function Is_Parsed (Self : Object) return Boolean;
   --  Returns true when a valid ali file has been parsed.

   function Units (Self : Object) return Unit_Info_Vector
     with Pre => Self.Is_Parsed;
   --  Return the unit records found in the ALI, in file order. Only parts
   --  described by a U record are included.

   procedure Reset (Self : in out Object)
     with Pre  => Self.Is_Defined,
          Post => not Self.Is_Parsed;
   --  Empty the parser cache

   function Path_Name (Self : Object) return Path_Name.Object
   with Pre => Self.Is_Defined;
   --  Path the the ali file represented by Self

   function Dependencies (Self : Object) return GPR2.Build.Dependencies.Set
   with Pre => Self.Is_Parsed;
   --  Return dependencies grouped by source filename

   function Withed_From_Spec (Self : Object) return GPR2.Containers.Name_Set
   with Pre => Self.Is_Parsed;

   function Withed_From_Body (Self : Object) return GPR2.Containers.Name_Set
   with Pre => Self.Is_Parsed;

   function Has_Imports (Self : Object) return Boolean
   with Pre => Self.Is_Parsed;
   --  Whether the spec or the body imports anything

   function Unresolved_Withs (Self : Object) return GPR2.Containers.Name_Set
   with Pre => Self.Is_Parsed;
   --  Units referenced on a W/Y/Z with-line for which the compiler recorded
   --  no source/ALI file at all (a bare "W unit_name%s" with nothing
   --  following).

   function Spec_Needs_Body (Self : Object) return Boolean
   with Pre => Self.Is_Parsed;

   function Linker_Options (Self : Object) return GPR2.Containers.Value_List
   with Pre => Self.Is_Parsed;

   function Version (Self : Object) return String
   with Pre => Self.Is_Parsed;
   --  Parse the ALI file to obtain the version, and return the
   --  "vXX.XXXXXXX".

   function Switches
     (ALI_File : GPR2.Path_Name.Object) return GPR2.Containers.Value_List;

   function Version (ALI_File : GPR2.Path_Name.Object) return String;

private

   type Object is tagged record
      Path           : GPR2.Path_Name.Object;
      Is_Parsed      : Boolean := False;
      Dependencies   : GPR2.Build.Dependencies.Set;
      --  Current unit dependencies
      Spec_Imports   : GPR2.Containers.Name_Set;
      --  The list of units imported by the spec
      Body_Imports   : GPR2.Containers.Name_Set;
      --  The list of units imported by the body
      Unresolved_Withs : GPR2.Containers.Name_Set;
      --  Units from Spec_Imports/Body_Imports whose with-line had no
      --  source/ALI file recorded by the compiler
      Needs_Body     : Boolean := False;
      --  Whether importing the specs requires a visible body
      Version        : Unbounded_String;
      --  Version of the toolchain that produced the .ALI
      Linker_Options : GPR2.Containers.Value_List;
      --  Contains the list of pragma Linker_Option present in the source, if
      --  any.
      Units          : Unit_Info_Vector;
      --  Units information
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);

   function Path_Name (Self : Object) return GPR2.Path_Name.Object
   is (Self.Path);

   function Is_Parsed (Self : Object) return Boolean
   is (Self.Is_Parsed);

   function Units (Self : Object) return Unit_Info_Vector
   is (Self.Units);

   function Dependencies (Self : Object) return GPR2.Build.Dependencies.Set
   is (Self.Dependencies);

   function Withed_From_Spec (Self : Object) return GPR2.Containers.Name_Set
   is (Self.Spec_Imports);

   function Withed_From_Body (Self : Object) return GPR2.Containers.Name_Set
   is (Self.Body_Imports);

   function Has_Imports (Self : Object) return Boolean
   is (not Self.Spec_Imports.Is_Empty or else not Self.Body_Imports.Is_Empty);

   function Unresolved_Withs (Self : Object) return GPR2.Containers.Name_Set
   is (Self.Unresolved_Withs);

   function Spec_Needs_Body (Self : Object) return Boolean
   is (Self.Needs_Body);

   function Linker_Options (Self : Object) return GPR2.Containers.Value_List
   is (Self.Linker_Options);

   function Version (Self : Object) return String
   is (-Self.Version);

end GPR2.Build.ALI_Parser;
