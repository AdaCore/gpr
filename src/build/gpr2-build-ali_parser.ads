--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Containers;
with GPR2.Path_Name;

package GPR2.Build.ALI_Parser is

   type Spec_Body is (U_Spec, U_Body);

   type Object is tagged private;
   --  Object used to store the information extracted from .ali files

   function Create
     (Path : GPR2.Path_Name.Object; Parse : Boolean) return Object;
   --  Create a new instance for the ali file located at "Path". If "Parse" is
   --  set, the ali file is parsed immediately. Else an explicit call to the
   --  function "Parse" below is needed.

   function Is_Defined (Self : Object) return Boolean;

   procedure Parse (Self : in out Object);
   --  Tries to parse the .ali file pointed to by Self. Note that Is_Parsed
   --  won't necessarily return True after this call, if the .ali file could
   --  not be found or contains errors.

   function Is_Parsed (Self : Object) return Boolean;
   --  Returns true when a valid ali file has been parsed.

   function Path_Name (Self : Object) return Path_Name.Object
   with Pre => Self.Is_Defined;
   --  Path the the ali file represented by Self

   function Dependencies (Self : Object) return GPR2.Containers.Filename_Set
   with Pre => Self.Is_Parsed;
   --  Parse the dependency file names and store them in the provided
   --  'Dep_Names' vector. Returns True upon success.

   function Withed_From_Spec (Self : Object) return GPR2.Containers.Name_Set
   with Pre => Self.Is_Parsed;

   function Withed_From_Body (Self : Object) return GPR2.Containers.Name_Set
   with Pre => Self.Is_Parsed;

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
      Dependencies   : GPR2.Containers.Filename_Set;
      --  The list of sources this unit depends on
      Spec_Imports   : GPR2.Containers.Name_Set;
      --  The list of units imported by the spec
      Body_Imports   : GPR2.Containers.Name_Set;
      --  The list of units imported by the body
      Needs_Body     : Boolean := False;
      --  Whether importing the specs requires a visible body
      Version        : Unbounded_String;
      --  Version of the toolchain that produced the .ALI
      Linker_Options : GPR2.Containers.Value_List;
      --  Contains the list of pragma Linker_Option present in the source, if
      --  any.
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);

   function Path_Name (Self : Object) return GPR2.Path_Name.Object
   is (Self.Path);

   function Is_Parsed (Self : Object) return Boolean
   is (Self.Is_Parsed);

   function Dependencies (Self : Object) return GPR2.Containers.Filename_Set
   is (Self.Dependencies);

   function Withed_From_Spec (Self : Object) return GPR2.Containers.Name_Set
   is (Self.Spec_Imports);

   function Withed_From_Body (Self : Object) return GPR2.Containers.Name_Set
   is (Self.Body_Imports);

   function Spec_Needs_Body (Self : Object) return Boolean
   is (Self.Needs_Body);

   function Linker_Options (Self : Object) return GPR2.Containers.Value_List
   is (Self.Linker_Options);

   function Version (Self : Object) return String
   is (-Self.Version);

end GPR2.Build.ALI_Parser;
