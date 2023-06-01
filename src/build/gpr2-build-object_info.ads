--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This object stores the information from a source. Those
--  information are coming either from a source parser or a
--  compilation artifact parser. The routine Used_Backend will return
--  the kind of parser that has been used.

with Ada.Calendar;
with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Ordered_Sets;
private with Ada.Containers.Vectors;

with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Source_Reference.Identifier;
with GPR2.Unit;

package GPR2.Build.Object_Info is

   type Object is tagged private;
   --  The object information container

   Undefined : constant Object;

   --  Parser type definition

   type Backend is (None, LI, Source);
   --  None   : not yet parsed, no information
   --  LI     : information from compiler generated data (.ali or .d)
   --  Parser : information from source

   subtype Implemented_Backend is Backend range LI .. Source;
   --  The implemented backends. These are the values that are possible
   --  when checking which backend has been used to compute a specific data.

   type Backend_Set is array (Implemented_Backend) of Boolean;
   --  Set of backends to use in parser

   All_Backends : constant Backend_Set;
   --  All backends allowed

   No_Backends : constant Backend_Set;
   --  No backends allowed

   --  Compilation unit definition

   type CU_Part is (CU_Spec, CU_Body, CU_Separate);
   subtype Main_CU_Part is CU_Part range CU_Spec .. CU_Body;

   package Separate_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Filename_Type);

   --  Object_Info primitives

   function Is_Defined (Self : Object) return Boolean;
   --  Returns True if Self is defined

   function Create
     (View             : Project.View.Object;
      Path             : Path_Name.Object;
      Timestamp        : Ada.Calendar.Time;
      Language         : Language_Id;
      Is_Overloaded    : Boolean := False) return Object;

   procedure Parse
     (Self     : in out Object;
      Messages : in out GPR2.Log.Object;
      Backends : Backend_Set := All_Backends)
     with Pre => Self.Is_Defined;
   --  Try to parse dependency information for Self.
   --  If Backends is All_Backends, dependency information will be retrieved
   --  in priority with Self's language LI dependency file.
   --  If no such dependency file exists, the compilation unit sources are
   --  parsed (if such parser exists for the language).
   --

   function Is_Parsed (Self  : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True when the Unit in the source has been computed

   function Used_Backend (Self  : Object) return Backend
     with Pre => Self.Is_Defined;
   --  Returns the backend used to compute the object information. None is
   --  returned if no such backend has been used.

   function Language (Self : Object) return Language_Id
     with Pre => Self.Is_Defined;
   --  Returns the language of the source

   function Unit_Name (Self : Object) return Name_Type
     with Pre => Self.Is_Parsed;

   function Has_Spec (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function Spec (Self : Object) return Filename_Type
     with Pre => Self.Has_Spec;

   function Spec_Flags (Self : Object) return Unit.Flags_Set
     with Pre => Self.Has_Spec;

   function Has_Body (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function Main_Body (Self : Object) return Filename_Type
     with Pre => Self.Has_Body;

   function Body_Flags (Self : Object) return Unit.Flags_Set
     with Pre => Self.Has_Body;

   function Separates (Self : Object) return Separate_Maps.Map
     with Pre => Self.Is_Defined;

   procedure Context_Clause_Dependencies
     (Self   : Object;
      Part   : Main_CU_Part := CU_Body;
      Action : access procedure (Ref : Source_Reference.Identifier.Object))
     with Pre => Self.Is_Defined;
   --  Call Action for each with clause of the compilation unit part identified
   --  by Part.
   --  If Object is not parsed, an attempt to parse it is made with
   --  all backends available.

   procedure Dependencies
     (Self   : Object;
      Action : access procedure
                 (Sfile     : Filename_Type;
                  Unit_Name : Name_Type;
                  Kind      : CU_Part;
                  Stamp     : Ada.Calendar.Time))
     with Pre => Self.Is_Defined;
   --  Call Action for each of the object's dependencies, as retrieved from
   --    LI file.
   --  Sfile: base name or full path of the source, depending on how the
   --    LI information is stored.
   --  Unit_Name: No_Name in case of non unit-based dependency, or the
   --    name of the unit.
   --  Kind: S_Spec for non unit-based dependency, or the actual unit kind.
   --  Stamp: the source timestamp at the time the object was built.
   --
   --  Note: Unit_Name and Kind are there to identify a particular unit that
   --  belongs to a multi-unit source.
   --  Note2: if Self is not parsed, and attempt to parse it is made with
   --  the LI backend.

private

   use Ada.Calendar;
   use type GPR2.Source_Reference.Identifier.Object;

   type Compilation_Unit_Item is record
      Source : Unbounded_String;
      --  Source indication: may be a full path or a simple name depending on
      --  how the information was retrieved (e.g. ALI or source parsing).
      Flags  : Unit.Flags_Set         := Unit.Default_Flags;
      L_Type : Unit.Library_Item_Type := Unit.Is_Package;
   end record;

   type CU_Separate_Item is record
      Source   : Unbounded_String;
      Sub_Unit : Unbounded_String;
      Stamp    : Ada.Calendar.Time;
   end record;

   No_Item : constant Compilation_Unit_Item := (others => <>);

   package CU_Dependency_Lists is new Ada.Containers.Vectors
     (Positive, CU_Separate_Item);

   type Compilation_Unit is record
      Name      : Unbounded_String;
      Spec      : Compilation_Unit_Item;
      Implem    : Compilation_Unit_Item;
      Separates : CU_Dependency_Lists.Vector;
   end record;

   type Dependency (Name_Length, SFile_Length : Natural) is record
      Unit_Kind : CU_Part;
      --  Unit kind (S_Separate for a subunit)

      Stamp     : Time := No_Time;
      --  Time stamp value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files.

      Checksum  : Word := 0;
      --  Checksum value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files
      --  Zero if Sfile is configuration pragmas file.

      Unit_Name : Name_Type (1 .. Name_Length);
      --  Name of the unit or subunit.
      --  Empty if Sfile is configuration pragmas file.

      Sfile     : Filename_Type (1 .. SFile_Length);
      --  Base name of the source file for Ada.
      --  Full path name for none-Ada and for configuration pragmas files.
   end record;

   package Dependency_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Dependency);

   package Include_Sets is new Ada.Containers.Ordered_Sets
     (GPR2.Source_Reference.Identifier.Object);

   type Includes_List is array (Main_CU_Part) of Include_Sets.Set;

   type Object is tagged record
      View         : Project.View.Object;
      --  The view owning the object
      Path         : Path_Name.Object;
      --  The object path
      LI_Path      : Path_Name.Object;
      --  Corresponding LI file if it exists, or undefined
      Language     : Language_Id         := No_Language;
      --  Language of the original sources (??? Is that really needed?)
      Timestamp    : Ada.Calendar.Time   := No_Time;
      --  Timestamp of the .o file. No_Time if the file doesn't actually exist
      Dependencies : Dependency_Vectors.Vector;
      --  List of sources that were used to build the object
      Includes     : Includes_List;
      --  List of sources that are included by the specified compilation unit
      --  part.
      CU           : Compilation_Unit    := (others => <>);
      --  Compilation unit
      Main         : GPR2.Unit.Main_Type := Unit.None;
      --  The compilation unit used to build the object
      --  For non-unit based languages, this will contain the main source
      --  (e.g. the .c file for C).
      Parsed       : Backend := None;
      --  Parsed state
   end record;
   --  Record that holds relevant object file information

   function Ensure_Parsed (Self     : in out Object;
                           Backends : Backend_Set) return Boolean;
   --  Ensure the information is parsed and return True in this case

   Undefined : constant Object := (others => <>);

   All_Backends : constant Backend_Set := (others => True);
   No_Backends  : constant Backend_Set := (others => False);

   function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

   function Used_Backend (Self  : Object) return Backend is
     (Self.Parsed);

   function Language (Self : Object) return Language_Id is
     (Self.Language);

   function Is_Parsed (Self  : Object) return Boolean is
     (Self.Parsed /= None);

   function Unit_Name (Self : Object) return Name_Type is
      (Name_Type (-Self.CU.Name));

   function Has_Spec (Self : Object) return Boolean is
      (Length (Self.CU.Spec.Source) > 0);

   function Spec (Self : Object) return Filename_Type is
      (Filename_Type (-Self.CU.Spec.Source));

   function Spec_Flags (Self : Object) return Unit.Flags_Set is
      (Self.CU.Spec.Flags);

   function Has_Body (Self : Object) return Boolean is
      (Length (Self.CU.Implem.Source) > 0);

   function Main_Body (Self : Object) return Filename_Type is
      (Filename_Type (-Self.CU.Implem.Source));

   function Body_Flags (Self : Object) return Unit.Flags_Set is
      (Self.CU.Implem.Flags);

end GPR2.Build.Object_Info;
