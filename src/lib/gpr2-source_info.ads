--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This object stores the information from a source. Those
--  information are coming either from a source parser or a
--  compilation artifact parser. The routine Used_Backend will return
--  the kind of parser that has been used.

with Ada.Calendar;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;

with GPR2.Unit.List;
with GPR2.Containers;
with GPR2.Source_Reference.Identifier.Set;

with GNATCOLL.Refcount;

package GPR2.Source_Info is

   use type Containers.Count_Type;
   use type GPR2.Unit.Library_Unit_Type;

   type Object is tagged private;
   --  The source information container

   Undefined : constant Object;

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

   function Is_Defined (Self : Object) return Boolean;
   --  Returns True if Self is defined

   type Parse_State is (No, Partial, Full);

   function Is_Parsed (Self : Object) return Parse_State;

   function Is_Parsed (Self  : Object;
                       Index : Unit_Index) return Boolean;
   --  Returns True when the Unit in the source has been computed

   function Used_Backend (Self  : Object;
                          Index : Unit_Index) return Implemented_Backend
     with Pre => Self.Is_Defined and then Self.Is_Parsed (Index);
   --  Returns the backend used to compute the source information

   function Is_Ada (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if language is Ada

   function Language (Self : Object) return Language_Id
     with Pre => Self.Is_Defined;
   --  Returns the language of the source

   function Build_Timestamp (Self  : Object;
                             Index : Unit_Index) return Ada.Calendar.Time
     with Inline,
          Pre => Self.Is_Defined;
   --  Returns last modification of the source file from the time point when
   --  the last successful build was done.

   function Checksum (Self : Object) return Word
     with Pre => Self.Is_Defined;

   function Has_Units (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if source is unit-based (i.e. Ada)

   function Has_Unit (Self : Object; Unit : Name_Type) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns True if Self contains the given unit

   function Kind
     (Self  : Object;
      Index : Unit_Index := No_Index) return Unit.Library_Unit_Type
     with Pre => Self.Is_Defined
                 and then (not Self.Has_Units
                           or else Self.Has_Unit_At (Index))
                 and then (Self.Has_Units or else Index = No_Index);
   --  Returns the kind of Self's source at the given index

   function Check_Unit
     (Self : Object;
      Name : Name_Type;
      Spec : Boolean;
      Unit : out GPR2.Unit.Object) return Boolean;
   --  Check if the unit exists in the source file and set Unit and returns
   --  True if found.
   --  If Spec is True search for the unit kind in Spec_Kind.
   --  Search for the Body_Kind or S_Separate otherwise.

   function Has_Unit_At
     (Self : Object; Index : Unit_Index) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns True if Self has a compilation unit at Index

   function Has_Single_Unit (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns true if the source contains a single unit

   function Has_Index (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if source contains one or more units declared in Naming
   --  package with "at" Index.

   function Unit (Self  : Object;
                  Index : Unit_Index) return GPR2.Unit.Object
     with Pre => Self.Is_Defined
                   and then Self.Has_Units and then Self.Has_Unit_At (Index);

   function Units
     (Self : Object) return GPR2.Unit.List.Object
     with Inline,
          Pre  => Self.Is_Defined and then Self.Has_Units,
          Post => Units'Result.Length > 1
                  or else Self.Has_Single_Unit;
   --  Returns all compilation units for self

   function Unit_Name (Self  : Object;
                       Index : Unit_Index := No_Index) return Name_Type
     with Pre => Self.Is_Defined and then Self.Has_Units
                 and then Self.Has_Unit_At (Index);
   --  Returns the unit name for the source Self at Index (default = 1)

   function Is_Generic
     (Self : Object; Index : Unit_Index) return Boolean
     with Pre => Self.Is_Defined
                   and then
                 (not Self.Has_Units
                  or else Self.Has_Unit_At (Index));
   --  Returns True if the source Self has the generic unit at Index

   function Is_Implementation_Required
     (Self : Object; Index : Unit_Index) return Boolean
     with Pre => Self.Is_Defined
                 and then Self.Has_Unit_At (Index);
   --  Returns True if the source for the implementation is required for the
   --  compilation. This is the case for a generic package or a package having
   --  inlined routines.

   function Context_Clause_Dependencies
     (Self  : Object;
      Index : Unit_Index)
      return Source_Reference.Identifier.Set.Object
     with Pre => Self.Is_Defined and then Self.Has_Units
                 and then Self.Has_Unit_At (Index);
   --  Returns the list of withed unit for Self's source at Index (default = 1)

   procedure Dependencies
     (Self   : Object;
      Index  : Unit_Index;
      Action : access procedure
                 (Unit_Name : Name_Type;
                  Sfile     : Simple_Name;
                  Kind      : GPR2.Unit.Library_Unit_Type;
                  Stamp     : Ada.Calendar.Time))
     with Pre => Self.Is_Defined;
   --  Call Action for each of unit dependencies

   procedure Set_Non_Ada
     (Self     : in out Object;
      Language : Language_Id;
      Kind     : GPR2.Unit.Library_Unit_Type);

   procedure Set_Ada
     (Self  : in out Object;
      Units : GPR2.Unit.List.Object)
     with Post => Self.Is_Ada and then Self.Has_Index;
   --  Set Ada-specific info for a multi-unit source

   procedure Set_Ada
     (Self          : in out Object;
      Unit          : GPR2.Unit.Object;
      Is_RTS_Source : Boolean)
     with Pre  => Unit.Is_Defined,
          Post => Self.Is_Ada
                    and then not Self.Has_Index and then Self.Has_Single_Unit;
   --  Set Ada-specific info for a single-unit source

   procedure Update_Kind
     (Self  : in out Object;
      Kind  : GPR2.Unit.Library_Unit_Type;
      Index : Unit_Index)
     with Pre  => Self.Is_Defined
               and then Kind in GPR2.Unit.S_Spec_Only | GPR2.Unit.S_Body_Only;
   --  Update kind for the source, this is only to adjust the kind to
   --  S_Spec_Only and S_Body_Only after a source based parser has been used.

   procedure Update_Build_Timestamp
     (Self : in out Object; Stamp : Ada.Calendar.Time)
     with Pre  => Self.Is_Defined;
   --  Update source file timestamp

   procedure Reset (Self : in out Object)
     with Post => not Self.Is_Defined;
   --  Reset Self to undefined

   procedure Clear (Self : in out Object);
   --  Clear units and dependencies

   function Is_Runtime (Self : Object) return Boolean;
   --  Is the source from runtime library

private

   use Ada.Calendar;

   type Dependency (Name_Length, SFile_Length : Natural) is record
      Unit_Kind : GPR2.Unit.Library_Unit_Type;
      --  Unit kind (S_Separate for a subunit)

      Stamp     : Time := No_Time;
      --  Time stamp value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files.

      Checksum  : Word := 0;
      --  Checksum value. Note that this will be all zero characters for the
      --  dummy entries for missing or non-dependent files
      --  Zero if Sfile is configuration pragmas file.

      Unit_Name : String (1 .. Name_Length);
      --  Name of the unit or subunit.
      --  Empty if Sfile is configuration pragmas file.

      Sfile     : String (1 .. SFile_Length);
      --  Base name of the source file for Ada.
      --  Full path name for none-Ada and for configuration pragmas files.
   end record;

   package Dependency_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Dependency);

   package Dependency_Vectors_Ref is new GNATCOLL.Refcount.Shared_Pointers
     (Dependency_Vectors.Vector);

   function Equ (L, R : Dependency_Vectors_Ref.Ref) return Boolean is
     (if L.Is_Null and then R.Is_Null then True
      elsif L.Is_Null or else R.Is_Null then False
      else Dependency_Vectors."=" (L.Get.Element.all, R.Get.Element.all));

   package Unit_Dependencies is new Ada.Containers.Ordered_Maps
     (Unit_Index, Dependency_Vectors_Ref.Ref, "=" => Equ);

   type Unit_Info is record
      Build_Timestamp : Calendar.Time := No_Time;
      Parsed          : Backend := None;
   end record;

   package Unit_Info_Vectors is new Ada.Containers.Vectors
     (Multi_Unit_Index, Unit_Info);

   type Object is tagged record
      --  Common to all sources
      Language      : Language_Id   := No_Language;
      Checksum      : Word          := 0;
      Dependencies  : Unit_Dependencies.Map;
      --  Non unit based source info
      Kind          : GPR2.Unit.Library_Unit_Type := GPR2.Unit.S_Separate;
      --  Non unit-based or single unit info
      Parsed        : Backend := None;
      LI_Timestamp  : Calendar.Time := No_Time;
      --  unit based sources properties
      Is_RTS_Source : Boolean := False;
      CU_List       : GPR2.Unit.List.Object;
      --  multi-unit specifc source property
      CU_Info       : Unit_Info_Vectors.Vector;
   end record;
   --  Record that holds relevant source information, including details about
   --  the compilation unit(s) for Ada sources.

   Undefined : constant Object := (others => <>);

   All_Backends : constant Backend_Set := (others => True);
   No_Backends  : constant Backend_Set := (others => False);

   function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

   function Checksum (Self : Object) return Word is
     (Self.Checksum);

   function Has_Units (Self : Object) return Boolean is (Self.Is_Ada);

   function Used_Backend (Self  : Object;
                          Index : Unit_Index)
                          return Implemented_Backend
   is (if Index = No_Index
       then Self.Parsed
       else Self.CU_Info (Index).Parsed);

   function Has_Single_Unit (Self : Object) return Boolean is
     (Self.CU_List.Length = 1);

   function Has_Index (Self : Object) return Boolean is
     (Self.CU_List.Is_Indexed_List);

   function Is_Ada (Self : Object) return Boolean is
     (Self.Language = Ada_Language);

   function Language (Self : Object) return Language_Id is
     (Self.Language);

   function Is_Runtime (Self : Object) return Boolean is
     (Self.Is_RTS_Source);

   function Is_Parsed (Self  : Object;
                       Index : Unit_Index) return Boolean is
     (if Index = No_Index
      then Self.Parsed /= None
      else
        (if Self.CU_Info.Last_Index >= Index
         then Self.CU_Info (Index).Parsed /= None
         else False));

   function Units
     (Self : Object) return GPR2.Unit.List.Object is (Self.CU_List);

end GPR2.Source_Info;
