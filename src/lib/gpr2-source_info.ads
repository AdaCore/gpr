------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This object stores the information from a source. Those
--  information are coming either from a source parser or a
--  compilation artifact parser. The routine Used_Backend will return
--  the kind of parser that has been used.

with GPR2.Unit.List;
with GPR2.Containers;
with GPR2.Path_Name;
with GPR2.Source_Reference.Identifier.Set;

private with GPR2.Unit.Map;

package GPR2.Source_Info is

   use type Containers.Count_Type;
   use type GPR2.Unit.Kind_Type;

   type Object is tagged private;
   --  The source information container

   Undefined : constant Object;

   type Backend is (None, LI, Source, Auto);
   --  None   : not yet parsed, no information
   --  LI     : information from compiler generated data (.ali or .d)
   --  Parser : information from source
   --  Auto   : information to be retrieved from ALI if present
   --           or Parser otherwise.

   subtype Implemented_Backend is Backend range LI .. Source;
   --  The implemented backends. These are the values that are possible
   --  when checking which backend has been used to compute a specific data.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns True if Self is defined

   function Is_Parsed (Self : Object) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True if the source info has been computed

   function Used_Backend (Self : Object) return Implemented_Backend
     with Pre  => Self.Is_Defined and then Self.Is_Parsed;
   --  Returns the backend used to compute the source information

   function Is_Ada (Self : Object) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True if language is Ada

   function Has_Units (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if source is unit-based (i.e. Ada)

   function Has_Unit (Self : Object; Unit : Name_Type) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns True if Self contains the given unit

   type Unit_Index is new Positive;

   function Kind
     (Self : Object; Index : Unit_Index := 1) return Unit.Kind_Type
     with Pre => Self.Is_Defined
                 and then (not Self.Has_Units
                           or else Self.Has_Unit_At (Index));
   --  Returns the kind of Self's source at the given index

   function Has_Unit_At
     (Self : Object; Index : Unit_Index) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns True if Self has a compilation unit at Index

   function Has_Single_Unit (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns true if the source contains a single unit

   function Units
     (Self : Object) return Unit.List.Object
     with Pre  => Self.Is_Defined and then Self.Has_Units,
          Post => Units'Result.Length > 1
                  or else Self.Has_Single_Unit;
   --  Returns all compilation units for self

   function Unit_Name (Self : Object; Index : Unit_Index := 1) return Name_Type
     with Pre => Self.Is_Defined and then Self.Has_Units
                 and then Self.Has_Unit_At (Index);
   --  Returns the unit name for the source Self at Index (default = 1)

   function Is_Generic
     (Self : Object; Index : Unit_Index := 1) return Boolean
     with Pre => Self.Is_Defined and then Self.Has_Unit_At (Index);
   --  Returns True if Self is a generic unit

   function Dependencies
     (Self  : Object;
      Index : Unit_Index := 1) return Source_Reference.Identifier.Set.Object
     with Pre => Self.Is_Defined and then Self.Has_Units
                 and then Self.Has_Unit_At (Index);
   --  Returns the list of withed unit for Self's source at Index (default = 1)

   function Dependencies
     (Self : Object;
      Unit : Name_Type) return Source_Reference.Identifier.Set.Object
     with Pre => Self.Is_Defined
                 and then Self.Has_Units
                 and then Self.Has_Unit (Unit);
   --  Returns the dependencies in Self associated with all the compilation
   --  units for the given Unit. The result may be empty.

   procedure Set
     (Self : in out Object;
      Kind : Unit.Kind_Type)
     with Post => not Self.Is_Ada;

   procedure Set_Ada
     (Self          : in out Object;
      Units         : Unit.List.Object;
      Is_RTS_Source : Boolean)
     with Post => Self.Is_Ada;

   procedure Update (Self : in out Object) is null
     with Pre'Class => Self.Is_Defined;
   --  Update source information. The default implementation does nothing. The
   --  actual work must be done for Source or Project.Source object and depends
   --  on different parser (language or LI based).

   procedure Update_Kind (Self : in out Object; Kind : Unit.Kind_Type)
     with Pre  => Self.Is_Defined
                  and then Self.Has_Units
                  and then Self.Has_Single_Unit
                  and then Kind in Unit.S_Spec_Only | Unit.S_Body_Only,
          Post => Self.Kind = Kind;
   --  Update kind for the source, this is only to adjust the kind to
   --  S_Spec_Only and S_Body_Only after a source based parser has been used.

   procedure Reset (Self : in out Object)
     with Post => not Self.Is_Defined;
   --  Reset Self to undefined

private

   type Object is tagged record
      Is_Ada        : Boolean := True;
      Parsed        : Backend := None;
      Is_RTS_Source : Boolean := False;
      CU_List       : Unit.List.Object;
      CU_Map        : Unit.Map.Object;
      Kind          : Unit.Kind_Type;
   end record
     with Dynamic_Predicate =>
            Object.CU_List.Length = 0
            or else Object.CU_List (1).Kind = Object.Kind;
   --  Record that holds relevant source information, including details about
   --  the compilation unit(s) for Ada sources.

   Undefined : constant Object :=
                 Object'(Is_Ada        => False,
                         Parsed        => None,
                         Is_RTS_Source => True,
                         Kind          => Unit.S_Separate,
                         others        => <>);

   function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

   function Has_Units (Self : Object) return Boolean is (Self.Is_Ada);

   function Used_Backend (Self : Object) return Implemented_Backend is
     (Self.Parsed);

   function Has_Single_Unit (Self : Object) return Boolean is
     (Self.CU_List.Length = 1);

   function Is_Ada (Self : Object) return Boolean is (Self.Is_Ada);

   function Is_Parsed (Self : Object) return Boolean is (Self.Parsed /= None);

end GPR2.Source_Info;
