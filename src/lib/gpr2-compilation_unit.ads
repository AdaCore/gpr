------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

--  This package represents an Ada Compilation_Unit in the sense of the Ada
--  grammar. An Ada source may be multi-unit (versus single-unit) i.e. define
--  more than one such compilation units, each one having its own set of with
--  clauses. The list of those compilation units for the source is kept in
--  Source.Registry.
--
--  Each Compilation_Unit object has the following data:
--     - The name of the unit it refers to
--     - Its index in the compilation units that the source contains,
--       starting from 1
--     - The kind of definition it provides for the unit: either it is the
--       unit's specification, main body, or a separate body.
--     - Its with clauses, represented as Source_Reference.Identifier objects
--       (withed package name + source location)
--     - The name of the unit it is a separate from, if it's a separate
--
--  For instance, if source S has the following content:
--
--     with Foo;
--     package body Pkg is
--        [...]
--     end Pkg;
--
--     with Bar;
--     separate (A) procedure Proc is
--        [...]
--     end Proc;
--
--  Then two compilation units will be stored in the registry entry for S:
--     { (Unit_Name => Pkg, Index => 1, Kind => S_Body,
--        Withed_Units => {Foo}, Is_Sep_From => -),
--       (Unit_Name => Proc, Index => 2, Kind => S_Separate,
--        Withed_Units => {Bar}, Is_Sep_From => A) }

with GPR2.Source_Reference.Identifier.Set;

private with Ada.Strings.Unbounded;

package GPR2.Compilation_Unit is

   type Object is tagged private;

   Undefined : constant Object;

   function Create
     (Unit_Name    : Name_Type;
      Index        : Positive;
      Kind         : Kind_Type;
      Withed_Units : Source_Reference.Identifier.Set.Object;
      Is_Sep_From  : Optional_Name_Type) return Object;

   function Unit_Name (Self : Object) return Name_Type
     with Pre => Self /= Undefined;
   --  Returns the unit name for this compilation unit

   function Index (Self : Object) return Positive
     with Pre => Self /= Undefined;
   --  Returns the source index for this compilation unit

   function Kind (Self : Object) return Kind_Type
     with Pre => Self /= Undefined;
   --  Returns the kind for this compilation unit

   function Withed_Units
     (Self : Object) return Source_Reference.Identifier.Set.Object
     with Pre => Self /= Undefined;
   --  Returns the set of withed units for this compilation unit

   function Is_Separate (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if Self is a separate

   function Is_Separate_From (Self : Object) return Name_Type
     with Pre => Self /= Undefined and then Self.Is_Separate;
   --  If this compilation unit is a separate, returns its parent unit, else
   --  returns an empty string.

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Unit_Name    : Unbounded_String;
      Index        : Natural;
      Kind         : Kind_Type;
      Withed_Units : Source_Reference.Identifier.Set.Object;
      Is_Sep_From  : Unbounded_String;
   end record
     with Dynamic_Predicate =>
       Length (Is_Sep_From) = 0 or else Kind = S_Separate;
   --  Note that in GPR2 we have a distinction between sources, that may
   --  define either the spec or body/ies for a unit - those definitions are
   --  represented as Compil_Unit records, and the unit itself which
   --  is just a name (the Unit_Name).

   Undefined : constant Object :=
                 Object'(Unit_Name    => Null_Unbounded_String,
                         Index        => 0,
                         Kind         => S_Spec,
                         Withed_Units => <>,
                         Is_Sep_From  => <>);

   function Create
     (Unit_Name    : Name_Type;
      Index        : Positive;
      Kind         : Kind_Type;
      Withed_Units : Source_Reference.Identifier.Set.Object;
      Is_Sep_From  : Optional_Name_Type) return Object is
     (Object'(Unit_Name    => To_Unbounded_String (String (Unit_Name)),
              Index        => Index,
              Kind         => Kind,
              Withed_Units => Withed_Units,
              Is_Sep_From  => To_Unbounded_String (String (Is_Sep_From))));

   function Unit_Name (Self : Object) return Name_Type is
     (Name_Type (To_String (Self.Unit_Name)));

   function Index (Self : Object) return Positive is
     (Self.Index);

   function Is_Separate (Self : Object) return Boolean is
     (Self.Kind = S_Separate);

   function Is_Separate_From (Self : Object) return Name_Type is
     (Name_Type (To_String (Self.Is_Sep_From)));

   function Kind (Self : Object) return Kind_Type is
     (Self.Kind);

   function Withed_Units
     (Self : Object) return Source_Reference.Identifier.Set.Object
   is
     (Self.Withed_Units);

end GPR2.Compilation_Unit;
