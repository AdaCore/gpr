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

--  This package represents a unit object. This is useful for unit-based
--  language like Ada. Note that we associate a spec with multiple bodies
--  as we can have a main body and a set of separate source.

with Ada.Strings.Unbounded;

with GPR2.Project.Source.Set;

package GPR2.Unit is

   use type Project.Source.Object;

   type Object is tagged private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Name      : Name_Type;
      Spec      : Project.Source.Object;
      Main_Body : Project.Source.Object;
      Separates : Project.Source.Set.Object) return Object;
   --  Constructor for a Unit object

   function Has_Spec (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a spec is defined for this unit

   function Has_Body (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a body is defined for this unit

   function Spec (Self : Object) return Project.Source.Object
     with Pre => Self.Is_Defined;
   --  Returns the Spec

   function Main_Body (Self : Object) return Project.Source.Object
     with Pre => Self.Is_Defined;
   --  Returns the Body

   function Separates (Self : Object) return Project.Source.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns all separates

   procedure Update_Spec
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;
   --  Sets unit spec

   procedure Update_Body
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;
   --  Sets unit body

   procedure Update_Separates
     (Self : in out Object; Source : Project.Source.Object)
     with Pre => Source.Is_Defined;
   --  Appends separate

   function Is_Interface (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the unit is an interface

   function Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the unit name

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Name      : Unbounded_String;
      Spec      : Project.Source.Object;
      Main_Body : Project.Source.Object;
      Separates : Project.Source.Set.Object;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Has_Spec (Self : Object) return Boolean is
     (Self.Spec.Is_Defined);

   function Has_Body (Self : Object) return Boolean is
     (Self.Main_Body.Is_Defined);

   function Spec
     (Self : Object) return Project.Source.Object is (Self.Spec);

   function Main_Body
     (Self : Object) return Project.Source.Object is (Self.Main_Body);

   function Separates
     (Self : Object) return Project.Source.Set.Object is (Self.Separates);

   function Is_Interface (Self : Object) return Boolean is
     (Self.Spec.Is_Interface);

   function Name (Self : Object) return Name_Type is
     (Name_Type (To_String (Self.Name)));

end GPR2.Unit;
