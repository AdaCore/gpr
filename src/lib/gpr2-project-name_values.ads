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

--  This package shares the implementation part of attributes and variables.
--  That is it abstracts out a name associated with a single value or a list
--  of values (possibly none).

with GPR2.Containers;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

private with Ada.Strings.Unbounded;

package GPR2.Project.Name_Values is

   use type Containers.Count_Type;
   use all type Registry.Attribute.Value_Kind;

   type Object is new Source_Reference.Object with private;

   Undefined : constant Object;

   subtype Value_Kind is Registry.Attribute.Value_Kind;

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Name  : Source_Reference.Identifier.Object;
      Value : Source_Reference.Value.Object) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name = Name.Text
                  and then Create'Result.Count_Values = 1;
   --  Create a single-valued object

   function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name = Name.Text
                  and then Create'Result.Count_Values = Values.Length;
   --  Create a multi-valued object

   function Kind (Self : Object'Class) return Registry.Attribute.Value_Kind
     with Pre => Object (Self).Is_Defined;
   --  Returns the Kind for the Name/Values pair object

   function Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the name of the Name/Value pair object

   function Count_Values (Self : Object) return Containers.Count_Type
     with Pre  => Self.Is_Defined,
          Post => (if Self.Kind = Single then Count_Values'Result = 1);
   --  Returns the number of values for the Name/Values pair object

   function Values (Self : Object) return Containers.Source_Value_List
     with Pre  => Self.Is_Defined,
          Post => Values'Result.Length = Self.Count_Values;
   --  Returns the values for the Name/Values pair object

   function Has_Value (Self : Object; Value : Value_Type) return Boolean
     with Pre => Self.Kind = List;
   --  Returns true whether the list of value contains Value

   function Value (Self : Object) return Source_Reference.Value.Object
     with Pre => Self.Is_Defined and then Self.Kind = Single;
   --  Returns the value for the Name/Values pair object

   function Value_Equal (Self : Object; Value : Value_Type) return Boolean
     with Pre => Self.Kind = Single;
   --  Returns True if the attribute's value is equal to Value taking into
   --  account the case-sensitivity of the value.

   function Image (Self : Object; Name_Len : Natural := 0) return String;
   --  Returns a string representation. Name_Len represents the length in
   --  character than the Name should take, so possibly some space padding
   --  are added.

   procedure Set_Case
     (Self                    : in out Object;
      Value_Is_Case_Sensitive : Boolean);
   --  Sets values case sensitivity which is by default it is case-sensitive

private

   use Ada.Strings.Unbounded;

   type Object is new Source_Reference.Object with record
      Kind                 : Registry.Attribute.Value_Kind := List;
      Name                 : Unbounded_String;
      Values               : Containers.Source_Value_List;
      Value_Case_Sensitive : Boolean := True;
      V_Set                : Containers.Value_Set;  -- for fast check
   end record;

   Undefined : constant Object :=
                 Object'(Source_Reference.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Project.Name_Values;
