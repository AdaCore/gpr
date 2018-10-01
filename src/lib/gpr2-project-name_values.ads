------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package shares the implementation part of attributes and variables.
--  That is it abstracts out a name associated with a single value or a list
--  of values (possibly none).

with GPR2.Containers;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference;

private with Ada.Strings.Unbounded;

package GPR2.Project.Name_Values is

   use type Containers.Count_Type;
   use all type Registry.Attribute.Value_Kind;

   type Object is new Source_Reference.Object with private;

   Undefined : constant Object;

   subtype Value_Kind is Registry.Attribute.Value_Kind;

   function Create
     (Name  : Name_Type;
      Value : Value_Type;
      Sloc  : Source_Reference.Object) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = 1;
   --  Create a single-valued object

   function Create
     (Name   : Name_Type;
      Values : Containers.Value_List;
      Sloc   : Source_Reference.Object) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = Values.Length;
   --  Create a multi-valued object

   function Kind (Self : Object'Class) return Registry.Attribute.Value_Kind
     with Pre => Object (Self) /= Undefined;
   --  Returns the Kind for the Name/Values pair object

   function Name (Self : Object) return Name_Type
     with Pre => Self /= Undefined;
   --  Returns the name of the Name/Value pair object

   function Count_Values (Self : Object) return Containers.Count_Type
     with Pre  => Self /= Undefined,
          Post => (if Self.Kind = Single then Count_Values'Result = 1);
   --  Returns the number of values for the Name/Values pair object

   function Values (Self : Object) return Containers.Value_List
     with Pre  => Self /= Undefined,
          Post => Values'Result.Length = Self.Count_Values;
   --  Returns the values for the Name/Values pair object

   function Value (Self : Object) return Value_Type
     with Pre => Self /= Undefined and then Self.Kind = Single;
   --  Returns the value for the Name/Values pair object

   function Image (Self : Object; Name_Len : Natural := 0) return String;
   --  Returns a string representation. Name_Len represents the length in
   --  character than the Name should take, so possibly some space padding
   --  are added.

private

   use Ada.Strings.Unbounded;

   type Object is new Source_Reference.Object with record
      Kind   : Registry.Attribute.Value_Kind := List;
      Name   : Unbounded_String;
      Values : Containers.Value_List;
   end record;

   Undefined : constant Object :=
                 Object'(Source_Reference.Undefined with others => <>);

end GPR2.Project.Name_Values;
