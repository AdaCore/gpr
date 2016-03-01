------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

package GPR2.Project.Name_Values is

   use type Containers.Count_Type;

   type Object is tagged private;

   type Kind_Type is (K_Single, K_List);
   --  Either a single value or a list of values

   Undefined : constant Object;

   function Create
     (Name  : Name_Type;
      Value : Value_Type) return Object
     with Post => Create'Result.Kind = K_Single
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = 1;
   --  Create a single-valued object

   function Create
     (Name   : Name_Type;
      Values : Containers.Value_List) return Object
     with Post => Create'Result.Kind = K_List
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = Values.Length;
   --  Create a multi-valued object

   function Kind (Self : Object'Class) return Kind_Type
     with Pre => Object (Self) /= Undefined;
   --  Returns the Kind for the Name/Values pair object

   function Name (Self : Object) return Name_Type
     with Pre => Self /= Undefined;
   --  Returns the name of the Name/Value pair object

   function Count_Values (Self : Object) return Containers.Count_Type
     with Pre  => Self /= Undefined,
          Post =>
            (if Self.Kind = K_Single then Count_Values'Result = 1);
   --  Returns the number of values for the Name/Values pair object

   function Values (Self : Object) return Containers.Value_List
     with Pre  => Self /= Undefined,
          Post => Values'Result.Length = Self.Count_Values;
   --  Returns the values for the Name/Values pair object

   function Value (Self : Object) return Value_Type
     with Pre  => Self /= Undefined and then Self.Kind = K_Single;
   --  Returns the value for the Name/Values pair object

private

   type Object is tagged record
      Kind   : Kind_Type;
      Name   : Unbounded_String;
      Values : Containers.Value_List;
   end record;

   Undefined : constant Object := Object'(others => <>);

end GPR2.Project.Name_Values;
