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

with GPR2.Containers;
with GPR2.Project.Name_Values;

private with Ada.Strings.Unbounded;

package GPR2.Project.Attribute is

   use type GPR2.Project.Name_Values.Kind_Type;
   use type GPR2.Containers.Count_Type;

   type Object is new Name_Values.Object with private;

   subtype Project_Attribute is Object;

   Undefined : constant Object;

   function Create
     (Name  : Name_Type;
      Index : Name_Type;
      Value : Value_Type) return Object
     with Post => Create'Result.Kind = Name_Values.K_Single
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = 1;
   --  Create a single-valued object

   function Create
     (Name   : Name_Type;
      Index  : Name_Type;
      Values : Containers.Value_List) return Object
     with Post => Create'Result.Kind = Name_Values.K_List
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = Values.Length;
   --  Create a multi-valued object

   overriding function Create
     (Name  : Name_Type;
      Value : Value_Type) return Object
     with Post => Create'Result.Kind = Name_Values.K_Single
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = 1;
   --  Create a single-valued object

   overriding function Create
     (Name   : Name_Type;
      Values : Containers.Value_List) return Object
     with Post => Create'Result.Kind = Name_Values.K_List
                  and then Create'Result.Name = Name
                  and then Create'Result.Count_Values = Values.Length;
   --  Create a multi-valued object

   function Has_Index (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the attribute has an index

   function Index (Self : Object) return Name_Type
     with Pre => Self /= Undefined;
   --  Returns the attribute's index value

private

   use Ada.Strings.Unbounded;

   type Object is new Name_Values.Object with record
      Index : Unbounded_String;
   end record;

   Undefined : constant Object :=
                 (Name_Values.Undefined with Null_Unbounded_String);

end GPR2.Project.Attribute;
