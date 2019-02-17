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

with Ada.Characters.Handling;

with GPR2.Containers;
with GPR2.Project.Name_Values;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

package GPR2.Project.Attribute is

   use type Containers.Count_Type;
   use all type Registry.Attribute.Value_Kind;

   type Object is new Name_Values.Object with private;

   subtype Project_Attribute is Object;

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Name  : Source_Reference.Identifier.Object;
      Index : Source_Reference.Value.Object;
      Value : Source_Reference.Value.Object) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name = Name.Text
                  and then Create'Result.Count_Values = 1;
   --  Creates a single-valued object

   function Create
     (Name   : Source_Reference.Identifier.Object;
      Index  : Source_Reference.Value.Object;
      Values : Containers.Source_Value_List) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name = Name.Text
                  and then Create'Result.Count_Values = Values.Length;
   --  Creates a multi-valued object

   overriding function Create
     (Name  : Source_Reference.Identifier.Object;
      Value : Source_Reference.Value.Object) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name = Name.Text
                  and then Create'Result.Count_Values = 1;
   --  Creates a single-valued object

   overriding function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name = Name.Text
                  and then Create'Result.Count_Values = Values.Length;
   --  Creates a multi-valued object

   function Has_Index (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the attribute has an index

   function Index (Self : Object) return Source_Reference.Value.Object
     with Inline, Pre => Self.Is_Defined;
   --  Returns the attribute's index value

   function Index_Equal (Self : Object; Value : Value_Type) return Boolean;
   --  Returns True if the attribute's index is equal to Value taking into
   --  account the case-sensitivity of the index.

   procedure Set_Case
     (Self                    : in out Object;
      Index_Is_Case_Sensitive : Boolean;
      Value_Is_Case_Sensitive : Boolean);
   --  Sets attribute case sensitivity for the index and the value.
   --  By default both are case-sensitive.

   overriding function Image
     (Self : Object; Name_Len : Natural := 0) return String;
   --  Returns a string representation. The attribute name is represented with
   --  Name_Len characters (right padding with space) except if Name_Len is 0.

   Default_Source_Dirs : constant Object;

private

   type Object is new Name_Values.Object with record
      Index                : Source_Reference.Value.Object;
      Index_Case_Sensitive : Boolean := True;
   end record;

   function Case_Aware_Index (Self : Object) return Value_Type is
     (if Self.Index_Case_Sensitive
      then Self.Index.Text
      else Ada.Characters.Handling.To_Lower (Self.Index.Text));
   --  Returns Index in lower case if index is case insensitive, returns as is
   --  otherwise.

   Undefined : constant Object := (Name_Values.Undefined with others => <>);

   Current_Dir : constant Source_Reference.Value.Object :=
                   Source_Reference.Value.Object
                     (Source_Reference.Value.Create
                        (Source_Reference.Undefined, "."));

   Default_Source_Dirs : constant Object :=
                           (Name_Values.Create
                             (Source_Reference.Identifier.Object
                                (Source_Reference.Identifier.Create
                                   (Source_Reference.Undefined,
                                    Project.Registry.Attribute.Source_Dirs)),
                               Containers.Source_Value_Type_List.To_Vector
                                 (Current_Dir, 1))
                              with Source_Reference.Value.Undefined, True);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Project.Attribute;
