------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

package body GPR2.Project.Name_Values is

   function Build_Map (Values : Containers.Source_Value_List)
                       return Containers.Value_Source_Reference;
   --  Returns a set with the value in values

   ---------------
   -- Build_Set --
   ---------------

   function Build_Map (Values : Containers.Source_Value_List)
                       return Containers.Value_Source_Reference is
   begin
      return R : Containers.Value_Source_Reference do
         for V of Values loop
            R.Include (V.Text, V);
         end loop;
      end return;
   end Build_Map;

   ------------------
   -- Count_Values --
   ------------------

   function Count_Values (Self : Object) return Containers.Count_Type is
   begin
      return Self.Values.Length;
   end Count_Values;

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Source_Reference.Identifier.Object;
      Value : Source_Reference.Value.Object) return Object
   is
      Sloc   : constant Source_Reference.Object :=
                 Source_Reference.Object (Name);
      Values : constant Containers.Source_Value_List :=
                 Containers.Source_Value_Type_List.To_Vector
                   (Value, 1);
   begin
      return Object'
        (Sloc
         with Single,
              Name, Values, Build_Map (Values));
   end Create;

   function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List) return Object is
   begin
      return Object'
        (Source_Reference.Object (Name)
         with List,
         Name, Values, Build_Map (Values));
   end Create;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object'Class) return Registry.Attribute.Value_Kind is
   begin
      return Self.Kind;
   end Kind;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Source_Reference.Identifier.Object is
   begin
      return Self.Name;
   end Name;

   -----------
   -- Value --
   -----------

   function Value (Self : Object) return Source_Reference.Value.Object is
   begin
      return Self.Values.First_Element;
   end Value;

   ------------
   -- Values --
   ------------

   function Values (Self : Object) return Containers.Source_Value_List is
   begin
      return Self.Values;
   end Values;

end GPR2.Project.Name_Values;
