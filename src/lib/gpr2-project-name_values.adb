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

with Ada.Strings.Equal_Case_Insensitive;
with Ada.Characters.Handling;

package body GPR2.Project.Name_Values is

   function Build_Set
     (Values         : Containers.Value_List;
      Case_Sensitive : Boolean) return Containers.Value_Set;
   --  Returns a set with the value in values

   ---------------
   -- Build_Set --
   ---------------

   function Build_Set
     (Values         : Containers.Value_List;
      Case_Sensitive : Boolean) return Containers.Value_Set
   is
      use Ada;
   begin
      return R : Containers.Value_Set do
         for V of Values loop
            if Case_Sensitive then
               R.Include (V);
            else
               R.Include (Characters.Handling.To_Lower (V));
            end if;
         end loop;
      end return;
   end Build_Set;

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
     (Name  : Name_Type;
      Value : Value_Type;
      Sloc  : Source_Reference.Object) return Object
   is
      Values : constant Containers.Value_List :=
                 Containers.Value_Type_List.To_Vector (String (Value), 1);
   begin
      return Object'
        (Sloc
         with Single,
              To_Unbounded_String (String (Name)),
              Values, True, Build_Set (Values, True));
   end Create;

   function Create
     (Name   : Name_Type;
      Values : Containers.Value_List;
      Sloc   : Source_Reference.Object) return Object is
   begin
      return Object'
        (Sloc with List,
         To_Unbounded_String (String (Name)),
         Values, True, Build_Set (Values, True));
   end Create;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value (Self : Object; Value : Value_Type) return Boolean is
      use Ada;
   begin
      return Self.V_Set.Contains
        (if Self.Value_Case_Sensitive
         then Value
         else Characters.Handling.To_Lower (Value));
   end Has_Value;

   -----------
   -- Image --
   -----------

   function Image (Self : Object; Name_Len : Natural := 0) return String is
      Result : Unbounded_String := Self.Name;
   begin
      if Name_Len > 0 and then Length (Result) < Name_Len then
         Append (Result, (Name_Len - Integer (Length (Result))) * ' ');
      end if;

      Append (Result, " :");

      for V of Self.Values loop
         Append (Result, ' ' & V);
      end loop;

      return To_String (Result);
   end Image;

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

   function Name (Self : Object) return Name_Type is
   begin
      return Name_Type (To_String (Self.Name));
   end Name;

   --------------
   -- Set_Case --
   --------------

   procedure Set_Case
     (Self                    : in out Object;
      Value_Is_Case_Sensitive : Boolean) is
   begin
      --  Are we changing the casing

      if Value_Is_Case_Sensitive /= Self.Value_Case_Sensitive then
         Self.Value_Case_Sensitive := Value_Is_Case_Sensitive;
         Self.V_Set := Build_Set (Self.Values, Value_Is_Case_Sensitive);
      end if;
   end Set_Case;

   -----------
   -- Value --
   -----------

   function Value (Self : Object) return Value_Type is
   begin
      return Self.Values.First_Element;
   end Value;

   -----------------
   -- Value_Equal --
   -----------------

   function Value_Equal (Self : Object; Value : Value_Type) return Boolean is
      use Ada.Strings;
   begin
      if Self.Value_Case_Sensitive then
         return Self.Value = String (Value);
      else
         return Equal_Case_Insensitive (Self.Value, String (Value));
      end if;
   end Value_Equal;

   ------------
   -- Values --
   ------------

   function Values (Self : Object) return Containers.Value_List is
   begin
      return Self.Values;
   end Values;

end GPR2.Project.Name_Values;
