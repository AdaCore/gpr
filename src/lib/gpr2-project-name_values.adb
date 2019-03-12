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
with Ada.Strings.Equal_Case_Insensitive;

package body GPR2.Project.Name_Values is

   function Build_Map
     (Values         : Containers.Source_Value_List;
      Case_Sensitive : Boolean) return Containers.Value_Source_Reference;
   --  Returns a set with the value in values

   ---------------
   -- Build_Set --
   ---------------

   function Build_Map
     (Values         : Containers.Source_Value_List;
      Case_Sensitive : Boolean) return Containers.Value_Source_Reference
   is
      use Ada;
   begin
      return R : Containers.Value_Source_Reference do
         for V of Values loop
            if Case_Sensitive then
               R.Include (V.Text, V);

            else
               R.Include (Characters.Handling.To_Lower (V.Text), V);
            end if;
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
              To_Unbounded_String (String (Name.Text)),
              Values, True, Build_Map (Values, True));
   end Create;

   function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List) return Object is
   begin
      return Object'
        (Source_Reference.Object (Name)
         with List,
         To_Unbounded_String (String (Name.Text)),
         Values, True, Build_Map (Values, True));
   end Create;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value (Self : Object; Value : Value_Type) return Boolean is
      use Ada;
   begin
      return Self.V_Map.Contains
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
         Append (Result, ' ' & V.Text);
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

   ------------
   -- Rename --
   ------------

   function Rename (Self : in out Object; Name : Name_Type) return Object is
      Result : Object := Self;
   begin
      Result.Name := To_Unbounded_String (String (Name));
      return Result;
   end Rename;

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
         Self.V_Map := Build_Map (Self.Values, Value_Is_Case_Sensitive);
      end if;
   end Set_Case;

   -----------
   -- Value --
   -----------

   function Value
     (Self : Object; Value : Value_Type) return Source_Reference.Value.Object
   is
      use Ada;
   begin
      return Self.V_Map ((if Self.Value_Case_Sensitive
                          then Value
                          else Characters.Handling.To_Lower (Value)));
   end Value;

   function Value (Self : Object) return Source_Reference.Value.Object is
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
         return Self.Value.Text = String (Value);
      else
         return Equal_Case_Insensitive (Self.Value.Text, String (Value));
      end if;
   end Value_Equal;

   ------------
   -- Values --
   ------------

   function Values (Self : Object) return Containers.Source_Value_List is
   begin
      return Self.Values;
   end Values;

end GPR2.Project.Name_Values;
