--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling;
with Ada.Strings.Equal_Case_Insensitive;

package body GPR2.Project.Attr_Values is

   function Build_Map
     (Values         : Containers.Source_Value_List;
      Case_Sensitive : Boolean) return Containers.Value_Source_Reference;
   --  Returns a set with the value in values

   ---------------
   -- Build_Set --
   ---------------

   function Build_Map
     (Values         : Containers.Source_Value_List;
      Case_Sensitive : Boolean) return Containers.Value_Source_Reference is
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
     (Name  : Source_Reference.Attribute.Object;
      Value : Source_Reference.Value.Object) return Object
   is
      Values : constant Containers.Source_Value_List :=
                 Containers.Source_Value_Type_List.To_Vector
                   (Value, 1);
   begin
      return Object'
        (Name
         with Single,
              Values, True, Build_Map (Values, True));
   end Create;

   function Create
     (Name   : Source_Reference.Attribute.Object;
      Values : Containers.Source_Value_List) return Object is
   begin
      return Object'
        (Name
         with List,
         Values, True, Build_Map (Values, True));
   end Create;

   ----------------
   -- Ensure_Set --
   ----------------

   procedure Ensure_Set (Self : in out Object)
   is
      V2    : Containers.Source_Value_List;
      VMap2 : Containers.Value_Source_Reference;
      C     : Containers.Source_Value_Type_List.Cursor;
   begin
      if Self.Kind = Single then
         return;
      end if;

      for V of Self.Values loop
         declare
            Text     : constant String :=
                         (if Self.Value_Case_Sensitive
                          then V.Text
                          else Characters.Handling.To_Lower (V.Text));
            Cmap     : Containers.Value_Source_Reference_Package.Cursor;
            Inserted : Boolean;
         begin
            VMap2.Insert (Text, V, Cmap, Inserted);

            if not Inserted then
               --  Replace with the newer value
               C := V2.Find (VMap2.Element (Text));
               V2.Delete (C);

               VMap2.Replace_Element (Cmap, V);
            end if;

            V2.Append (V);
         end;
      end loop;

      Self.Values := V2;
      Self.V_Map  := VMap2;
   end Ensure_Set;

   ---------------
   -- Has_Value --
   ---------------

   function Has_Value (Self : Object; Value : Value_Type) return Boolean is
   begin
      return Self.V_Map.Contains
        (if Self.Value_Case_Sensitive
         then Value
         else Characters.Handling.To_Lower (Value));
   end Has_Value;

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

   function Name (Self : Object) return Source_Reference.Attribute.Object is
   begin
      return Source_Reference.Attribute.Object (Self);
   end Name;

   --------------------
   -- Prepend_Vector --
   --------------------

   procedure Prepend_Vector
     (Self : in out Object; Other : Object) is
   begin
      Self.Values.Prepend_Vector (Other.Values);
      for C in Other.V_Map.Iterate loop
         Self.V_Map.Include
           (GPR2.Containers.Value_Source_Reference_Package.Key (C),
            Containers.Value_Source_Reference_Package.Element (C));
      end loop;
   end Prepend_Vector;

   ------------
   -- Rename --
   ------------

   function Rename
     (Self : Object;
      Name : Source_Reference.Attribute.Object) return Object
   is
   begin
      return Object'
        (Name with
           Kind                 => Self.Kind,
           Values               => Self.Values,
           Value_Case_Sensitive => Self.Value_Case_Sensitive,
           V_Map                => Self.V_Map);
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
end GPR2.Project.Attr_Values;
