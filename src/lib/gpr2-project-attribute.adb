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

package body GPR2.Project.Attribute is

   ------------
   -- Create --
   ------------

   function Create
     (Index          : Attribute_Index.Object;
      Default_At_Num : Natural := 0) return Value_At_Num
   is
      Is_Others : constant Boolean    :=
                    Index.Is_Defined and then Index.Is_Others;
      Value     : constant Value_Type :=
                    (if Index.Is_Defined
                     then Index.Value (Index.Is_Case_Sensitive)
                     else "");
      Size      : constant Natural   :=
                    Value'Length + (if Is_Others then 1 else 0);
   begin
      return V : Value_At_Num (Size) do
         V.Value := (if Is_Others then "@" else "") & Value;
         V.At_Num := At_Num_Or
                       (Source_Reference.Value.Object (Index), Default_At_Num);
      end return;
   end Create;

   function Create
     (Name    : Source_Reference.Identifier.Object;
      Index   : Attribute_Index.Object;
      Value   : Source_Reference.Value.Object;
      Default : Boolean) return Object is
   begin
      return A : Object := Create (Name, Value) do
         A.Index   := Index;
         A.Default := Default;
      end return;
   end Create;

   function Create
     (Name  : Source_Reference.Identifier.Object;
      Index : Attribute_Index.Object;
      Value : Source_Reference.Value.Object) return Object is
   begin
      return A : Object := Create (Name, Value) do
         A.Index := Index;
      end return;
   end Create;

   function Create
     (Name    : Source_Reference.Identifier.Object;
      Index   : Attribute_Index.Object;
      Values  : Containers.Source_Value_List;
      Default : Boolean := False) return Object is
   begin
      return A : Object := Create (Name, Values) do
         A.Index   := Index;
         A.Default := Default;
      end return;
   end Create;

   overriding function Create
     (Name  : Source_Reference.Identifier.Object;
      Value : Source_Reference.Value.Object) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Value)
         with Index   => Attribute_Index.Undefined,
              Default => False);
   end Create;

   function Create
     (Name    : Source_Reference.Identifier.Object;
      Value   : Source_Reference.Value.Object;
      Default : Boolean) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Value)
         with Index   => Attribute_Index.Undefined,
              Default => Default);
   end Create;

   overriding function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Values)
         with Index   => Attribute_Index.Undefined,
              Default => False);
   end Create;

   function Create
     (Name    : Source_Reference.Identifier.Object;
      Values  : Containers.Source_Value_List;
      Default : Boolean) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Values)
         with Index   => Attribute_Index.Undefined,
              Default => Default);
   end Create;

   ---------------
   -- Has_Index --
   ---------------

   function Has_Index (Self : Object) return Boolean is
   begin
      return Self.Index.Is_Defined;
   end Has_Index;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Self     : Object;
      Name_Len : Natural := 0) return String
   is
      use GPR2.Project.Registry.Attribute;
      use all type GPR2.Project.Name_Values.Object;

      Name   : constant String := String (Self.Name.Text);
      Result : Unbounded_String := To_Unbounded_String ("for ");
   begin
      Append (Result, Name);

      if Name_Len > 0 and then Name'Length < Name_Len then
         Append (Result, (Name_Len - Name'Length) * ' ');
      end if;

      if Self.Has_Index then
         Append (Result, " (""" & Self.Index.Text & """)");
      end if;

      Append (Result, " use ");

      case Self.Kind is
         when Single =>
            Append (Result, '"' & Self.Value.Text & '"');

            if Self.Value.Has_At_Num then
               Append (Result, " at" & Integer'Image (Self.Value.At_Num));
            end if;

         when List =>
            Append (Result, Containers.Image (Self.Values));
      end case;

      Append (Result, ';');

      return To_String (Result);
   end Image;

   -----------
   -- Index --
   -----------

   function Index (Self : Object) return Attribute_Index.Object is
   begin
      return Self.Index;
   end Index;

   ------------
   -- Rename --
   ------------

   overriding function Rename
     (Self : Object;
      Name : Source_Reference.Identifier.Object) return Object is
   begin
      return (Name_Values.Object (Self).Rename (Name) with
                Default => True,
                Index   => Self.Index);
   end Rename;

   --------------
   -- Set_Case --
   --------------

   procedure Set_Case
     (Self                    : in out Object;
      Index_Is_Case_Sensitive : Boolean;
      Value_Is_Case_Sensitive : Boolean) is
   begin
      Self.Set_Case (Value_Is_Case_Sensitive);

      if Self.Has_Index then
         Self.Index.Set_Case (Index_Is_Case_Sensitive);
      end if;
   end Set_Case;

end GPR2.Project.Attribute;
