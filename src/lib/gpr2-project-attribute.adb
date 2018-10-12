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

package body GPR2.Project.Attribute is

   use Ada.Strings;

   ------------
   -- Create --
   ------------

   function Create
     (Name  : Name_Type;
      Index : Value_Type;
      Value : Value_Type;
      Sloc  : Source_Reference.Object) return Object is
   begin
      return A : Object := Create (Name, Value, Sloc) do
         A.Index := To_Unbounded_String (String (Index));
      end return;
   end Create;

   function Create
     (Name   : Name_Type;
      Index  : Value_Type;
      Values : Containers.Value_List;
      Sloc   : Source_Reference.Object) return Object is
   begin
      return A : Object := Create (Name, Values, Sloc) do
         A.Index := To_Unbounded_String (String (Index));
      end return;
   end Create;

   overriding function Create
     (Name  : Name_Type;
      Value : Value_Type;
      Sloc  : Source_Reference.Object) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Value, Sloc)
         with Index                => Null_Unbounded_String,
              Index_Case_Sensitive => True,
              Value_Case_Sensitive => True);
   end Create;

   overriding function Create
     (Name   : Name_Type;
      Values : Containers.Value_List;
      Sloc   : Source_Reference.Object) return Object is
   begin
      return Object'
        (Name_Values.Create (Name, Values, Sloc)
         with Index                => Null_Unbounded_String,
              Index_Case_Sensitive => True,
              Value_Case_Sensitive => True);
   end Create;

   ---------------
   -- Has_Index --
   ---------------

   function Has_Index (Self : Object) return Boolean is
   begin
      return Self.Index /= Null_Unbounded_String;
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

      Name   : constant String := String (Self.Name);
      Result : Unbounded_String := To_Unbounded_String ("for ");
   begin
      Append (Result, Name);

      if Name_Len > 0 and then Name'Length < Name_Len then
         Append (Result, (Name_Len - Name'Length) * ' ');
      end if;

      if Self.Has_Index then
         Append (Result, " (""" & To_String (Self.Index) & """)");
      end if;

      Append (Result, " use ");

      case Self.Kind is
         when Single =>
            Append (Result, '"' & Self.Value & '"');

         when List =>
            Append (Result, Containers.Image (Self.Values));
      end case;

      Append (Result, ';');

      return To_String (Result);
   end Image;

   -----------
   -- Index --
   -----------

   function Index (Self : Object) return Value_Type is
   begin
      return To_String (Self.Index);
   end Index;

   -----------------
   -- Index_Equal --
   -----------------

   function Index_Equal (Self : Object; Value : Value_Type) return Boolean is
   begin
      if Self.Index_Case_Sensitive then
         return To_String (Self.Index) = String (Value);
      else
         return Equal_Case_Insensitive
           (To_String (Self.Index), String (Value));
      end if;
   end Index_Equal;

   --------------
   -- Set_Case --
   --------------

   procedure Set_Case
     (Self                    : in out Object;
      Index_Is_Case_Sensitive : Boolean;
      Value_Is_Case_Sensitive : Boolean) is
   begin
      Name_Values.Object (Self).Set_Case (Value_Is_Case_Sensitive);
      Self.Index_Case_Sensitive := Index_Is_Case_Sensitive;
   end Set_Case;

end GPR2.Project.Attribute;
