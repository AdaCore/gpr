------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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

with Ada.Strings.Equal_Case_Insensitive; use Ada.Strings;

package body GPR2.Project.Attribute is

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
      Self.Index_Case_Sensitive := Index_Is_Case_Sensitive;
      Self.Value_Case_Sensitive := Value_Is_Case_Sensitive;
   end Set_Case;

   -----------------
   -- Value_Equal --
   -----------------

   function Value_Equal (Self : Object; Value : Value_Type) return Boolean is
   begin
      if Self.Value_Case_Sensitive then
         return Self.Value = String (Value);
      else
         return Equal_Case_Insensitive (Self.Value, String (Value));
      end if;
   end Value_Equal;

end GPR2.Project.Attribute;
