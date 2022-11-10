------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Unbounded;

with GPR2.Path_Name;

package GPRname.Common is

   use Ada.Strings.Unbounded;

   use GPR2;

   type Verbosity_Level_Type is (None, Low, High);

   --
   --  String types
   --

   function "+"
     (Str : String) return Unbounded_String renames To_Unbounded_String;

   type Value_Case_Insens is new Value_Type;

   overriding function "=" (Left, Right : Value_Case_Insens) return Boolean is
     (Ada.Strings.Equal_Case_Insensitive (String (Left), String (Right)));

   overriding function "<" (Left, Right : Value_Case_Insens) return Boolean is
     (Ada.Strings.Less_Case_Insensitive (String (Left), String (Right)));

   function Str_Hash_Case_Insensitive
     (Key : Value_Case_Insens) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (String (Key)));

   subtype Pattern_Type is String
     with Dynamic_Predicate => Pattern_Type'Length > 0;

   subtype Language_Type is Value_Case_Insens;

   --
   --  Containers
   --

   package String_Vector is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package String_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (String, "<", "=");

   package Language_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (Language_Type, "<", "=");

   package Language_Vector is
     new Ada.Containers.Indefinite_Vectors (Positive, Language_Type);

   package Path_Name_Vector is new Ada.Containers.Indefinite_Vectors
     (Positive, Path_Name.Object, Path_Name."=");

   package Path_Name_Set is new Ada.Containers.Indefinite_Ordered_Sets
     (Path_Name.Object, Path_Name."<", Path_Name."=");

   --
   --  Constants
   --

   No_String : constant String := "";

   Ada_Lang : constant Language_Type := "Ada";

   --
   --  Exceptions
   --

   GPRname_Exception : exception;

end GPRname.Common;
