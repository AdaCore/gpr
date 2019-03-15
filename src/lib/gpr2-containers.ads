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

--  Some common containers for Name, Value

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with GPR2.Path_Name;
with GPR2.Source_Reference.Value;

package GPR2.Containers is

   subtype Count_Type is Ada.Containers.Count_Type;

   package Name_Type_List is
     new Ada.Containers.Indefinite_Vectors (Positive, Name_Type);

   subtype Name_List is Name_Type_List.Vector;

   package Name_Type_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (Name_Type);

   subtype Name_Set is Name_Type_Set.Set;

   package Value_Type_List is
     new Ada.Containers.Indefinite_Vectors (Positive, Value_Type);

   subtype Value_List is Value_Type_List.Vector;

   subtype Extended_Index is Value_Type_List.Extended_Index;

   function Image (Values : Value_List) return String;
   --  Returns a string representation of the list of values

   package Value_Type_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (Value_Type);

   subtype Value_Set is Value_Type_Set.Set;

   package Name_Value_Map_Package is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, Value_Type);

   subtype Name_Value_Map is Name_Value_Map_Package.Map;

   function Value_Or_Default
     (Map     : Name_Value_Map;
      Key     : Name_Type;
      Default : Value_Type := No_Value) return Value_Type
     with Post => (if not Map.Contains (Key)
                   then Value_Or_Default'Result = Default);
   --  Returns value by key if exists or Default value if key not found

   function "=" (Left, Right : Source_Reference.Value.Object) return Boolean
     renames GPR2.Source_Reference.Value."=";

   function "<" (Left, Right : Source_Reference.Value.Object) return Boolean
     renames GPR2.Source_Reference.Value."<";

   package Source_Value_Type_List is new Ada.Containers.Indefinite_Vectors
     (Positive, Source_Reference.Value.Object);

   subtype Source_Value_List is Source_Value_Type_List.Vector;

   package Value_Source_Reference_Package is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Value_Type, Source_Reference.Value.Object);

   subtype Value_Source_Reference is Value_Source_Reference_Package.Map;

   function Image (Values : Source_Value_List) return String;
   --  Returns a string representation of the list of values

   package Source_Value_Type_Set is new Ada.Containers.Indefinite_Ordered_Sets
     (Source_Reference.Value.Object);

   subtype Source_Value_Set is Source_Value_Type_Set.Set;

end GPR2.Containers;
