------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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

--  Some common containers for Name, Value

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with GPR2.Source_Reference.Value;

package GPR2.Containers is

   subtype Count_Type is Ada.Containers.Count_Type;

   package Name_Type_List is
     new Ada.Containers.Indefinite_Vectors (Positive, Name_Type);

   subtype Name_List is Name_Type_List.Vector;

   package Name_Type_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (Name_Type);

   subtype Name_Set is Name_Type_Set.Set;

   Empty_Name_Set : Name_Set renames Name_Type_Set.Empty_Set;

   package Filename_Type_List is
     new Ada.Containers.Indefinite_Vectors (Positive, Filename_Type);

   subtype Filename_List is Filename_Type_List.Vector;

   package Filename_Type_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (Filename_Type);

   subtype Filename_Set is Filename_Type_Set.Set;

   Empty_Filename_Set : Filename_Set renames Filename_Type_Set.Empty_Set;

   package Value_Type_List is
     new Ada.Containers.Indefinite_Vectors (Positive, Value_Type);

   subtype Value_List is Value_Type_List.Vector;

   function Create
     (Value     : Name_Type;
      Separator : Name_Type) return Containers.Value_List;
   --  Create a Value_List out of a set of Value separated by Separator

   function Create
     (Value     : Name_Type;
      Separator : Name_Type) return Containers.Name_List;
   --  As above but for a list of names

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

   function Image (Values : Source_Value_List) return String;
   --  Returns a string representation of the list of values

   package Value_Source_Reference_Package is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Value_Type, Source_Reference.Value.Object);

   subtype Value_Source_Reference is Value_Source_Reference_Package.Map;

   package Filename_Source_Reference_Package is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Filename_Type, Source_Reference.Value.Object);

   subtype Filename_Source_Reference is Filename_Source_Reference_Package.Map;

   package Source_Value_Type_Set is new Ada.Containers.Indefinite_Ordered_Sets
     (Source_Reference.Value.Object);

   subtype Source_Value_Set is Source_Value_Type_Set.Set;

end GPR2.Containers;
