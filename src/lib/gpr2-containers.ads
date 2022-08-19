--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Some common containers for Name, Value

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;

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
     (Value     : Value_Type;
      Separator : Value_Not_Empty) return Containers.Value_List;
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

   package Attr_Value_Map_Package is
     new Ada.Containers.Indefinite_Ordered_Maps (Attribute_Id, Value_Type);
   subtype Attr_Value_Map is Attr_Value_Map_Package.Map;

   package Name_Value_Map_Package is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, Value_Type);

   subtype Name_Value_Map is Name_Value_Map_Package.Map;

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

   package Attribute_Id_Type_List is new Ada.Containers.Ordered_Sets
     (Q_Attribute_Id);

   subtype Attribute_Id_List is Attribute_Id_Type_List.Set;

   package Package_Id_Type_List is new Ada.Containers.Ordered_Sets
     (Package_Id);

   subtype Package_Id_List is Package_Id_Type_List.Set;

   package Language_Id_Set is new Ada.Containers.Hashed_Sets
     (Language_Id, Hash, "=");
   subtype Language_Set is Language_Id_Set.Set;

   Empty_Language_Set : Language_Set renames Language_Id_Set.Empty_Set;

   package Lang_Value_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Language_Id, Value_Type);
   subtype Lang_Value_Map is Lang_Value_Maps.Map;

   function Value_Or_Default
     (Map     : Lang_Value_Map;
      Key     : Language_Id;
      Default : Value_Type := No_Value) return Value_Type
     with Post => (if not Map.Contains (Key)
                   then Value_Or_Default'Result = Default);

end GPR2.Containers;
