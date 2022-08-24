--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides serialization of common GPR2 data structures into
--  JSON.

with Ada.Calendar;

package GPR2.C.JSON.Encoders is

   -------------------
   --  Simple types --
   -------------------

   function From_Boolean (Value : Boolean) return JSON_Value;
   --  Convert an Ada Boolean into a JSON boolean

   function From_String (Value : String) return JSON_Value;
   --  Convert an Ada String into a JSON string

   function From_Time
      (Value : Ada.Calendar.Time) return JSON_Value;
   --  Convert an Ada.Calendar.Time to JSON. The JSON value is an integer
   --  Unix timestamp.

   function From_Filename (Value : GPR2.Filename_Optional) return JSON_Value;
   --  Convert a Filename into a JSON value. The JSON value is a string that
   --  corresponds to the filename path

   function From_Language_Id (Value : GPR2.Language_Id) return JSON_Value;
   --  Convert a Language_Id into a JSON value. The JSON value is a string
   --  corresponding to the language name. Note that casing is not relevant
   --  for language name but the returned value uses the casing registered in
   --  the GPR knowledge base.

   function From_GPR_Path (Value : GPR_Path) return JSON_Value;
   --  Convert a GPR_Path into a JSON value. The JSON value is a string
   --  representing the absolute path.

   function From_GPR_Paths (Value : GPR_Paths) return JSON_Value;
   --  Convert a list of GPR_Path into a JSON value. The JSON value is a list
   --  of strings each string containing an absolute path.

   function From_GPR_Views (Value : GPR_Views) return JSON_Value;

   function From_GPR_Source (Value : GPR_Source) return JSON_Value;

   function From_GPR_Sources (Value : GPR_Sources) return JSON_Value;

   function From_Name
      (Value : Optional_Name_Type) return JSON_Value;

   function From_Filename_List
      (Value : GPR2.Containers.Filename_List) return JSON_Value;

   function From_Unit_Info (Value : GPR_Unit_Info) return JSON_Value;

   function From_Unit_Infos (Value : GPR_Unit_Infos) return JSON_Value;

   function From_GPR_Tree (Value : GPR_Tree_Access) return JSON_Value;
   --  Sets member Key of Obj to the project tree id of Value

   function From_GPR_View (Value : GPR_View) return JSON_Value;
   --  Sets member Key of Obj to the project view id of Value.
   --  If View is undefined, it is deallocated.

   function From_Project_Kind
      (Value : GPR2.Project_Kind) return JSON_Value;

   function From_GPR_Attribute (Value : GPR_Attribute) return JSON_Value;
   --  Serialize a Project_Attribute as JSON
   --
   --  PROJECT_ATTRIBUTE
   --      {'value': None | str | list[str],
   --       'is_default': bool}

   function From_GPR_Message (Value : GPR_Message) return JSON_Value;
   --  Add message to JSON array Obj

   function From_Source_Reference
     (Value : GPR_Source_Reference) return JSON_Value;
   --  Sets source reference object's members in Obj

   function From_Context (Value : GPR_Context) return JSON_Value;
   --  Set member Key of Obj to context Context. A context is serialized as
   --  a JSON object.

end GPR2.C.JSON.Encoders;
