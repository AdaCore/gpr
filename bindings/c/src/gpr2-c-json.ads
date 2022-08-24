--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Exceptions;
with GNATCOLL.JSON;

with GPR2.C.Utils; use GPR2.C.Utils;
with GPR2.Containers;
with GPR2.Message;
with GPR2.Path_Name.Set;
with GPR2.Project.Configuration;

package GPR2.C.JSON is

   subtype JSON_Value is GNATCOLL.JSON.JSON_Value;
   subtype JSON_Array is GNATCOLL.JSON.JSON_Array;

   function Decode (Request : C_Request) return JSON_Value;
   --  Decodes a C_Request (char * in C) into a JSON_Value

   function Encode (Answer : JSON_Value) return C_Answer;
   --  Encodes a JSON_Value into a C_Answer (char * in C)

   type Bind_Handler is access procedure
     (Request : JSON_Value; Result : JSON_Value);

   function Bind
     (Request : C_Request;
      Answer  : out C_Answer;
      Handler : Bind_Handler) return C_Status;
   --  Takes care of the boilerplate that decodes the incoming request and
   --  encodes the answer. Handler function receives the decoded JSON and
   --  returns the 'result' part of the answer.
   --  Bind catches all exceptions and sets properly the error in the answer.
   --  Note that Result is in fact an in/out parameter. When the handler is
   --  called, the Result JSON_Value is initialized as an empty JSON object
   --  (i.e. dictionary).

   -----------------------
   -- Decoding requests --
   -----------------------

   type GPR_Tree_Access is access all GPR_Tree;
   pragma No_Strict_Aliasing (GPR_Tree_Access);

   type Project_Configuration_Access is
     access all GPR2.Project.Configuration.Object;
   pragma No_Strict_Aliasing (Project_Configuration_Access);

   function Get (Obj : JSON_Value; Key : String) return JSON_Value;
   --  Returnes member Key of JSON object Obj. Return Null object if the member
   --  cannot be found.

   function To_Boolean (Obj : JSON_Value; Default : Boolean) return Boolean;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a boolean.
   --  If Obj does not have a Key member then Default is returned.
   --  If the Key member exists but is not a boolean then an exception is
   --  raised.

   function To_Context (Obj : JSON_Value) return GPR_Context;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a JSON object for which all members values are strings. The returned
   --  object is a GPR_Context.
   --  If the member does not exist or its value does not comply with the
   --  expected structure an exception is raised.

   function Get_GPR_Tree
     (Obj : JSON_Value; Key : String) return GPR_Tree_Access;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a JSON string representing a project tree id. The returned object is an
   --  access to a GPR_Tree.
   --  If the member does not exist or its value does not comply with the
   --  expected structure an exception is raised.

   function Get_Status (Obj : JSON_Value) return C_Status;
   --  Returns the "status" member of an answer JSON object

   function Get_Result (Obj : JSON_Value) return JSON_Value;
   --  Returns the "result" member of an answer JSON object. Note that the
   --  returned value is mutable.

   function Get_Level_Format
     (Obj : JSON_Value; Key : String; Default : GPR2.Message.Level_Format)
      return GPR2.Message.Level_Format;
   --  Return the Level_Format corresponding to value found in JSON at 'Key'

   function Get_Level_Output
     (Obj : JSON_Value; Default : GPR2.Message.Level_Output)
      return GPR2.Message.Level_Output;
   --  Return the Level_Output encoded in JSON

   function To_Path_Name (Obj : JSON_Value) return GPR_Path;

   function To_Path_Name_Set
      (Obj : JSON_Value) return GPR2.Path_Name.Set.Object;
   --  Decode a JSON_Value into a Path_Name_Set. The JSON_Value is expected to
   --  be an array of strings.

   function To_Name_Value_Map
      (Obj : JSON_Value) return GPR2.Containers.Name_Value_Map;

   function To_Lang_Value_Map
      (Obj : JSON_Value) return GPR2.Containers.Lang_Value_Map;
   --  Decode a JSON_Value into a Lang_Value_Map object. the expected format is
   --  { "lang1": "value1",
   --    ...
   --    "langN": "valueN"}

   function To_GPR_View
     (Tree : GPR_Tree; Obj : JSON_Value) return GPR_View;
   --  Decode a JSON_Value into a View.Object. The JSON_Value should be a
   --  which is an image of a valid view id.

   function To_Filename
      (Obj : JSON_Value) return GPR2.Filename_Type;

   function To_Language
      (Obj : JSON_Value) return GPR2.Language_Id;

   function To_Unit_Index
      (Obj : JSON_Value; Default : Unit_Index) return Unit_Index;

   function To_String (Obj : JSON_Value) return String;
   function To_String (Obj : JSON_Value; Default : String) return String;

   function To_Name (Obj : JSON_Value) return Optional_Name_Type;

   function Get_Name_Value_Map
     (Obj : JSON_Value; Key : String) return GPR2.Containers.Name_Value_Map;
   --  Return the name value map defined in Key dictionnary

   ---------------------
   -- Encoding answer --
   ---------------------

   function Initialize_Answer return JSON_Value;
   --  Creates an answer JSON object:
   --     {
   --         "result":     {},
   --         "error_msg":  "",
   --         "error_name": "",
   --         "status": 0
   --     }

   procedure Set_Status (Obj : JSON_Value; Value : C_Status);
   --  Sets the status member of an answer JSON object Obj

   procedure Set_Status
     (Obj   : JSON_Value;
      Value : C_Status;
      E     : Ada.Exceptions.Exception_Occurrence);
   --  Sets the status, error_msg and error_name of an answer JSON object Obj

   --  Convertion functions to JSON values

   procedure Set (Obj : JSON_Value; Key : String; Value : JSON_Value);

private

   function To_Name (Obj : JSON_Value) return Optional_Name_Type is
      (Optional_Name_Type (To_String (Obj)));

end GPR2.C.JSON;
