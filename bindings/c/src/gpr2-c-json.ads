------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Exceptions;
with GNATCOLL.JSON;
with GPR2.Containers;
with GPR2.Context;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Project.View.Set;
with GPR2.Source_Reference;

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

   type Project_Tree_Access is access all GPR2.Project.Tree.Object;
   pragma No_Strict_Aliasing (Project_Tree_Access);

   type Project_View_Access is access all GPR2.Project.View.Object;
   pragma No_Strict_Aliasing (Project_View_Access);

   type Project_Configuration_Access is access all
     GPR2.Project.Configuration.Object;
   pragma No_Strict_Aliasing (Project_Configuration_Access);

   function Get_String
      (Obj : JSON_Value; Key : String) return String;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string. If Obj does not have a Key member or if the member is not a
   --  string then an exception is raised.

   function Get_String
      (Obj     : JSON_Value;
       Key     : String;
       Default : String) return String;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string.
   --  If Obj does not have a Key member then Default is returned.
   --  If the Key member exists but is not a string then an exception is
   --  raised.

   function Get_Optional_Name
      (Obj : JSON_Value; Key : String) return GPR2.Optional_Name_Type;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string or null.
   --  If Obj does not have a Key member or member value is null then
   --  No_Name is returned
   --  If the Key member exists but is not a string or null then an
   --  exception is raised.

   function Get_Boolean
      (Obj     : JSON_Value;
       Key     : String;
       Default : Boolean) return Boolean;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a boolean.
   --  If Obj does not have a Key member then Default is returned.
   --  If the Key member exists but is not a boolean then an exception is
   --  raised.

   function Get_File_Path
      (Obj : JSON_Value; Key : String) return GPR2.Path_Name.Object;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string that can be used to constuct a GPR2.Path_Name file.
   --  Exception is raised in the member does not exist or if member's value
   --  is not a string.

   function Get_File_Path
      (Obj     : JSON_Value;
       Key     : String;
       Default : GPR2.Path_Name.Object) return GPR2.Path_Name.Object;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string that can be used to constuct a GPR2.Path_Name file.
   --  If the member does not exist Default is returned.
   --  Exception is raised if the member is not a string.

   function Get_Optional_File_Path
      (Obj : JSON_Value; Key : String) return GPR2.Path_Name.Object;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string that can be used to constuct a GPR2.Path_Name file.
   --  If the member does not exist GPR2.Path_Name.Undefined is returned.
   --  Exception is raised if the member is not a string.

   function Get_Dir_Path
      (Obj     : JSON_Value;
       Key     : String;
       Default : GPR2.Path_Name.Object) return GPR2.Path_Name.Object;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string that can be used to constuct a GPR2.Path_Name directory.
   --  If the member does not exist Default is returned.
   --  Exception is raised if the member is not a string.

   function Get_Optional_Dir_Path
      (Obj : JSON_Value; Key : String) return GPR2.Path_Name.Object;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a string that can be used to constuct a GPR2.Path_Name directory.
   --  If the member does not exist GPR2.Path_Name.Undefined is returned.
   --  Exception is raised if the member is not a string.

   function Get_Context
      (Obj : JSON_Value; Key : String) return GPR2.Context.Object;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a JSON object for which all members values are strings. The returned
   --  object is a GPR2.Context.Object.
   --  If the member does not exist or its value does not comply with the
   --  expected structure an exception is raised.

   function Get_Project_Tree
      (Obj : JSON_Value; Key : String) return Project_Tree_Access;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a JSON string representing a project tree id. The returned object is an
   --  access to a GPR2.Project.Tree.Object.
   --  If the member does not exist or its value does not comply with the
   --  expected structure an exception is raised.

   function Get_Project_View
     (Obj : JSON_Value; Key : String) return Project_View_Access;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a JSON string representing a project view id. The returned object is an
   --  access to a GPR2.Project.Tree.Object.
   --  If the member does not exist or its value does not comply with the
   --  expected structure an exception is raised.

   function Get_Optional_Project_View
     (Obj : JSON_Value; Key : String) return Project_View_Access;
   --  Returns member Key of JSON object Obj, assuming that member's value is
   --  a JSON string representing a project view id. If no value is provide
   --  null is returned

   function Get_Status (Obj : JSON_Value) return C_Status;
   --  Returns the "status" member of an answer JSON object.

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
   --  Return the Level_Output encoded in JSON.

   function Get_Name_Set
     (Obj : JSON_Value; Key : String) return GPR2.Containers.Name_Set;
   --  Return the strings defined in Key string array

   function Get_Name_Value_Map
     (Obj : JSON_Value; Key : String) return GPR2.Containers.Name_Value_Map;
   --  Return the name value map defined in Key dictionnary

   function Get_Project_Configuration
     (Obj : JSON_Value; Key : String := "configuration_id")
      return GPR2.Project.Configuration.Object;
   --  Return configuration defined in Key member

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
   --  Sets the status member of an answer JSON object Obj.

   procedure Set_Status
      (Obj   : JSON_Value;
       Value : C_Status;
       E     : Ada.Exceptions.Exception_Occurrence);
   --  Sets the status, error_msg and error_name of an answer JSON object Obj.

   procedure Set_Project_Tree
      (Obj   : JSON_Value;
       Key   : String;
       Value : Project_Tree_Access);
   --  Sets member Key of Obj to the project tree id of Value.

   procedure Set_Project_View
      (Obj  : JSON_Value;
       Key  : String;
       View : in out Project_View_Access);
   --  Sets member Key of Obj to the project view id of Value.
   --  If View is undefined, it is deallocated.

   procedure Set_String
      (Obj   : JSON_Value;
       Key   : String;
       Value : String);
   --  Sets member Key of Obj to the string Value.

   procedure Set_Project_Attribute
      (Obj   : JSON_Value;
       Key   : String;
       Value : GPR2.Project.Attribute.Object);
   --  Sets member Key of Obj to the GPR2 attribute. The attribute is
   --  serialized as string for single project values or to a list of
   --  string.

   procedure Add_Message
     (Obj     : JSON_Value;
      Message : GPR2.Message.Object);
   --  Add message to JSON array Obj

   procedure Set_Source_Reference
     (Obj  : JSON_Value;
      Key  : String;
      Sloc : GPR2.Source_Reference.Object);
   --  Sets source reference object's members in Obj

   procedure Set_Context
     (Obj     : JSON_Value;
      Key     : String;
      Context : GPR2.Context.Object);
   --  Set member Key of Obj to context Context. A context is serialized as
   --  a JSON object.

   procedure Set_Path
      (Obj  : JSON_Value;
       Key  : String;
       Path : GPR2.Path_Name.Object);
   --  Set member Key of Obj to Path. In case Path is not defined then set the
   --  value to null.

   procedure Set_Optional_Name
      (Obj  : JSON_Value;
       Key  : String;
       Name : GPR2.Optional_Name_Type);
   --  Set member Key of Obj to Name. In case Name = No_Name then set the
   --  value to null.

   procedure Set_Name
      (Obj  : JSON_Value;
       Key  : String;
       Name : GPR2.Name_Type);
   --  Set member Key of Obj to Name.

   procedure Set_Path_Name_Set_Object
     (Obj : JSON_Value;
      Key : String;
      Set : GPR2.Path_Name.Set.Object);
   --  Set member Key of Obj with path name set full names.

   procedure Set_Project_Views
     (Obj   : JSON_Value;
      Key   : String;
      Views : GPR2.Project.View.Set.Object);
   --  Set member Key of Obj to view_ids array.

   procedure Set_Attributes
     (Obj        : JSON_Value;
      Key        : String;
      Attributes : GPR2.Project.Attribute.Set.Object);
   --  Set member Key of Obj with attributes in Set.

   procedure Set_Types
     (Obj   : JSON_Value;
      Key   : String;
      Types : GPR2.Project.Typ.Set.Object);
   --  Set member Key of Obj with types in Set.

   procedure Set_Variables
     (Obj       : JSON_Value;
      Key       : String;
      Variables : GPR2.Project.Variable.Set.Object);
   --  Set member Key of Obj with variables in set Variables
   --  The member value is Dict[str, Variable] (see GPR2.C for Variable format)

   procedure Set_Sources
      (Obj : JSON_Value;
       Key : String;
       Sources : GPR2.Project.Source.Set.Object);
   --  Sets member Key of Obj with sources in set Sources
   --  The member value is List[Source] (see GPR2.C for Source format)

   procedure Set_Boolean
      (Obj  : JSON_Value;
       Key  : String;
       Bool : Boolean);
   --  Sets member Key of Obj with boolean Bool

   procedure Set_Null
      (Obj : JSON_Value;
       Key : String);
   --  Sets member Key of Obj to null
end GPR2.C.JSON;
