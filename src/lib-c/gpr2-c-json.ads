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
with GPR2.Context;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Source_Reference;

package GPR2.C.JSON is

   subtype JSON_Value is GNATCOLL.JSON.JSON_Value;
   subtype JSON_Array is GNATCOLL.JSON.JSON_Array;

   function Decode (Request : C_Request) return JSON_Value;
   --  Decodes a C_Request (char * in C) into a JSON_Value

   function Encode (Answer : JSON_Value) return C_Answer;
   --  Encodes a JSON_Value into a C_Answer (char * in C)

   -----------------------
   -- Decoding requests --
   -----------------------

   type Project_Tree_Access is access all GPR2.Project.Tree.Object;
   pragma No_Strict_Aliasing (Project_Tree_Access);

   type Project_View_Access is access all GPR2.Project.View.Object;
   pragma No_Strict_Aliasing (Project_View_Access);

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
      (Obj   : JSON_Value;
       Key   : String;
       Value : Project_View_Access);
   --  Sets member Key of Obj to the project view id of Value.

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

   procedure Set_Message
     (Obj            : JSON_Value;
      Message        : GPR2.Message.Object;
      Full_Path_Name : Boolean;
      Levels         : GPR2.Message.Level_Output);
   --  Sets message object's members in Obj

   procedure Set_Source_Reference
     (Obj              : JSON_Value;
      Source_Reference : GPR2.Source_Reference.Object);
   --  Sets source reference object's members in Obj

   procedure Set_Context
     (Obj     : JSON_Value;
      Key     : String;
      Context : GPR2.Context.Object);
   --  Set member Key of Obj to context Context. A context is serialized as
   --  a JSON object.

end GPR2.C.JSON;
