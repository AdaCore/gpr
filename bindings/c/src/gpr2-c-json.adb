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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Variable;

with System.Storage_Elements;

package body GPR2.C.JSON is

   use type GPR2.Project.Registry.Attribute.Value_Kind;
   use type GNATCOLL.JSON.JSON_Value;

   function Convert is new Ada.Unchecked_Conversion
      (Interfaces.C.Strings.chars_ptr, C_Answer);

   function Get_Id (Addr : System.Address) return String;

   procedure Set_Address
      (Obj : JSON_Value; Key : String; Addr : System.Address);

   function Get_Address
      (Obj : JSON_Value; Key : String) return System.Address;

   function Has_Non_Null_Field
     (Obj : JSON_Value; Key : String) return Boolean is
     (GNATCOLL.JSON.Has_Field (Val => Obj, Field => Key) and then
      GNATCOLL.JSON.Get (Val => Obj, Field => Key) /= GNATCOLL.JSON.JSON_Null);
   --  Return True if Obj contains a non null field named Key

   procedure Set_Source_Value_List
     (Obj    : JSON_Value;
      Key    : String;
      Values : GPR2.Containers.Source_Value_List);
   -----------------
   -- Add_Message --
   -----------------

   procedure Add_Message
     (Obj            : JSON_Value;
      Message        : GPR2.Message.Object)
   is
      JSON_Message : constant GNATCOLL.JSON.JSON_Value :=
         GNATCOLL.JSON.Create_Object;
   begin
      Set_String (JSON_Message, "level", Message.Level'Img);
      Set_String (JSON_Message, "message", Message.Message);
      Set_Source_Reference (JSON_Message, "sloc", Message.Sloc);
      Obj.Append (JSON_Message);
   end Add_Message;

   ------------
   -- Decode --
   ------------

   function Decode (Request : C_Request) return JSON_Value
   is
   begin
      return GNATCOLL.JSON.Read (Value (Request));
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode (Answer : JSON.JSON_Value) return C_Answer
   is
      use Interfaces.C.Strings;
      Result : constant chars_ptr :=
         New_String (GNATCOLL.JSON.Write (Answer));
   begin
      return Convert (Result);
   end Encode;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
      (Obj : JSON_Value; Key : String) return System.Address
   is
      use System.Storage_Elements;
      Str : constant String := GNATCOLL.JSON.Get (Obj, Key);
      Addr_Int : constant Integer_Address := Integer_Address'Value (Str);
   begin
      return To_Address (Addr_Int);
   end Get_Address;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
      (Obj     : JSON_Value;
       Key     : String;
       Default : Boolean) return Boolean
   is
   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         return GNATCOLL.JSON.Get (Val => Obj, Field => Key);
      else
         return Default;
      end if;
   end Get_Boolean;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
      (Obj : JSON_Value; Key : String) return GPR2.Context.Object
   is
      Result : GPR2.Context.Object;

      procedure CB
         (Name  : GNATCOLL.JSON.UTF8_String;
          Value : GNATCOLL.JSON.JSON_Value);

      --------
      -- CB --
      --------

      procedure CB
         (Name : GNATCOLL.JSON.UTF8_String;
          Value : GNATCOLL.JSON.JSON_Value)
      is
      begin
         Result.Insert (Optional_Name_Type (Name),
                        GNATCOLL.JSON.Get (Value));
      end CB;

   begin
      GNATCOLL.JSON.Map_JSON_Object (Val => GNATCOLL.JSON.Get (Obj, Key),
                                     CB  => CB'Unrestricted_Access);
      return Result;
   end Get_Context;

   ------------------
   -- Get_Dir_Path --
   ------------------

   function Get_Dir_Path
      (Obj     : JSON_Value;
       Key     : String;
       Default : GPR2.Path_Name.Object) return GPR2.Path_Name.Object
   is
   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         return GPR2.Path_Name.Create_Directory
            (Name => GPR2.Name_Type (Get_String (Obj, Key)));
      else
         return Default;
      end if;
   end Get_Dir_Path;

   -------------------
   -- Get_File_Path --
   -------------------

   function Get_File_Path
      (Obj : JSON_Value; Key : String) return GPR2.Path_Name.Object
   is
   begin
      return GPR2.Path_Name.Create_File
         (Name => GPR2.Name_Type (Get_String (Obj, Key)));
   end Get_File_Path;

   function Get_File_Path
      (Obj     : JSON_Value;
       Key     : String;
       Default : GPR2.Path_Name.Object) return GPR2.Path_Name.Object
   is
   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         return GPR2.Path_Name.Create_File
            (Name => GPR2.Name_Type (Get_String (Obj, Key)));
      else
         return Default;
      end if;
   end Get_File_Path;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Addr : System.Address) return String
   is
      use System.Storage_Elements;
      Addr_Int : constant Integer_Address := To_Integer (Addr);

      --  When compiling with -fstack-usage, the following call is marked as
      --  "dynamic". Nevertheless we know that max integer for a given address
      --  is in fact bounded thus we can ignore the issue.
      Addr_Img : constant String := Addr_Int'Img;
   begin
      return Addr_Img (Addr_Img'First + 1 .. Addr_Img'Last);
   end Get_Id;

   ----------------------
   -- Get_Level_Format --
   ----------------------

   function Get_Level_Format
     (Obj : JSON_Value; Key : String; Default : GPR2.Message.Level_Format)
      return GPR2.Message.Level_Format
   is
   begin
      if not Has_Non_Null_Field (Obj => Obj, Key => Key) then
         return Default;
      else
         declare
            Value : constant String := Get_String (Obj, Key);
         begin
            if Value = "short" then
               return GPR2.Message.Short;
            elsif Value = "none" then
                  return GPR2.Message.None;
            else
               return GPR2.Message.Long;
            end if;
         end;
      end if;
   end Get_Level_Format;

   ----------------------
   -- Get_Level_Output --
   ----------------------

   function Get_Level_Output
     (Obj : JSON_Value; Default : GPR2.Message.Level_Output)
      return GPR2.Message.Level_Output
   is
      Level_Output : GPR2.Message.Level_Output;
   begin
      Level_Output (GPR2.Message.Information) :=
        Get_Level_Format (Obj, "information_level_output",
                          Default (GPR2.Message.Information));
      Level_Output (GPR2.Message.Warning) :=
        Get_Level_Format (Obj, "warning_level_output",
                          Default (GPR2.Message.Warning));
      Level_Output (GPR2.Message.Error) :=
        Get_Level_Format (Obj, "error_level_output",
                          Default (GPR2.Message.Error));
      return Level_Output;
   end Get_Level_Output;

   ------------------
   -- Get_Name_Set --
   ------------------

   function Get_Name_Set
     (Obj : JSON_Value; Key : String) return GPR2.Containers.Name_Set
   is
      Result : GPR2.Containers.Name_Set;
   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         for Value of JSON_Array'
           (GNATCOLL.JSON.Get (Val => Obj, Field => Key)) loop
            declare
               Element : constant String := Value.Get;
            begin
               Result.Insert (Optional_Name_Type (Element));
            end;
         end loop;
      end if;
      return Result;
   end Get_Name_Set;

   ------------------------
   -- Get_Name_Value_Map --
   ------------------------

   function Get_Name_Value_Map
     (Obj : JSON_Value; Key : String) return GPR2.Containers.Name_Value_Map
   is
      Result : GPR2.Containers.Name_Value_Map;

      procedure CB
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value);

      --------
      -- CB --
      --------

      procedure CB
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value)
      is
      begin
         Result.Insert (Optional_Name_Type (Name), Value.Get);
      end CB;

   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         GNATCOLL.JSON.Map_JSON_Object (Val => GNATCOLL.JSON.Get (Obj, Key),
                                        CB  => CB'Unrestricted_Access);
      end if;
      return Result;
   end Get_Name_Value_Map;

   ----------------------------
   -- Get_Optional_Dir_Path --
   ----------------------------

   function Get_Optional_Dir_Path
      (Obj : JSON_Value; Key : String) return GPR2.Path_Name.Object
   is
   begin
      return Get_Dir_Path (Obj, Key, GPR2.Path_Name.Undefined);
   end Get_Optional_Dir_Path;

   ----------------------------
   -- Get_Optional_File_Path --
   ----------------------------

   function Get_Optional_File_Path
      (Obj : JSON_Value; Key : String) return GPR2.Path_Name.Object
   is
   begin
      return Get_File_Path (Obj, Key, GPR2.Path_Name.Undefined);
   end Get_Optional_File_Path;

   -----------------------
   -- Get_Optional_Name --
   -----------------------

   function Get_Optional_Name
      (Obj : JSON_Value; Key : String) return GPR2.Optional_Name_Type
   is
   begin
      return Optional_Name_Type
         (Get_String (Obj, Key, String (GPR2.No_Name)));
   end Get_Optional_Name;

   -------------------------------
   -- Get_Optional_Project_View --
   -------------------------------

   function Get_Optional_Project_View
     (Obj : JSON_Value; Key : String) return Project_View_Access
   is
   begin
      if Has_Non_Null_Field (Obj, Key) then
         return Get_Project_View (Obj, Key);
      else
         return null;
      end if;
   end Get_Optional_Project_View;

   -------------------------------
   -- Get_Project_Configuration --
   -------------------------------

   function Get_Project_Configuration
     (Obj : JSON_Value; Key : String := "configuration_id")
      return GPR2.Project.Configuration.Object
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Project_Configuration_Access);
   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         return Convert (Get_Address (Obj, Key)).all;
      else
         return GPR2.Project.Configuration.Undefined;
      end if;
   end Get_Project_Configuration;

   ----------------------
   -- Get_Project_Tree --
   ----------------------

   function Get_Project_Tree
      (Obj : JSON_Value; Key : String) return Project_Tree_Access
   is
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Project_Tree_Access);
   begin
      return Convert (Get_Address (Obj, Key));
   end Get_Project_Tree;

   ----------------------
   -- Get_Project_View --
   ----------------------

   function Get_Project_View
      (Obj : JSON_Value; Key : String) return Project_View_Access
   is
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Project_View_Access);
   begin
      return Convert (Get_Address (Obj, Key));
   end Get_Project_View;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Obj : JSON_Value) return JSON_Value
   is
   begin
      return GNATCOLL.JSON.Get (Obj, "result");
   end Get_Result;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Obj : JSON_Value) return C_Status
   is
      I : constant Integer := GNATCOLL.JSON.Get (Obj, "status");
   begin
      return C_Status (I);
   end Get_Status;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
      (Obj : JSON_Value; Key : String) return String
   is
   begin
      return GNATCOLL.JSON.Get (Val => Obj, Field => Key);
   exception
      when Constraint_Error =>
         raise GPR2_C_Exception
            with "missing string parameter: '" & Key & "'";
   end Get_String;

   function Get_String
      (Obj     : JSON_Value;
       Key     : String;
       Default : String)
      return String
   is
   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         return GNATCOLL.JSON.Get (Val => Obj, Field => Key);
      else
         return Default;
      end if;
   end Get_String;

   -----------------------
   -- Initialize_Answer --
   -----------------------

   function Initialize_Answer return JSON_Value
   is
      Answer : JSON_Value;
   begin
      Answer := GNATCOLL.JSON.Create_Object;
      GNATCOLL.JSON.Set_Field (Answer, "result", GNATCOLL.JSON.Create_Object);
      Set_Status (Answer, 0);
      return Answer;
   end Initialize_Answer;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
      (Obj  : JSON_Value;
       Key  : String;
       Addr : System.Address)
   is
   begin
      GNATCOLL.JSON.Set_Field (Obj, Key, Get_Id (Addr));
   end Set_Address;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Obj        : JSON_Value;
      Key        : String;
      Attributes : GPR2.Project.Attribute.Set.Object)
   is
      Attributes_Array : GNATCOLL.JSON.JSON_Array;
   begin
      for Attribute of Attributes loop
         declare
            Content : constant GNATCOLL.JSON.JSON_Value :=
                        GNATCOLL.JSON.Create_Object;
         begin
            GNATCOLL.JSON.Set_Field (Content, "name",
                                     String (Attribute.Name.Text));
            if Attribute.Has_Index then
               GNATCOLL.JSON.Set_Field (Content, "index",
                                        String (Attribute.Index.Text));
               if Attribute.Index.Has_At_Num then
                  GNATCOLL.JSON.Set_Field (Content, "at",
                                           Attribute.Index.At_Num);
               else
                  GNATCOLL.JSON.Set_Field (Content, "at",
                                           GNATCOLL.JSON.JSON_Null);
               end if;
            else
               GNATCOLL.JSON.Set_Field (Content, "index",
                                        GNATCOLL.JSON.JSON_Null);
               GNATCOLL.JSON.Set_Field (Content, "at",
                                        GNATCOLL.JSON.JSON_Null);
            end if;
            if Attribute.Kind = GPR2.Project.Registry.Attribute.Single then
               GNATCOLL.JSON.Set_Field (Content, "value",
                                        Attribute.Value.Text);
            else
               Set_Source_Value_List (Content, "value", Attribute.Values);
            end if;
            GNATCOLL.JSON.Append (Attributes_Array, Content);
         end;
      end loop;
      GNATCOLL.JSON.Set_Field (Obj, Key, Attributes_Array);
   end Set_Attributes;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Obj     : JSON_Value;
      Key     : String;
      Context : GPR2.Context.Object)
   is
      JSON_Context : constant JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      for C in Context.Iterate loop
         GNATCOLL.JSON.Set_Field
            (JSON_Context,
             String (GPR2.Context.Key_Value.Key (C)),
             GPR2.Context.Key_Value.Element (C));
      end loop;

      GNATCOLL.JSON.Set_Field (Obj, Key, JSON_Context);
   end Set_Context;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Obj  : JSON_Value;
       Key  : String;
       Name : GPR2.Name_Type)
   is
   begin
      Set_String (Obj, Key, String (Name));
   end Set_Name;

   -----------------------
   -- Set_Optional_Name --
   -----------------------

   procedure Set_Optional_Name
      (Obj  : JSON_Value;
       Key  : String;
       Name : GPR2.Optional_Name_Type)
   is
   begin
      if Name = GPR2.No_Name then
         GNATCOLL.JSON.Set_Field (Obj, Key, GNATCOLL.JSON.JSON_Null);
      else
         Set_String (Obj, Key, String (Name));
      end if;
   end Set_Optional_Name;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
      (Obj  : JSON_Value;
       Key  : String;
       Path : GPR2.Path_Name.Object)
   is
   begin
      if Path.Is_Defined then
         Set_String (Obj, Key, Path.Value);
      else
         GNATCOLL.JSON.Set_Field (Obj, Key, GNATCOLL.JSON.JSON_Null);
      end if;
   end Set_Path;

   ------------------------------
   -- Set_Path_Name_Set_Object --
   ------------------------------

   procedure Set_Path_Name_Set_Object
     (Obj : JSON_Value;
      Key : String;
      Set : GPR2.Path_Name.Set.Object)
   is
      Elements : JSON_Array;
   begin
      for Path of Set loop
         GNATCOLL.JSON.Append
           (Elements, GNATCOLL.JSON.Create (String (Path.Value)));
      end loop;
      GNATCOLL.JSON.Set_Field (Obj, Key, GNATCOLL.JSON.Create (Elements));
   end Set_Path_Name_Set_Object;

   ---------------------------
   -- Set_Project_Attribute --
   ---------------------------

   procedure Set_Project_Attribute
      (Obj   : JSON_Value;
       Key   : String;
       Value : GPR2.Project.Attribute.Object)
   is
   begin
      if Value.Kind = GPR2.Project.Registry.Attribute.Single then
         Set_String (Obj, Key, Value.Value.Text);
      else
         declare
            Value_Array : GNATCOLL.JSON.JSON_Array;
         begin
            for Index in 1 .. Value.Count_Values loop
               declare
                  JSON_Str : constant JSON_Value :=
                     GNATCOLL.JSON.Create
                        (Value.Values.Element (Integer (Index)).Text);
               begin
                  GNATCOLL.JSON.Append (Value_Array, JSON_Str);
               end;
            end loop;
            GNATCOLL.JSON.Set_Field
               (Obj, Key, GNATCOLL.JSON.Create (Value_Array));
         end;

      end if;

   end Set_Project_Attribute;

   ----------------------
   -- Set_Project_Tree --
   ----------------------

   procedure Set_Project_Tree
      (Obj   : JSON_Value;
       Key   : String;
       Value : Project_Tree_Access)
   is
   begin
      Set_Address (Obj, Key, Value.all'Address);
   end Set_Project_Tree;

   ----------------------
   -- Set_Project_View --
   ----------------------

   procedure Set_Project_View
      (Obj  : JSON_Value;
       Key  : String;
       View : in out Project_View_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (GPR2.Project.View.Object, Project_View_Access);

   begin
      if View.Is_Defined then
         Set_Address (Obj, Key, View.all'Address);
      else
         Free (View);
         GNATCOLL.JSON.Set_Field (Obj, Key, GNATCOLL.JSON.JSON_Null);
      end if;
   end Set_Project_View;

   -----------------------
   -- Set_Project_Views --
   -----------------------

   procedure Set_Project_Views
     (Obj   : JSON_Value;
      Key   : String;
      Views : GPR2.Project.View.Set.Object) is
      View_Ids : GNATCOLL.JSON.JSON_Array;
   begin
      for View of Views loop
         if View.Is_Defined then
            declare
               View_Access : constant Project_View_Access :=
                               new GPR2.Project.View.Object;
            begin
               View_Access.all := View;
               GNATCOLL.JSON.Append
                 (View_Ids, GNATCOLL.JSON.Create
                    (Get_Id (View_Access.all'Address)));
            end;
         end if;
      end loop;
      GNATCOLL.JSON.Set_Field (Obj, Key, GNATCOLL.JSON.Create (View_Ids));
   end Set_Project_Views;

   --------------------------
   -- Set_Source_Reference --
   --------------------------

   procedure Set_Source_Reference
     (Obj  : JSON_Value;
      Key  : String;
      Sloc : GPR2.Source_Reference.Object)
   is
      JSON_Sloc : constant GNATCOLL.JSON.JSON_Value :=
         GNATCOLL.JSON.Create_Object;
   begin
      Set_String (JSON_Sloc, "filename", Sloc.Filename);
      if Sloc.Has_Source_Reference then
         GNATCOLL.JSON.Set_Field (JSON_Sloc, "line", Sloc.Line);
         GNATCOLL.JSON.Set_Field (JSON_Sloc, "column", Sloc.Column);
      else
         GNATCOLL.JSON.Set_Field (JSON_Sloc, "line", GNATCOLL.JSON.JSON_Null);
         GNATCOLL.JSON.Set_Field
            (JSON_Sloc, "column", GNATCOLL.JSON.JSON_Null);
      end if;
      GNATCOLL.JSON.Set_Field (Obj, Key, JSON_Sloc);
   end Set_Source_Reference;

   ---------------------------
   -- Set_Source_Value_List --
   ---------------------------

   procedure Set_Source_Value_List
     (Obj : JSON_Value;
      Key : String;
      Values : GPR2.Containers.Source_Value_List) is
      Values_Array : JSON_Array;
   begin
      for Value of Values loop
         GNATCOLL.JSON.Append
           (Values_Array, GNATCOLL.JSON.Create (String (Value.Text)));
      end loop;
      GNATCOLL.JSON.Set_Field (Obj, Key, Values_Array);
   end Set_Source_Value_List;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
      (Obj   : JSON_Value;
       Value : C_Status)
   is
   begin
      GNATCOLL.JSON.Set_Field (Obj, "status", Integer (Value));
      Set_String (Obj, "error_msg", "");
      Set_String (Obj, "error_name", "");
   end Set_Status;

   procedure Set_Status
      (Obj   : JSON_Value;
       Value : C_Status;
       E     : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      GNATCOLL.JSON.Set_Field (Obj, "status", Integer (Value));
      Set_String (Obj, "error_msg", Ada.Exceptions.Exception_Message (E));
      Set_String (Obj, "error_name", Ada.Exceptions.Exception_Name (E));
   end Set_Status;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
      (Obj   : JSON_Value;
       Key   : String;
       Value : String)
   is
   begin
      GNATCOLL.JSON.Set_Field (Obj, Key, Value);
   end Set_String;

   ---------------
   -- Set_Types --
   ---------------

   procedure Set_Types
     (Obj   : JSON_Value;
      Key   : String;
      Types : GPR2.Project.Typ.Set.Object)
   is
      Types_Array : GNATCOLL.JSON.JSON_Array;
   begin
      for Typ of Types loop
         declare
            Content : constant GNATCOLL.JSON.JSON_Value :=
                        GNATCOLL.JSON.Create_Object;
         begin
            GNATCOLL.JSON.Set_Field (Content, "name",
                                     String (Typ.Name.Text));
            if Typ.Kind = GPR2.Project.Registry.Attribute.Single then
               GNATCOLL.JSON.Set_Field (Content, "value", Typ.Value.Text);
            else
               Set_Source_Value_List (Content, "value", Typ.Values);
            end if;
         end;
      end loop;
      GNATCOLL.JSON.Set_Field (Obj, Key, Types_Array);
   end Set_Types;

   -------------------
   -- Set_Variables --
   -------------------

   procedure Set_Variables
     (Obj       : JSON_Value;
      Key       : String;
      Variables : GPR2.Project.Variable.Set.Object)
   is
      Variables_Array : GNATCOLL.JSON.JSON_Array;
   begin
      for Variable of Variables loop
         declare
            Content : constant GNATCOLL.JSON.JSON_Value :=
                        GNATCOLL.JSON.Create_Object;
         begin
            GNATCOLL.JSON.Set_Field (Content, "name",
                                     String (Variable.Name.Text));
            if Variable.Has_Type then
               GNATCOLL.JSON.Set_Field (Content, "type",
                                        String (Variable.Typ.Name.Text));
            else
               GNATCOLL.JSON.Set_Field (Content, "type",
                                        GNATCOLL.JSON.JSON_Null);
            end if;
            if Variable.Kind = GPR2.Project.Registry.Attribute.Single then
               GNATCOLL.JSON.Set_Field (Content, "value", Variable.Value.Text);
            else
               Set_Source_Value_List (Content, "value", Variable.Values);
            end if;
         end;
      end loop;
      GNATCOLL.JSON.Set_Field (Obj, Key, Variables_Array);
   end Set_Variables;

end GPR2.C.JSON;
