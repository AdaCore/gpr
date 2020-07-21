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
with Interfaces.C.Strings;
with GPR2.Project.Registry.Attribute;
with System.Storage_Elements;

package body GPR2.C.JSON is

   use type GPR2.Project.Registry.Attribute.Value_Kind;

   function Convert is new Ada.Unchecked_Conversion
      (Interfaces.C.Strings.chars_ptr, C_Answer);

   procedure Set_Address
      (Obj : JSON_Value; Key : String; Addr : System.Address);

   function Get_Address
      (Obj : JSON_Value; Key : String) return System.Address;

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
      if GNATCOLL.JSON.Has_Field (Val => Obj, Field => Key) then
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
         null;
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
      if GNATCOLL.JSON.Has_Field (Val => Obj, Field => Key) then
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
      if GNATCOLL.JSON.Has_Field (Val => Obj, Field => Key) then
         return GPR2.Path_Name.Create_File
            (Name => GPR2.Name_Type (Get_String (Obj, Key)));
      else
         return Default;
      end if;
   end Get_File_Path;

   ----------------------
   -- Get_Level_Format --
   ----------------------

   function Get_Level_Format
     (Obj : JSON_Value; Key : String; Default : GPR2.Message.Level_Format)
      return GPR2.Message.Level_Format
   is
   begin
      if not GNATCOLL.JSON.Has_Field (Val => Obj, Field => Key) then
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
      if GNATCOLL.JSON.Has_Field (Val => Obj, Field => Key) then
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
      use System.Storage_Elements;
      Addr_Int : constant Integer_Address := To_Integer (Addr);

      --  When compiling with -fstack-usage, the following call is marked as
      --  "dynamic". Nevertheless we know that max integer for a given address
      --  is in fact bounded thus we can ignore the issue.
      Addr_Img : constant String := Addr_Int'Img;
   begin
      GNATCOLL.JSON.Set_Field
         (Obj, Key,
          Addr_Img (Addr_Img'First + 1 .. Addr_Img'Last));
   end Set_Address;

   -----------------
   -- Set_Message --
   -----------------

   procedure Set_Message
     (Obj            : JSON_Value;
      Message        : GPR2.Message.Object;
      Full_Path_Name : Boolean;
      Levels         : GPR2.Message.Level_Output)
   is
   begin
      Set_String (Obj, "level", Message.Level'Img);
      Set_String (Obj, "message", Message.Message);
      Set_String (Obj, "formatted_message",
                  GPR2.Message.Format (Self           => Message,
                                       Full_Path_Name => Full_Path_Name,
                                       Levels         => Levels));

      Set_Source_Reference (Obj, Message.Sloc);
   end Set_Message;

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
      (Obj   : JSON_Value;
       Key   : String;
       Value : Project_View_Access)
   is
   begin
      Set_Address (Obj, Key, Value.all'Address);
   end Set_Project_View;

   --------------------------
   -- Set_Source_Reference --
   --------------------------

   procedure Set_Source_Reference
     (Obj              : JSON_Value;
      Source_Reference : GPR2.Source_Reference.Object) is
   begin
      Set_String (Obj, "filename", Source_Reference.Filename);
      if Source_Reference.Has_Source_Reference then
         GNATCOLL.JSON.Set_Field (Obj, "line", Source_Reference.Line);
         GNATCOLL.JSON.Set_Field (Obj, "column", Source_Reference.Column);
      end if;
   end Set_Source_Reference;

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

end GPR2.C.JSON;
