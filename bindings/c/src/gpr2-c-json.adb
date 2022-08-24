--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with GPR2.View_Ids;
with GPR2.Context;
with System.Storage_Elements;

package body GPR2.C.JSON is

   use type GNATCOLL.JSON.JSON_Value;
   use type GNATCOLL.JSON.JSON_Value_Type;

   function Convert is new Ada.Unchecked_Conversion
     (Interfaces.C.Strings.chars_ptr, C_Answer);

   function Get_Address
     (Obj : JSON_Value; Key : String) return System.Address;

   function Has_Non_Null_Field
     (Obj : JSON_Value; Key : String) return Boolean
   is
     (GNATCOLL.JSON.Has_Field (Val => Obj, Field => Key) and then
      GNATCOLL.JSON.Get (Val => Obj, Field => Key) /= GNATCOLL.JSON.JSON_Null);
   --  Return True if Obj contains a non null field named Key

   ----------
   -- Bind --
   ----------

   function Bind
     (Request : C_Request;
      Answer  : out C_Answer;
      Handler : Bind_Handler) return C_Status
   is
      Answer_Obj  : constant JSON_Value := Initialize_Answer;
      Request_Obj : JSON.JSON_Value;
      Result_Obj  : JSON_Value;
      Can_Decode  : Boolean := True;

   begin
      Result_Obj := Get_Result (Answer_Obj);

      --  Until this stage an error can only occur if there is a lack of
      --  memory in which case nothing can really be done.

      begin
         Request_Obj := Decode (Request);
      exception
         when E : others =>
            --  Error detected during parsing of the request JSON
            Set_Status (Answer_Obj, Invalid_Request, E);
            Can_Decode := False;
      end;

      if Can_Decode then
         begin
            Handler (Request => Request_Obj, Result  => Result_Obj);
         exception
            when E : others =>
               Set_Status (Answer_Obj, Call_Error, E);
         end;
      end if;

      --  Unless there is a bug in the GNATCOLL.JSON library, all relevant
      --  errors have been caught. No exception is expected from here.

      Answer := Encode (Answer_Obj);

      return Get_Status (Answer_Obj);
   end Bind;

   ------------
   -- Decode --
   ------------

   function Decode (Request : C_Request) return JSON_Value is
   begin
      return GNATCOLL.JSON.Read (Value (Request));
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode (Answer : JSON.JSON_Value) return C_Answer is
      use Interfaces.C.Strings;
      Result : constant chars_ptr :=
                 New_String (GNATCOLL.JSON.Write (Answer));
   begin
      return Convert (Result);
   end Encode;

   ---------
   -- Get --
   ---------

   function Get (Obj : JSON_Value; Key : String) return JSON_Value is
   begin
      return GNATCOLL.JSON.Get (Obj, Key);
   end Get;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address
     (Obj : JSON_Value; Key : String) return System.Address
   is
      use System.Storage_Elements;
      Str      : constant String := GNATCOLL.JSON.Get (Obj, Key);
      Addr_Int : constant Integer_Address := Integer_Address'Value (Str);
   begin
      return To_Address (Addr_Int);
   end Get_Address;

   ----------------------
   -- Get_GPR_Tree --
   ----------------------

   function Get_GPR_Tree
     (Obj : JSON_Value; Key : String) return GPR_Tree_Access
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, GPR_Tree_Access);
   begin
      return Convert (Get_Address (Obj, Key));
   end Get_GPR_Tree;

   ----------------------
   -- Get_Level_Format --
   ----------------------

   function Get_Level_Format
     (Obj     : JSON_Value;
      Key     : String;
      Default : GPR2.Message.Level_Format)
      return GPR2.Message.Level_Format is
   begin
      if not Has_Non_Null_Field (Obj => Obj, Key => Key) then
         return Default;

      else
         declare
            Value : constant String := To_String (Get (Obj, Key));
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
     (Obj     : JSON_Value;
      Default : GPR2.Message.Level_Output)
      return GPR2.Message.Level_Output
   is
      Level_Output : GPR2.Message.Level_Output;
   begin
      Level_Output (GPR2.Message.Information) :=
        Get_Level_Format
          (Obj, "information_level_output",
           Default (GPR2.Message.Information));

      Level_Output (GPR2.Message.Warning) :=
        Get_Level_Format
          (Obj, "warning_level_output", Default (GPR2.Message.Warning));

      Level_Output (GPR2.Message.Error) :=
        Get_Level_Format
          (Obj, "error_level_output", Default (GPR2.Message.Error));
      return Level_Output;
   end Get_Level_Output;

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
         Value : GNATCOLL.JSON.JSON_Value) is
      begin
         Result.Insert (Optional_Name_Type (Name), Value.Get);
      end CB;

   begin
      if Has_Non_Null_Field (Obj => Obj, Key => Key) then
         GNATCOLL.JSON.Map_JSON_Object
           (Val => GNATCOLL.JSON.Get (Obj, Key),
            CB  => CB'Unrestricted_Access);
      end if;

      return Result;
   end Get_Name_Value_Map;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Obj : JSON_Value) return JSON_Value is
   begin
      return GNATCOLL.JSON.Get (Obj, "result");
   end Get_Result;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Obj : JSON_Value) return C_Status is
      I : constant Integer := GNATCOLL.JSON.Get (Obj, "status");
   begin
      return C_Status (I);
   end Get_Status;

   -----------------------
   -- Initialize_Answer --
   -----------------------

   function Initialize_Answer return JSON_Value is
      Answer : constant JSON_Value := GNATCOLL.JSON.Create_Object;
   begin
      GNATCOLL.JSON.Set_Field (Answer, "result", GNATCOLL.JSON.Create_Object);
      Set_Status (Answer, 0);

      return Answer;
   end Initialize_Answer;

   ---------
   -- Set --
   ---------

   procedure Set (Obj : JSON_Value; Key : String; Value : JSON_Value) is
   begin
      GNATCOLL.JSON.Set_Field (Obj, Key, Value);
   end Set;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Obj   : JSON_Value;
      Value : C_Status) is
   begin
      GNATCOLL.JSON.Set_Field (Obj, "status", Integer (Value));
      GNATCOLL.JSON.Set_Field (Obj, "error_msg", "");
      GNATCOLL.JSON.Set_Field (Obj, "error_name", "");
   end Set_Status;

   procedure Set_Status
     (Obj   : JSON_Value;
      Value : C_Status;
      E     : Ada.Exceptions.Exception_Occurrence) is
   begin
      GNATCOLL.JSON.Set_Field (Obj, "status", Integer (Value));
      GNATCOLL.JSON.Set_Field
         (Obj, "error_msg", Ada.Exceptions.Exception_Message (E));
      GNATCOLL.JSON.Set_Field
         (Obj, "error_name", Ada.Exceptions.Exception_Name (E));
   end Set_Status;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Obj : JSON_Value; Default : Boolean) return Boolean is
   begin
      if GNATCOLL.JSON."="(Obj, GNATCOLL.JSON.JSON_Null) then
         return Default;
      else
         return GNATCOLL.JSON.Get (Val => Obj);
      end if;
   end To_Boolean;

   ----------------
   -- To_Context --
   ----------------

   function To_Context (Obj : JSON_Value) return GPR_Context
   is
      Result : GPR_Context;

      procedure CB
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value);

      --------
      -- CB --
      --------

      procedure CB
        (Name : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value) is
      begin
         Result.Insert
            (Optional_Name_Type (Name), Value_Type (To_String (Value)));
      end CB;

   begin
      if GNATCOLL.JSON.Kind (Obj) = GNATCOLL.JSON.JSON_Object_Type then
         GNATCOLL.JSON.Map_JSON_Object
           (Val => Obj,
            CB  => CB'Unrestricted_Access);
      elsif GNATCOLL.JSON.Kind (Obj) /= GNATCOLL.JSON.JSON_Null_Type then
         raise GPR2_C_Exception with "expected context json value";
      end if;

      return Result;
   end To_Context;

   -----------------
   -- To_Filename --
   -----------------

   function To_Filename
      (Obj : JSON_Value) return GPR2.Filename_Type
   is
   begin
      return GPR2.Filename_Type (To_String (Obj));
   end To_Filename;

   ---------------------
   -- To_GPR_View --
   ---------------------

   function To_GPR_View
      (Tree : GPR_Tree; Obj : JSON_Value) return GPR_View
   is
      Id : GPR2.View_Ids.View_Id;
   begin
      Id := GPR2.View_Ids.Import (Value_Type (To_Name (Obj)));
      return Tree.Instance_Of (Instance_Id => Id);
   end To_GPR_View;

   -----------------------
   -- To_Lang_Value_Map --
   -----------------------

   function To_Lang_Value_Map
      (Obj : JSON_Value) return GPR2.Containers.Lang_Value_Map
   is
      Result : GPR2.Containers.Lang_Value_Map;

      procedure CB
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value);

      --------
      -- CB --
      --------

      procedure CB
        (Name  : GNATCOLL.JSON.UTF8_String;
         Value : GNATCOLL.JSON.JSON_Value) is
      begin
         Result.Insert (GPR2."+" (GPR2.Optional_Name_Type (Name)), Value.Get);
      end CB;

   begin
      if GNATCOLL.JSON.Kind (Obj) = GNATCOLL.JSON.JSON_Object_Type then
         GNATCOLL.JSON.Map_JSON_Object
            (Val => Obj,
             CB  => CB'Unrestricted_Access);
      elsif GNATCOLL.JSON.Kind (Obj) /= GNATCOLL.JSON.JSON_Null_Type then
         raise GPR2_C_Exception with "lang_value_map expected";
      end if;

      return Result;
   end To_Lang_Value_Map;

   -----------------
   -- To_Language --
   -----------------

   function To_Language
      (Obj : JSON_Value) return GPR2.Language_Id
   is
   begin
      return GPR2."+" (To_Name (Obj));
   end To_Language;

   -----------------------
   -- To_Name_Value_Map --
   -----------------------

   function To_Name_Value_Map
      (Obj : JSON_Value) return GPR2.Containers.Name_Value_Map
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
         Value : GNATCOLL.JSON.JSON_Value) is
      begin
         Result.Insert (GPR2.Optional_Name_Type (Name), Value.Get);
      end CB;

   begin
      GNATCOLL.JSON.Map_JSON_Object
         (Val => Obj,
          CB  => CB'Unrestricted_Access);
      return Result;
   end To_Name_Value_Map;

   ------------------
   -- To_Path_Name --
   ------------------

   function To_Path_Name (Obj : JSON_Value) return GPR2.Path_Name.Object is
   begin
      return GPR2.Path_Name.Create_File (Name => To_Filename (Obj));
   end To_Path_Name;

   ----------------------
   -- To_Path_Name_Set --
   ----------------------

   function To_Path_Name_Set
      (Obj : JSON_Value) return GPR2.Path_Name.Set.Object
   is
      Result : GPR2.Path_Name.Set.Object;
   begin
      if GNATCOLL.JSON.Kind (Obj) = GNATCOLL.JSON.JSON_Array_Type then
         declare
            Arr : constant JSON_Array := GNATCOLL.JSON.Get (Obj);
         begin
            for Value of Arr loop
               Result.Append
                  (GPR2.Path_Name.Create_File (Name => To_Filename (Value)));
            end loop;
         end;
      elsif GNATCOLL.JSON.Kind (Obj) /= GNATCOLL.JSON.JSON_Null_Type then
         raise GPR2_C_Exception with "path_name list expected";
      end if;

      return Result;
   end To_Path_Name_Set;

   ---------------
   -- To_String --
   ---------------

   function To_String (Obj : JSON_Value) return String
   is
   begin
      if GNATCOLL.JSON.Kind (Obj) = GNATCOLL.JSON.JSON_String_Type then
         return GNATCOLL.JSON.Get (Obj);
      else
         raise GPR2_C_Exception with "expected string type (got " &
         GNATCOLL.JSON.JSON_Value_Type'Image (GNATCOLL.JSON.Kind (Obj)) & ")";
      end if;
   end To_String;

   function To_String (Obj : JSON_Value; Default : String) return String is
   begin
      if GNATCOLL.JSON.Kind (Obj) = GNATCOLL.JSON.JSON_Null_Type then
         return Default;
      else
         return To_String (Obj);
      end if;
   end To_String;

   function To_Unit_Index
      (Obj : JSON_Value; Default : Unit_Index) return Unit_Index
   is
   begin
      if GNATCOLL.JSON.Kind (Obj) = GNATCOLL.JSON.JSON_Int_Type then
         declare
            Obj_Int : constant Integer := GNATCOLL.JSON.Get (Obj);
         begin
            return Unit_Index (Obj_Int);
         end;
      elsif GNATCOLL.JSON.Kind (Obj) = GNATCOLL.JSON.JSON_Null_Type then
         return Default;
      else
         raise GPR2_C_Exception with "expected unit_index";
      end if;
   end To_Unit_Index;
end GPR2.C.JSON;
