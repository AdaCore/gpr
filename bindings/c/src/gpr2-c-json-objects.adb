--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Arrays;
with GPR2.C.JSON.Values;

package body GPR2.C.JSON.Objects is

   use type GNATCOLL.JSON.JSON_Value_Type;

   procedure Create_If_Null (Self : in out JSON_Object'Class);

   --------------------
   -- Create_If_Null --
   --------------------

   procedure Create_If_Null (Self : in out JSON_Object'Class) is
   begin
      if Self.Value.Kind = GNATCOLL.JSON.JSON_Null_Type then
         Self.Value := GNATCOLL.JSON.Create_Object;
      end if;
   end Create_If_Null;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : GPR2.C.JSON.Arrays.JSON_Array) is
   begin
      Self.Create_If_Null;
      Self.Value.Set_Field (Name, Value.To_GNATCOLL_JSON_Value);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : GPR2.C.JSON.Objects.JSON_Object) is
   begin
      Self.Create_If_Null;
      Self.Value.Set_Field (Name, Value.Value);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : GPR2.C.JSON.Values.JSON_Value) is
   begin
      Self.Create_If_Null;
      Self.Value.Set_Field (Name, Value.To_GNATCOLL_JSON_Value);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : Integer) is
   begin
      Self.Create_If_Null;
      Self.Value.Set_Field (Name, Value);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : String) is
   begin
      Self.Create_If_Null;
      Self.Value.Set_Field (Name, Value);
   end Insert;

   ----------------------------
   -- To_GNATCOLL_JSON_Value --
   ----------------------------

   function To_GNATCOLL_JSON_Value
     (Self : JSON_Object'Class) return GNATCOLL.JSON.JSON_Value is
   begin
      if Self.Value.Kind = GNATCOLL.JSON.JSON_Null_Type then
         return GNATCOLL.JSON.Create_Object;
      else
         return Self.Value;
      end if;
   end To_GNATCOLL_JSON_Value;

   --------------------
   -- To_JSON_Object --
   --------------------

   function To_JSON_Object
     (Value : GNATCOLL.JSON.JSON_Value) return JSON_Object is
   begin
      return Result : JSON_Object do
         if GNATCOLL.JSON.Kind (Value) = GNATCOLL.JSON.JSON_Object_Type then
            Result.Value := Value;
         end if;
      end return;
   end To_JSON_Object;

   -------------------
   -- To_JSON_Value --
   -------------------

   function To_JSON_Value
     (Self : JSON_Object'Class) return GPR2.C.JSON.Values.JSON_Value is
   begin
      return
        GPR2.C.JSON.Values.To_JSON_Value
          (if Self.Value.Kind = GNATCOLL.JSON.JSON_Null_Type
           then GNATCOLL.JSON.Create_Object
           else Self.Value);
   end To_JSON_Value;

   -----------
   -- Value --
   -----------

   function Value
     (Self : JSON_Object'Class;
      Key  : String) return GPR2.C.JSON.Values.JSON_Value is
   begin
      return
        (if Self.Value.Kind = GNATCOLL.JSON.JSON_Null_Type
           then GPR2.C.JSON.Values.Undefined_Value
           elsif GNATCOLL.JSON.Has_Field (Self.Value, Key)
             then GPR2.C.JSON.Values.To_JSON_Value
                    (GNATCOLL.JSON.JSON_Value'
                       (GNATCOLL.JSON.Get (Self.Value, Key)))
             else GPR2.C.JSON.Values.Undefined_Value);
   end Value;

end GPR2.C.JSON.Objects;
