--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.C.JSON.Values is

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Self : JSON_Value'Class) return Boolean is
   begin
      return GNATCOLL.JSON.Get (Self.Value);
   end To_Boolean;

   ----------------------------
   -- To_GNATCOLL_JSON_Value --
   ----------------------------

   function To_GNATCOLL_JSON_Value
     (Self : JSON_Value'Class) return GNATCOLL.JSON.JSON_Value is
   begin
      return Self.Value;
   end To_GNATCOLL_JSON_Value;

   --------------------
   -- To_JSON_Object --
   --------------------

   function To_JSON_Object
     (Self : JSON_Value'Class) return GPR2.C.JSON.Objects.JSON_Object is
   begin
      return GPR2.C.JSON.Objects.To_JSON_Object (Self.Value);
   end To_JSON_Object;

   -------------------
   -- To_JSON_Value --
   -------------------

   function To_JSON_Value (Value : Boolean) return JSON_Value is
   begin
      return (Defined => True, Value => GNATCOLL.JSON.Create (Value));
   end To_JSON_Value;

   -------------------
   -- To_JSON_Value --
   -------------------

   function To_JSON_Value (Value : Integer) return JSON_Value is
   begin
      return (Defined => True, Value => GNATCOLL.JSON.Create (Value));
   end To_JSON_Value;

   -------------------
   -- To_JSON_Value --
   -------------------

   function To_JSON_Value
     (Item : GNATCOLL.JSON.JSON_Value) return JSON_Value is
   begin
      return (Defined => True, Value => Item);
   end To_JSON_Value;

   -------------------
   -- To_JSON_Value --
   -------------------

   function To_JSON_Value (Value : String) return JSON_Value is
   begin
      return (Defined => True, Value => GNATCOLL.JSON.Create (Value));
   end To_JSON_Value;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : JSON_Value'Class) return String is
      use type GNATCOLL.JSON.JSON_Value_Type;

   begin
      if GNATCOLL.JSON.Kind (Self.Value) = GNATCOLL.JSON.JSON_String_Type then
         return GNATCOLL.JSON.Get (Self.Value);

      else
         return "";
      end if;
   end To_String;

end GPR2.C.JSON.Values;
