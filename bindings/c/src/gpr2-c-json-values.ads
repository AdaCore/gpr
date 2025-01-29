--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.JSON;

with GPR2.C.JSON.Objects;

package GPR2.C.JSON.Values is

   type JSON_Value is tagged private;

   Undefined_Value : constant JSON_Value;
   Null_Value      : constant JSON_Value;

   function To_JSON_Value (Value : Boolean) return JSON_Value;

   function To_JSON_Value (Value : String) return JSON_Value;

   function To_JSON_Value (Value : Integer) return JSON_Value;

   function To_Boolean (Self : JSON_Value'Class) return Boolean;

   function To_String (Self : JSON_Value'Class) return String;

   function To_JSON_Object
     (Self : JSON_Value'Class) return GPR2.C.JSON.Objects.JSON_Object;

   --  GNATCOLL.JSON helpers

   function To_GNATCOLL_JSON_Value
     (Self : JSON_Value'Class) return GNATCOLL.JSON.JSON_Value;

   function To_JSON_Value (Item : GNATCOLL.JSON.JSON_Value) return JSON_Value;

private

   type JSON_Value is tagged record
      Defined : Boolean := False;
      Value   : GNATCOLL.JSON.JSON_Value;
   end record;

   Undefined_Value : constant JSON_Value := (others => <>);
   Null_Value      : constant JSON_Value := (Defined => True, Value => <>);

end GPR2.C.JSON.Values;
