--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.JSON;

limited with GPR2.C.JSON.Arrays;
limited with GPR2.C.JSON.Values;

package GPR2.C.JSON.Objects is

   type JSON_Object is tagged private;

   function To_JSON_Value
     (Self : JSON_Object'Class) return GPR2.C.JSON.Values.JSON_Value;

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : GPR2.C.JSON.Values.JSON_Value);

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : GPR2.C.JSON.Arrays.JSON_Array);

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : GPR2.C.JSON.Objects.JSON_Object);

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : String);

   procedure Insert
     (Self  : in out JSON_Object'Class;
      Name  : String;
      Value : Integer);

   function Value
     (Self : JSON_Object'Class;
      Key  : String) return GPR2.C.JSON.Values.JSON_Value;

   function To_GNATCOLL_JSON_Value
     (Self : JSON_Object'Class) return GNATCOLL.JSON.JSON_Value;

   function To_JSON_Object
     (Value : GNATCOLL.JSON.JSON_Value) return JSON_Object;

private

   type JSON_Object is tagged record
      Value : GNATCOLL.JSON.JSON_Value;
   end record;

end GPR2.C.JSON.Objects;
