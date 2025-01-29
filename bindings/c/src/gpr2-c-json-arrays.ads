--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GNATCOLL.JSON;

limited with GPR2.C.JSON.Objects;
limited with GPR2.C.JSON.Values;

package GPR2.C.JSON.Arrays is

   type JSON_Array is tagged private;

   function To_JSON_Value
     (Self : JSON_Array'Class) return GPR2.C.JSON.Values.JSON_Value;

   procedure Append
     (Self : in out JSON_Array'Class;
      Item : GPR2.C.JSON.Values.JSON_Value);
   --  Append an item to the array.

   procedure Append
     (Self : in out JSON_Array'Class;
      Item : GPR2.C.JSON.Objects.JSON_Object);
   --  Append an item to the array.

   --  GNATCOLL.JSON helpers

   function To_GNATCOLL_JSON_Value
     (Self : JSON_Array'Class) return GNATCOLL.JSON.JSON_Value;

private

   type JSON_Array is tagged record
      Value : GNATCOLL.JSON.JSON_Value;
   end record;

end GPR2.C.JSON.Arrays;
