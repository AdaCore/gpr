--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Objects;
with GPR2.C.JSON.Values;

package body GPR2.C.JSON.Arrays is

   use type GNATCOLL.JSON.JSON_Value_Type;

   procedure Create_If_Null (Self : in out JSON_Array'Class);

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out JSON_Array'Class;
      Item : GPR2.C.JSON.Objects.JSON_Object) is
   begin
      Self.Create_If_Null;
      Self.Value.Append (Item.To_GNATCOLL_JSON_Value);
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out JSON_Array'Class;
      Item : GPR2.C.JSON.Values.JSON_Value) is
   begin
      Self.Create_If_Null;
      Self.Value.Append (Item.To_GNATCOLL_JSON_Value);
   end Append;

   --------------------
   -- Create_If_Null --
   --------------------

   procedure Create_If_Null (Self : in out JSON_Array'Class) is
   begin
      if Self.Value.Kind = GNATCOLL.JSON.JSON_Null_Type then
         declare
            Aux : GNATCOLL.JSON.JSON_Array;

         begin
            Self.Value := GNATCOLL.JSON.Create (Aux);
         end;
      end if;
   end Create_If_Null;

   ----------------------------
   -- To_GNATCOLL_JSON_Value --
   ----------------------------

   function To_GNATCOLL_JSON_Value
     (Self : JSON_Array'Class) return GNATCOLL.JSON.JSON_Value is
   begin
      if Self.Value.Kind = GNATCOLL.JSON.JSON_Null_Type then
         declare
            Aux : GNATCOLL.JSON.JSON_Array;

         begin
            return GNATCOLL.JSON.Create (Aux);
         end;

      else
         return Self.Value;
      end if;
   end To_GNATCOLL_JSON_Value;

   -------------------
   -- To_JSON_Value --
   -------------------

   function To_JSON_Value
     (Self : JSON_Array'Class) return GPR2.C.JSON.Values.JSON_Value is
   begin
      return GPR2.C.JSON.Values.To_JSON_Value (Self.To_GNATCOLL_JSON_Value);
   end To_JSON_Value;

end GPR2.C.JSON.Arrays;
