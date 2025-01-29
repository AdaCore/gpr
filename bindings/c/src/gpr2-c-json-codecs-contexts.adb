--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.C.JSON.Codecs.Contexts is

   procedure Internal_Context_Decode
     (Self  : in out GPR2.Context.Object;
      Key   : String;
      Value : JSON_Value);

   ------------
   -- Decode --
   ------------

   function Decode
     (Value : GPR2.C.JSON.Values.JSON_Value) return GPR2.Context.Object
   is
      procedure Decode is
        new GNATCOLL.JSON.Gen_Map_JSON_Object (GPR2.Context.Object);

   begin
      return Result : GPR2.Context.Object do
         Decode
           (Value.To_GNATCOLL_JSON_Value,
            Internal_Context_Decode'Access,
            Result);
      end return;
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Object : GPR2.Context.Object)
      return GPR2.C.JSON.Objects.JSON_Object is
   begin
      return Result : GPR2.C.JSON.Objects.JSON_Object do
         for Position in Object.Iterate loop
            Result.Insert
              (String (GPR2.Context.Key_Value.Key (Position)),
               String (GPR2.Context.Key_Value.Element (Position)));
         end loop;
      end return;
   end Encode;

   -----------------------------
   -- Internal_Context_Decode --
   -----------------------------

   procedure Internal_Context_Decode
     (Self  : in out GPR2.Context.Object;
      Key   : String;
      Value : JSON_Value) is
   begin
      Self.Insert (External_Name_Type (Key), GNATCOLL.JSON.Get (Value));
   end Internal_Context_Decode;

end GPR2.C.JSON.Codecs.Contexts;
