--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Values;
with GPR2.Source_Reference;

package body GPR2.C.JSON.Codecs.Messages is

   function Encode
     (Value : GPR2.Message.Level_Value) return GPR2.C.JSON.Values.JSON_Value;

   function Encode
     (Object : GPR2.Source_Reference.Object)
      return GPR2.C.JSON.Objects.JSON_Object;

   ------------
   -- Encode --
   ------------

   function Encode
     (Value : GPR2.Message.Level_Value) return GPR2.C.JSON.Values.JSON_Value is
   begin
      return GPR2.C.JSON.Values.To_JSON_Value
        (case Value is
            when GPR2.Message.Warning  => "warning",
            when GPR2.Message.Error    => "error",
            when GPR2.Message.End_User => "end_user",
            when GPR2.Message.Hint     => "hint",
            when GPR2.Message.Lint     => "lint");
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Object : GPR2.Message.Object) return GPR2.C.JSON.Objects.JSON_Object is
   begin
      return Result : GPR2.C.JSON.Objects.JSON_Object do
         if Object.Is_Defined then
            Result.Insert ("message", Object.Message);
            Result.Insert ("level", Encode (Object.Level));
            Result.Insert ("sloc", Encode (Object.Sloc));
         end if;
      end return;
   end Encode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Object : GPR2.Source_Reference.Object)
      return GPR2.C.JSON.Objects.JSON_Object is
   begin
      return Result : GPR2.C.JSON.Objects.JSON_Object do
         if Object.Is_Defined then
            Result.Insert ("filename", String (Object.Filename));
            Result.Insert
              ("line",
               (if Object.Has_Source_Reference
                  then GPR2.C.JSON.Values.To_JSON_Value (Object.Line)
                  else GPR2.C.JSON.Values.Null_Value));
            Result.Insert
              ("column",
               (if Object.Has_Source_Reference
                  then GPR2.C.JSON.Values.To_JSON_Value (Object.Column)
                  else GPR2.C.JSON.Values.Null_Value));
         end if;
      end return;
   end Encode;

end GPR2.C.JSON.Codecs.Messages;
