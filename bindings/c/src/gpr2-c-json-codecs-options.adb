--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Codecs.Contexts;

package body GPR2.C.JSON.Codecs.Options is

   procedure Internal_Options_Decode
     (Self  : in out GPR2.Options.Object;
      Key   : String;
      Value : JSON_Value);

   ------------
   -- Decode --
   ------------

   function Decode
     (Value : GPR2.C.JSON.Values.JSON_Value) return GPR2.Options.Object
   is
      procedure Decode is
        new GNATCOLL.JSON.Gen_Map_JSON_Object (GPR2.Options.Object);

   begin
      return Result : GPR2.Options.Object do
         Decode
           (Value.To_GNATCOLL_JSON_Value,
            Internal_Options_Decode'Access,
            Result);
      end return;
   end Decode;

   -----------------------------
   -- Internal_Options_Decode --
   -----------------------------

   procedure Internal_Options_Decode
     (Self  : in out GPR2.Options.Object;
      Key   : String;
      Value : JSON_Value) is
   begin
      if Key = "P" then
         Self.Add_Switch (GPR2.Options.P, GNATCOLL.JSON.Get (Value));

      elsif Key = "context" then
         if not GNATCOLL.JSON.Is_Empty (Value) then
            Self.Add_Context
              (GPR2.C.JSON.Codecs.Contexts.Decode
                 (GPR2.C.JSON.Values.To_JSON_Value (Value)));
         end if;

      else
         raise Constraint_Error with "unknown option";
      end if;
   end Internal_Options_Decode;

end GPR2.C.JSON.Codecs.Options;
