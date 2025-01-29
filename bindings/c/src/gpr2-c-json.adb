--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Unchecked_Conversion;

with GPR2.C.JSON.Objects;
with GPR2.C.JSON.Values;

package body GPR2.C.JSON is

   function Convert is new Ada.Unchecked_Conversion
     (Interfaces.C.Strings.chars_ptr, C_Answer);

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
      Result_Obj  : GPR2.C.JSON.Objects.JSON_Object;
      Can_Decode  : Boolean := True;

   begin
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
            Handler
              (Request =>
                 GPR2.C.JSON.Values.To_JSON_Value (Request_Obj).To_JSON_Object,
               Result  => Result_Obj);
            Answer_Obj.Set_Field ("result", Result_Obj.To_GNATCOLL_JSON_Value);
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

end GPR2.C.JSON;
