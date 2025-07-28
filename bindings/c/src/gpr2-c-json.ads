--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Exceptions;

with GNATCOLL.JSON;

limited with GPR2.C.JSON.Objects;

package GPR2.C.JSON is

   subtype JSON_Value is GNATCOLL.JSON.JSON_Value;

   function Decode (Request : C_Request) return JSON_Value;
   --  Decodes a C_Request (char * in C) into a JSON_Value

   function Encode (Answer : JSON_Value) return C_Answer;
   --  Encodes a JSON_Value into a C_Answer (char * in C)

   type Bind_Handler is access procedure
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object);

   function Bind
     (Request : C_Request;
      Answer  : out C_Answer;
      Handler : Bind_Handler) return C_Status;
   --  Takes care of the boilerplate that decodes the incoming request and
   --  encodes the answer. Handler function receives the decoded JSON and
   --  returns the 'result' part of the answer.
   --  Bind catches all exceptions and sets properly the error in the answer.
   --  Note that Result is in fact an in/out parameter. When the handler is
   --  called, the Result JSON_Value is initialized as an empty JSON object
   --  (i.e. dictionary).

   -----------------------
   -- Decoding requests --
   -----------------------

   function Get_Status (Obj : JSON_Value) return C_Status;
   --  Returns the "status" member of an answer JSON object

   function Get_Result (Obj : JSON_Value) return JSON_Value;
   --  Returns the "result" member of an answer JSON object. Note that the
   --  returned value is mutable.

   ---------------------
   -- Encoding answer --
   ---------------------

   function Initialize_Answer return JSON_Value;
   --  Creates an answer JSON object:
   --     {
   --         "result":     {},
   --         "error_msg":  "",
   --         "error_name": "",
   --         "status": 0
   --     }

   procedure Set_Status (Obj : JSON_Value; Value : C_Status);
   --  Sets the status member of an answer JSON object Obj

   procedure Set_Status
     (Obj   : JSON_Value;
      Value : C_Status;
      E     : Ada.Exceptions.Exception_Occurrence);
   --  Sets the status, error_msg and error_name of an answer JSON object Obj

   --  Convertion functions to JSON values

   procedure Set (Obj : JSON_Value; Key : String; Value : JSON_Value);

end GPR2.C.JSON;
