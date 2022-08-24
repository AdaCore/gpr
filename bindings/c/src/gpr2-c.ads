--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides a low-level API that can be used to creates bindings
--  to the GPR2 library.
--
--  Variable: represents a project variable
--
--     {'name': str,
--      'type': Optional[str],
--      'value': Union[List[str], str]}
--
--  Source: represents a source
--
--     {'path': str,
--      'is_aggregated': bool,
--      'is_compilable': bool,
--      'is_interface': bool,
--      'has_naming_exception': bool,
--      'is_main': bool,
--      'language': str,
--      'timestamp': int}

with Interfaces.C.Strings;

package GPR2.C is

   type C_Function is new Integer;
   --  Function to invoke

   --  Tree functions
   TREE_LOAD                   : constant C_Function := 1;
   TREE_UNLOAD                 : constant C_Function := 2;
   TREE_LOG_MESSAGES           : constant C_Function := 3;
   TREE_INVALIDATE_SOURCE_LIST : constant C_Function := 4;
   TREE_UPDATE_SOURCE_LIST     : constant C_Function := 5;
   TREE_UPDATE_SOURCE_INFOS    : constant C_Function := 6;
   VIEW_LOAD                   : constant C_Function := 7;
   VIEW_ATTRIBUTE              : constant C_Function := 8;
   VIEW_SOURCES                : constant C_Function := 9;
   VIEW_UNITS                  : constant C_Function := 10;
   SOURCE_DEPENDENCIES         : constant C_Function := 11;
   SOURCE_UPDATE_SOURCE_INFOS  : constant C_Function := 12;

   type C_Request is new Interfaces.C.Strings.chars_ptr;
   --  Request C null terminated string

   type C_Answer is new Interfaces.C.Strings.chars_ptr;
   --  Answer C null terminater string

   type C_Status is new Integer;
   --  Integer status

   GPR2_C_Exception : exception;

   OK              : constant C_Status := 0;
   Invalid_Request : constant C_Status := 1;
   Call_Error      : constant C_Status := 2;
   Unknown_Error   : constant C_Status := 3;

   procedure GPR2_Free_Answer (Answer : C_Answer);
   --  Releases the memory held by an answer

   function GPR2_Request
      (Fun : C_Function; Request : C_Request; Answer : out C_Answer)
      return C_Status;
   --  Emits a GPR2 request.
   --
   --  Fun is the function to invoke.
   --  Request contains the function request.

private

   pragma Export (C, GPR2_Free_Answer, "gpr2_free_answer");
   pragma Export (C, GPR2_Request, "gpr2_request");

end GPR2.C;
