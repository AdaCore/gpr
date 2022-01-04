------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2020-2022, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a low-level API that can be used to creates bindings
--  to the GPR2 library.
--
--  Each function declared in this package has the same signature and follows
--  the same protocol.
--
--  -- C signature --
--
--  The C equivalent signature is always:
--      status NAME (char *request, char **answer);
--
--  -- Protocol --
--
--  Request is a JSON string containing one JSON object. The structure of the
--  object depends on the called method (see documentation of each method).
--
--  Answer is a JSON string containing on JSON object with the following
--  structure:
--
--    {'result': Dict,
--     'status': int,
--     'error_msg': str,
--     'error_name': str}
--
--  If status is set to 0 (OK), then the 'result' member contains the return
--  value. The structure of the returned value is described in each function
--  If status is set to another value, the call failed. In that case, the
--  answer object contains the error name and message in 'error_msg' and
--  'error_name'.
--
--  In the documentation Python type hinting annotations are used. Note that
--  in a request, if a JSON object member type is set to Optional it means
--  that the member is not mandatory in the request and that the default value
--  is used. For answers optional members are always set. If no default value
--  is provided then the value of the member is set to null.
--
--  -- Memory management --
--
--  Memory allocation/deallocation of the request is managed by the caller.
--  Caller should call gpr2_free_answer to release memory allocated for the
--  answer.
--
--  -- Complex structure JSON formats --
--
--  Message: represents GPR2 log messages
--
--     {'level':   str,
--      'message': str,
--      'sloc':    Sloc}
--
--  Sloc: represents a reference to/in a file
--
--     {'filename': str,
--      'line':     Optional[int],
--      'column':   Optional[int]}
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

private

   pragma Export (C, GPR2_Free_Answer, "gpr2_free_answer");

end GPR2.C;
