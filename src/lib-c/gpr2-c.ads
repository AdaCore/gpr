------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--  -- Memory management --
--
--  Memory allocation/deallocation of the request is managed by the caller.
--  Caller should call gpr2_free_answer to release memory allocated for the
--  answer.

with GPR2.Project.Tree;
with System;

package GPR2.C is

   type C_Request is new System.Address;
   --  Request C null terminated string

   type C_Answer  is new System.Address;
   --  Answer C null terminater string

   type C_Status is new Integer;
   --  Integer status

   OK              : constant C_Status := 0;
   Invalid_Request : constant C_Status := 1;
   Call_Error      : constant C_Status := 2;
   Unknown_Error   : constant C_Status := 3;

   function GPR2_Project_Tree_Load_Autoconf
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Load_Autoconf binding
   --
   --  Request:
   --      {'filename':          str,
   --       'context':           Dict[str, str],
   --       'subdirs':           Optional[str],
   --       'src_subdirs':       Optional[str],
   --       'check_shared_lib':  Optional[bool] = True,
   --       'implicit_project':  Optional[bool] = False,
   --       'absent_dir_error':  Optional[bool] = False,
   --       'implicit_with':     List[str],
   --       'target':            Optional[str],
   --       'language_runtimes': Optional[Dict[str, str]]}
   --
   --  Answer:
   --      {'project_id': str},

   function GPR2_Project_Tree_Root_Project
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Root_Project
   --
   --  Request:
   --      {'project_id': str}
   --
   --  Answer:
   --      {'view_id': str}

   function GPR2_Project_Tree_Get_View
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Get_View binding
   --
   --  Request:
   --      {'project_id': str,
   --       'unit':       str}
   --
   --  Answer:
   --      {'view_id': str}
   --

   function GPR2_Project_View_Attribute
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'view_id': str,
   --       'name':    str,
   --       'index':   Optional[str]}
   --
   --  Answer:
   --      {'attr': Any}
private

   pragma Export (C,
                  GPR2_Project_Tree_Load_Autoconf,
                  "gpr2_prj_tree_load_autoconf");
   pragma Export (C,
                  GPR2_Project_Tree_Get_View,
                  "gpr2_prj_tree_get_view");
   pragma Export (C,
                  GPR2_Project_View_Attribute,
                  "gpr2_prj_view_attribute");
   pragma Export (C,
                  GPR2_Project_Tree_Root_Project,
                  "gpr2_prj_tree_root_project");

end GPR2.C;
