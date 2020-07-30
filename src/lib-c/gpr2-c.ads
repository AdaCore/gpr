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
   --      {'tree_id': str},

   function GPR2_Project_Tree_Unload
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Unload binding.
   --
   --  After a call to that function, tree_id should not be used
   --  in any further calls.
   --
   --  Request:
   --      {'tree_id': str}
   --
   --  Answer:
   --      {}

   function GPR2_Project_Tree_Root_Project
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Root_Project
   --
   --  Request:
   --      {'tree_id': str}
   --
   --  Answer:
   --      {'view_id': str}

   function GPR2_Project_Tree_Log_Messages
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Log_Messages
   --
   --  Request:
   --      {'tree_id':                  str,
   --       'information':              Optional[bool] = True,
   --       'warning':                  Optional[bool] = True,
   --       'error':                    Optional[bool] = True,
   --       'read':                     Optional[bool] = True,
   --       'unread':                   Optional[bool] = True,
   --       'full_path_name':           Optional[bool] = False,
   --       'information_output_level': Optional[str] = 'long',
   --       'warning_output_level':     Optional[str] = 'long',
   --       'error_output_level':       Optional[str] = 'long'}
   --
   --       output level can be 'none', 'short' or 'long'
   --       all returned messages are automatically marked read
   --       information/warning/error/read/unread controls what messages are
   --       returned
   --
   --  Answer:
   --      {'messages':
   --          ['level':            str,
   --           'message':          str,
   --           'formatted_message: str,'
   --           'filename':         str,
   --           'line':             Optional[int],
   --           'column':           Optional[int]]}

   function GPR2_Project_Tree_Properties
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Target
   --  GPR2.Project.Tree.Archive_Suffix
   --  GPR2.Project.Tree.Subdirs
   --  GPR2.Project.Tree.Src_Subdirs
   --  GPR2.Project.Tree.Build_Path
   --
   --  Request:
   --      {'tree_id': str}
   --
   --  Answer:
   --      {'target':         str,
   --       'archive_suffix': str,
   --       'subdirs':        Optional[str],
   --       'src_subdirs':    Optional[str],
   --       'build_path':     Optional[str]}

   function GPR2_Project_Tree_Get_View
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Get_View binding
   --
   --  Request:
   --      {'tree_id': str,
   --       'unit':       str}
   --
   --  Answer:
   --      {'view_id': str}
   --

   function GPR2_Project_View_Information
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  Returns in one call several information about the selected view
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'path_name': str,   # GPR2.Project.View.Path_Name
   --       'dir_name':  str,   # GPR2.Project.View.Dir_Name
   --       'name':      str}   # GPR2.Project.View.Name

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

   function GPR2_Project_Tree_Context
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Context
   --
   --  Request:
   --      {'tree_id': str}
   --
   --  Answer:
   --      {'context': Dict[str, str]}

   function GPR2_Project_Tree_Language_Properties
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.Tree.Runtime, Object_Suffix, Dependency_Suffix  binding
   --
   --  Request:
   --
   --      {'tree_id':  str,
   --       'language': str}
   --
   --  Answer:
   --      {'runtime':           str,
   --       'object_suffix':     str,
   --       'dependency_suffix': str}

private

   pragma Export (C, GPR2_Free_Answer, "gpr2_free_answer");

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
                  GPR2_Project_View_Information,
                  "gpr2_prj_view_information");
   pragma Export (C,
                  GPR2_Project_Tree_Root_Project,
                  "gpr2_prj_tree_root_project");
   pragma Export (C,
                  GPR2_Project_Tree_Properties,
                  "gpr2_prj_tree_properties");
   pragma Export (C,
                  GPR2_Project_Tree_Unload,
                  "gpr2_prj_tree_unload");
   pragma Export (C,
                  GPR2_Project_Tree_Log_Messages,
                  "gpr2_prj_tree_log_messages");
   pragma Export (C,
                  GPR2_Project_Tree_Context,
                  "gpr2_prj_tree_context");
   pragma Export (C,
                  GPR2_Project_Tree_Language_Properties,
                  "gpr2_prj_tree_language_properties");
end GPR2.C;
