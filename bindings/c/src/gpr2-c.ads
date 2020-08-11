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
--  Message: represent GPR2 log messages
--
--     {'level':   str,
--      'message': str,
--      'sloc':    Sloc}
--
--  Sloc: represent a reference to/in a file
--
--     {'filename': str,
--      'line':     Optional[int],
--      'column':   Optional[int]}
--
--  Variable: represent a project variable
--
--     {'name': str,
--      'type': Optional[str],
--      'value': Union[List[str], str]}

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

   function GPR2_Project_Tree_Load
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  Load a project tree.
   --
   --  filename: location of the gpr file to load.
   --  implicit_project: if True then load behaves as if filename is
   --      located in the current directory. If False, directoty in
   --      which the project file is located is used. Using True allows
   --      implementation of "default projects" in tools.
   --  context: a JSON object that contains values for external variables
   --  configuration_id: optional id to a configuration. If None then use
   --      the default configuration.
   --  build_path: root directory where build artefacts are stored.
   --      If None then object, libraries and executable location is
   --      relative to the project location.
   --  subdirs: If not None, then add subdirs as suffix to object, library
   --      and executables directories.
   --  src_subdirs: If not None, then add as source directory for each
   --      project the src_subdirs subdirectory of the object directory.
   --  check_shared_lib: if True, check in the project tree, that all
   --       projects describing shared libraries do not import static
   --       library or standard project.
   --  absent_dir_error: TODO
   --  implicit_with: a list of implicitely withed projects
   --  target: target name
   --  language_runtimes: JSON object associating to a language name
   --       a runtime name.
   --
   --  Request:
   --      {'filename':          str,
   --       'implicit_project':  Optional[bool] = False,
   --       'context':           Optional[Dict[str, str]],
   --       'configuration_id':  Optional[str],
   --       'build_path':        Optional[str],
   --       'subdirs':           Optional[str],
   --       'src_subdirs':       Optional[str],
   --       'check_shared_lib':  Optional[bool] = True,
   --       'absent_dir_error':  Optional[bool] = False,
   --       'implicit_with':     List[str],
   --       'target':            Optional[str],
   --       'language_runtimes': Optional[Dict[str, str]]}
   --
   --  Answer:
   --      {'tree_id': str}

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
   --  Return messages associated with the project tree.
   --
   --  tree_id: gpr2 tree id
   --  information: if True select messages for which level is Information
   --  warning: if True select messages for which level is Warning
   --  error: if True select messages for which level is Error
   --  read: if set to True return already read messages
   --  unread: if set to True return unread messages. Note that all returned
   --      unread messages are marked read after this call.
   --
   --  Request:
   --      {'tree_id':                  str,
   --       'information':              Optional[bool] = True,
   --       'warning':                  Optional[bool] = True,
   --       'error':                    Optional[bool] = True,
   --       'read':                     Optional[bool] = True,
   --       'unread':                   Optional[bool] = True}
   --
   --  Answer:
   --      {'messages': List[Message]}

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

   function GPR2_Project_View_Unload
      (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  Unload a project view.
   --
   --  After a call to that function, view_id should not be used
   --  in any further calls.
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {}

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

   function GPR2_Project_Tree_Add_Tool_Prefix
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'tree_id'   : str,
   --       'tool_name' : str}
   --
   --  Answer:
   --      {'full_tool_name' : str}

   function GPR2_Project_Tree_Set_Context
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'tree_id': str,
   --      {'context':
   --          ['name':  str,
   --           'value': str]}
   --
   --  Answer:
   --      {}

   function GPR2_Project_Tree_Invalidate_Sources
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'tree_id': str,
   --       'view_id': Optional[str] = none}
   --
   --  Answer:
   --      {}

   function GPR2_Project_Tree_Update_Sources
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'tree_id'       : str,
   --       'stop_on_error' : Optional[bool] = true}
   --
   --  Answer:
   --      {}

   function GPR2_Project_Tree_Register_Project_Search_Path
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'tree_id': str,
   --       'dir'    : str}
   --
   --  Answer:
   --      {}

   function GPR2_Project_Tree_Project_Search_Paths
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'tree_id': str,}
   --
   --  Answer:
   --      {'project_search_paths' : [str]}

   function GPR2_Project_Tree_Get_File
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attribute binding
   --
   --  Request:
   --
   --      {'tree_id'          : str,
   --       'base_name'        : str,
   --       'view'             : Optional[ProjectView] = None,
   --       'use_source_path'  : Optional[bool] = true,
   --       'use_object_path'  : Optional[bool] = true,
   --       'predefined_only'  : Optional[bool] = false,
   --       'return_ambiguous' : Optional[bool] = true}
   --
   --  Answer:
   --      {'filename'         : str}

   function GPR2_Project_View_Aggregate
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Aggregate
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'aggregate_view_id': str}

   function GPR2_Project_View_Aggregated
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Aggregated
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'aggregated_view_ids': [str]}

   function GPR2_Project_View_Artifacts
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Artifacts
   --
   --  Request:
   --      {'view_id':  str}
   --
   --  Answer:
   --      {'artifacts': [str]}

   function GPR2_Project_View_Binder_Artifacts
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Binder_Artifacts
   --
   --  Request:
   --      {'view_id':  str,
   --       'name':     str,
   --       'language': Optional[str] = None}
   --
   --  Answer:
   --      {'binder_artifacts': [str]}

   function GPR2_Project_View_Context
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Context
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'context': Dict[str, str]}

   function GPR2_Project_View_Extended
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Extended
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'extended_view_id': str}

   function GPR2_Project_View_Extending
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Extending
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'extending_view_id': str}

   function GPR2_Project_View_Imports
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Imports
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'imported_view_ids': [str]}

   function GPR2_Project_View_Invalidate_Sources
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Invalidate_Sources
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {}

   function GPR2_Project_View_Source_Path
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Source_Path
   --
   --  Request:
   --      {'view_id':     str,
   --       'filename':    str,
   --       'need_update': Optional[bool] = True}
   --
   --  Answer:
   --      {'source_path': str}

   function GPR2_Project_View_View_For
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.View_For
   --
   --  Request:
   --      {'view_id': str,
   --       'name':    str}
   --
   --  Answer:
   --      {'found_view_id': str}

   function GPR2_Project_View_Attributes
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attributes binding
   --  Also package's attributes are returned
   --
   --  Request:
   --
   --      {'view_id'          : str}
   --
   --  Answer:
   --      {'attributes': Any,
   --       'packages':   Any}

   function GPR2_Project_View_Types
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  GPR2.Project.View.Attributes binding
   --  Also package's attributes are returned
   --
   --  Request:
   --
   --      {'view_id'          : str}
   --
   --  Answer:
   --      {'types': Any}

   function GPR2_Project_View_Variables
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  Return variables for a given view
   --
   --  Request:
   --
   --      {'view_id' : str}
   --
   --  Answer:
   --      {'variables': Dict[str, Variable],
   --       'packages':  Dict[str, Dict[str, Variable]]}

private

   pragma Export (C, GPR2_Free_Answer, "gpr2_free_answer");

   pragma Export (C,
                  GPR2_Project_Tree_Load,
                  "gpr2_prj_tree_load");
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
   pragma Export (C,
                  GPR2_Project_Tree_Add_Tool_Prefix,
                  "gpr2_prj_tree_add_tool_prefix");
   pragma Export (C,
                  GPR2_Project_Tree_Set_Context,
                  "gpr2_prj_tree_set_context");
   pragma Export (C,
                  GPR2_Project_Tree_Invalidate_Sources,
                  "gpr2_prj_tree_invalidate_src");
   pragma Export (C,
                  GPR2_Project_Tree_Update_Sources,
                  "gpr2_prj_tree_update_src");
   pragma Export (C,
                  GPR2_Project_Tree_Register_Project_Search_Path,
                  "gpr2_prj_tree_register_prj_search_path");
   pragma Export (C,
                  GPR2_Project_Tree_Project_Search_Paths,
                  "gpr2_prj_tree_prj_search_paths");
   pragma Export (C,
                  GPR2_Project_Tree_Get_File,
                  "gpr2_prj_tree_get_file");
   pragma Export (C,
                  GPR2_Project_View_Imports,
                  "gpr2_prj_view_imports");
   pragma Export (C,
                  GPR2_Project_View_Extended,
                  "gpr2_prj_view_extended");
   pragma Export (C,
                  GPR2_Project_View_Extending,
                  "gpr2_prj_view_extending");
   pragma Export (C,
                  GPR2_Project_View_Aggregated,
                  "gpr2_prj_view_aggregated");
   pragma Export (C,
                  GPR2_Project_View_Aggregate,
                  "gpr2_prj_view_aggregate");
   pragma Export (C,
                  GPR2_Project_View_Context,
                  "gpr2_prj_view_context");
   pragma Export (C,
                  GPR2_Project_View_View_For,
                  "gpr2_prj_view_view_for");
   pragma Export (C,
                  GPR2_Project_View_Source_Path,
                  "gpr2_prj_view_source_path");
   pragma Export (C,
                  GPR2_Project_View_Invalidate_Sources,
                  "gpr2_prj_view_invalidate_sources");
   pragma Export (C,
                  GPR2_Project_View_Binder_Artifacts,
                  "gpr2_prj_view_binder_artifacts");
   pragma Export (C,
                  GPR2_Project_View_Artifacts,
                  "gpr2_prj_view_artifacts");
   pragma Export (C,
                  GPR2_Project_View_Attributes,
                  "gpr2_prj_view_attributes");
   pragma Export (C,
                  GPR2_Project_View_Types,
                  "gpr2_prj_view_types");
   pragma Export (C,
                  GPR2_Project_View_Unload,
                  "gpr2_prj_view_unload");
   pragma Export (C,
                  GPR2_Project_View_Variables,
                  "gpr2_prj_view_variables");

end GPR2.C;
