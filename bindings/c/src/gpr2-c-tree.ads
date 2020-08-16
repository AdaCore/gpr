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

--  C Binding to GPR2 Project Trees API

package GPR2.C.Tree is

   function GPR2_Project_Tree_Load
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  Loads a project tree.
   --
   --  filename: path to the gpr file to load.
   --  project_dir: if True then load behaves as if filename is
   --      located in the project_dir directory. If False, directory in
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
   --  implicit_with: a list of implicitly withed projects
   --  target: target name
   --  language_runtimes: JSON object associating to a language name
   --       a runtime name.
   --
   --  Request:
   --      {'filename':          str,
   --       'project_dir':       Optional[str],
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
   --  Returns messages associated with the project tree.
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

private

   pragma Export (C,
                  GPR2_Project_Tree_Load,
                  "gpr2_prj_tree_load");
   pragma Export (C,
                  GPR2_Project_Tree_Get_View,
                  "gpr2_prj_tree_get_view");
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
end GPR2.C.Tree;
