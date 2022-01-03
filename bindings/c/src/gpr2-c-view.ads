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

--  C Binding to GPR2 Project Views API

package GPR2.C.View is

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

   function GPR2_Project_View_Properties
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  Returns in one call aggregated information about the selected view
   --
   --  Request:
   --      {'view_id': str}
   --
   --  Answer:
   --      {'name:  str,   # Project Name
   --       'path': str,   # Full path to the project file
   --       'dir':  str,   # Working directory
   --       'kind': str,
   --       'is_extending': bool,
   --       'is_extending_all': bool,
   --       'is_extended': bool,
   --       'is_aggregated': bool,
   --       'is_aggregated_in_library': bool,
   --       'mains': List[str],
   --       'library': Library
   --       'object_dir': str,
   --       'source_subdir': str,
   --       'exec_dir': str,
   --       'exec_suffix': str}

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

   function GPR2_Project_View_Sources
     (Request : C_Request; Answer : out C_Answer) return C_Status;
   --  Return sources for a given view
   --
   --  Request:
   --
   --      {'view_id'    : str,
   --       'need_update : Optional[bool] = True}
   --
   --  Answer:
   --      {'sources': List[Source]}

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

   pragma Export (C,
                  GPR2_Project_View_Sources,
                  "gpr2_prj_view_sources");
   pragma Export (C,
                  GPR2_Project_View_Attribute,
                  "gpr2_prj_view_attribute");
   pragma Export (C,
                  GPR2_Project_View_Properties,
                  "gpr2_prj_view_properties");
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

end GPR2.C.View;
