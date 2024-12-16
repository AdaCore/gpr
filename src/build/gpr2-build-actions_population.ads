--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with GPR2.Build.Compilation_Unit;
with GPR2.Containers;
with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2.Build.Actions_Population is

   package Lang_Args is new Ada.Containers.Ordered_Maps
     (GPR2.Language_Id, GPR2.Containers.Value_List,
      GPR2."<",
      GPR2.Containers.Value_Type_List."=");

   type Build_Options is record
      --  Input/Output specified explicitly:

      Mains               : GPR2.Containers.Value_Set;
      --  List of mains to build, specified explicitly. If empty the list is
      --  retrieved from the root project
      Unit_Index          : GPR2.Unit_Index := GPR2.No_Index;
      --  Allows specifying an index for the main given on the command line
      Output_File         : Ada.Strings.Unbounded.Unbounded_String;
      --  If specified on the command line, the output file basename. This
      --  case supposes a single Main in Mains
      Create_Map_File     : Boolean := False;
      Mapping_File_Name   : Ada.Strings.Unbounded.Unbounded_String;
      --  Whether the linker should create a mapping file. If a specific map
      --  file name is needed, then Mapping_File_Name is set, otherwise
      --  <exec_file_name>.map is used.

      --  Modification of the actions behavior:

      No_Indirect_Imports          : Boolean := False;
      --  When set, sources have visibility only on source dirs of views
      --  that are directly imported by their owning view.  By default the
      --  source include path is transitive.
      No_SAL_Binding               : Boolean := False;
      --  Do not bind standalone libraries if a binder file already exists
      No_Run_Path                  : Boolean := False;
      --  Do not set the Run_Path for shared libraries resolution

      --  Restrictions on the actions to execute

      Restricted_Build_Phase       : Boolean := False;
      --  Some restriction applies to the build phases
      Compile_Phase_Mandated       : Boolean := False;
      --  Do the compilation phase in Restricted_Build_Phase mode
      Bind_Phase_Mandated          : Boolean := False;
      --  Do the binding phase in Restricted_Build_Phase mode
      Link_Phase_Mandated          : Boolean := False;
      --  Do the link phase in Restricted_Build_Phase mode
      Unique_Compilation           : Boolean := False;
      --  Just build the sources from the command line, or if none specified
      --  the sources from the root project
      Unique_Compilation_Recursive : Boolean := False;
      --  Similar to Unique_Recompilation, except that if no source is given
      --  on the command line, then this compiles the whole tree.
      No_Main_Subprogram           : Boolean := False;
      --  -z option: tell the binder to not generate an actual main subprogram
   end record;

   function Resolve_Mains
     (Tree    : GPR2.Project.Tree.Object;
      Options : Build_Options;
      Error   : out Boolean)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector;
   --  Utility function to resolve the actual list of Mains to consider for
   --  the given Build_Options. Error is set upon error retrieving an actual
   --  main.

   function Populate_Actions
     (Tree    : GPR2.Project.Tree.Object;
      Options : Build_Options) return Boolean;

end GPR2.Build.Actions_Population;
