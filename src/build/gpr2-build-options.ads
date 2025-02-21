--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded;

with GPR2.Containers;

package GPR2.Build.Options is

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
      Restricted_To_Languages      : GPR2.Containers.Language_Set;
      --  Restrict the compile actions to the given set of languages
   end record;

end GPR2.Build.Options;
