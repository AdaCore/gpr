--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package is provided to simplify and normalize GPR common switches
--  support.

--  Once options object is configured (by parsing the command line or calling
--  On_Switch/Finalize functions directly), it can be used to load a Tree.

with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Environment;
with GPR2.File_Readers;
with GPR2.KB;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;

package GPR2.Options is

   Usage_Error : exception;
   --  Raised when a wrong usage is detected

   type Object is tagged private;
   --  This object handles all common gpr switches used when loading a project.
   --  Common gpr switches used when loading a project are described below in
   --  Option type definition.
   --  When processing the command line or any load configuration data, call
   --  Add_Switch procedure to configure this object.
   --  When command line or configuration parsing is done, Finalize procedure
   --  should be called prior to be able to call any getter or the Load_Project
   --  procedure.

   Empty_Options : constant Object;

   type Option is
     (AP,
      --  -aP<dir> or -aP <dir> Add directory dir to project search path

      Autoconf,
      --  --autoconf=file.cgpr Specify/create the main config project file name

      Config,
      --  --config=file.cgpr Specify the configuration project file name

      Db,
      --  --db dir Parse dir as an additional knowledge base

      Db_Minus,
     --  --db- Do not load the standard knowledge base

     Implicit_With,
      --  --implicit-with=filename
      --      Add the given projects as a dependency on all loaded projects

      No_Project,
      --  --no-project Do not use project file

      P,
      --  -Pproj<.gpr> or -P proj<.gpr> Use Project File <proj>

      Relocate_Build_Tree,
      --  --relocate-build-tree[=dir]
      ---     Root obj/lib/exec dirs are current-directory or dir

      Root_Dir,
      --  --root-dir=dir Root directory of obj/lib/exec to relocate

      RTS,
      --  --RTS=<runtime> Use runtime <runtime> for language Ada
      --  --RTS:<lang>=<runtime> Use runtime <runtime> for language <lang>

      Src_Subdirs,
      --  --src-subdirs=dir
      --      Prepend <obj>/dir to the list of source dirs for each project

      Subdirs,
      --  --subdirs=dir Use dir as suffix to obj/lib/exec directories

      Target,
      --  --target=targetname Specify a target for cross platforms

      Unchecked_Shared_Lib_Imports,
      --  --unchecked-shared-lib-imports
      --      Shared lib projects may import any project

      X
      --  -Xnm=val or -X nm=val Specify an external reference for Project Files

     );

   function Is_Finalized (Self : Object) return Boolean;
   --  Returns True if 'Self' object already successfully finalized.

   procedure Add_Switch
     (Self   : in out Object;
      Switch : Option;
      Param  : String := "";
      Index  : String := "")
     with Pre => not Self.Is_Finalized;
   --  Add a switch to options.
   --  Switches format is -[-]<Switch>[ ][:<Index>][=]<Param>
   --  Usage_Error exception is raised if options set in invalid.

   function On_Extra_Arg (Self : in out Object; Arg : String) return Boolean
     with Pre => not Self.Is_Finalized;
   --  Use this function if gpr file can be provided directly.
   --  If Arg is not a valid file name with a gpr extension False is returned.
   --  If Arg is a valid file name and Project_File was never set, it is set
   --    and True is returned, else Usage_Error is raised.

   procedure Finalize
     (Self                   : in out Object;
      Allow_Implicit_Project : Boolean := True;
      Quiet                  : Boolean := False;
      Environment            : GPR2.Environment.Object :=
                                 GPR2.Environment.Process_Environment)
     with Pre => not Self.Is_Finalized,
          Post => Self.Is_Finalized;
   --  Option set should be finalized before Load_Project can be called.
   --  Allow_Implicit_Project is required to load default project.
   --  Quiet is useful to prevent output/error stream usage.
   --  Usage_Error exception is raised if options set in invalid.

   procedure Register_Project_Search_Paths
     (Self : Object;
      Tree : in out GPR2.Project.Tree.Object);
   --  Add directories to project search path
   --  Note: This procedure is called automatically if during Load_Project call
   --        Tree parameter is undefined.

   function Load_Project
     (Self             : in out Object;
      Tree             : in out GPR2.Project.Tree.Object;
      With_Runtime     : Boolean;
      Absent_Dir_Error : GPR2.Project.Tree.Error_Level :=
                           GPR2.Project.Tree.Warning;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference;
      Quiet            : Boolean := False) return Boolean
     with Pre => Self.Is_Finalized;
   --  Load a project tree using configured options.
   --  If successful, Tree contains loaded project tree.
   --  If Tree is undefined on entry, project search paths are automatically
   --   registered.
   --  Load messages are appended to Log.
   --  Absent_Dir_Error: whether a missing directory should be treated as an
   --   error or a warning.
   --  If Quiet is true no output is printed.

   -----------------------------------------------------
   --  Object's getters. (Requires finalized options) --
   -----------------------------------------------------

   function Project_File
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Finalized;
   --  Returns the project file found in the arguments or
   --  implicit project file when allowed. It can be undefined.

   function Filename
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Finalized;
   --  Returns Filename argument used loading the project
   --  Self.Project_File if defined otherwise the current directory.

   function Context
     (Self : Object) return GPR2.Context.Object
     with Pre => Self.Is_Finalized;
   --  Returns Context argument used loading the project

   function Config_Project
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Finalized;
   --  Returns path name object used as configuration file used loading
   --  configuration project or as Config_Project argument used to save
   --  configuration file generated by load project in auto-configuration

   function Build_Path
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Finalized;
   --  Returns Build_Path argument used loading the project.

   function Subdirs
     (Self : Object) return Ada.Strings.Unbounded.Unbounded_String
     with Pre => Self.Is_Finalized;
   --  Returns Subdirs argument used loading the project.

   function Subdirs
     (Self : Object) return GPR2.Optional_Name_Type
     with Pre => Self.Is_Finalized;
   --  Returns Subdirs argument used loading the project.

   function Src_Subdirs
     (Self : Object) return Ada.Strings.Unbounded.Unbounded_String
     with Pre => Self.Is_Finalized;
   --  Returns Src_Subdirs argument used loading the project.

   function Src_Subdirs
     (Self : Object) return GPR2.Optional_Name_Type
     with Pre => Self.Is_Finalized;
   --  Returns Src_Subdirs argument used loading the project.

   function Check_Shared_Lib
     (Self : Object) return Boolean
     with Pre => Self.Is_Finalized;
   --  Returns Check_Shared_Lib argument used loading the project.

   function Implicit_With (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre => Self.Is_Finalized;
   --  Returns Implicit_With argument used loading the project

   function Target (Self : Object) return GPR2.Name_Type
     with Pre => Self.Is_Finalized;
   --  Returns Target argument loading the project.

   function RTS_Map (Self : Object) return GPR2.Containers.Lang_Value_Map
     with Pre => Self.Is_Finalized;
   --  Returns Language_Runtimes argument loading the project.

   function Base (Self : Object) return GPR2.KB.Object
     with Pre => Self.Is_Finalized;
   --  Returns knowledge base object used as Base argument loading the project.

   function Config_Project_Log (Self : Object) return GPR2.Log.Object
     with Pre => Self.Is_Finalized;
   --  Returns log object filled during Config_Project Load done loading the
   --  project.

   function Config_Project_Has_Error
     (Self : Object) return Boolean
     with Pre => Self.Is_Finalized;
   --  Returns True if configuration cannot be loaded successfully.

   function Project_Is_Defined (Self : Object) return Boolean
     with Pre => Self.Is_Finalized;
   --  Returns True if project defined directly, not using implicit project.

   function Check_For_Default_Project
     (Directory : String := "") return GPR2.Path_Name.Object;
   --  returns default gpr file or else the only gpr file found in directory.

private

   type Object is tagged record

      --  Object state

      Finalized                : Boolean := False;

      --  Project file and context:

      Context                  : GPR2.Context.Object;
      Project_File             : GPR2.Path_Name.Object;
      Project_Is_Defined       : Boolean := False;

      Prj_Got_On_Extra_Arg     : Boolean := False;
      --  True if project defined not using -P option

      No_Project               : Boolean := False;
      Project_Base             : GPR2.Path_Name.Object;
      --  If defined, then process Project_File like it is located in the
      --  Project_Base.

      --  Project tree modifiers

      Root_Path                : GPR2.Path_Name.Object;
      Build_Path               : GPR2.Path_Name.Object;
      Src_Subdirs              : Ada.Strings.Unbounded.Unbounded_String;
      Subdirs                  : Ada.Strings.Unbounded.Unbounded_String;
      Implicit_With            : GPR2.Path_Name.Set.Object;
      Unchecked_Shared_Lib     : Boolean := False;

      --  Conf/Autoconf

      Config_Project           : GPR2.Path_Name.Object;
      Create_Missing_Config    : Boolean := False;
      Target                   : Ada.Strings.Unbounded.Unbounded_String :=
                                   Ada.Strings.Unbounded.To_Unbounded_String
                                     ("all");
      RTS_Map                  : GPR2.Containers.Lang_Value_Map;
      Skip_Default_KB          : aliased Boolean := False;
      KB_Locations             : GPR2.Path_Name.Set.Object;

      Search_Paths             : GPR2.Path_Name.Set.Object;

      Config_Project_Has_Error : Boolean := False;
      --  configuration file that cannot be loaded.

      Config_Project_Log       : GPR2.Log.Object;

      Environment              : GPR2.Environment.Object;

   end record;

   function Is_Finalized (Self : Object) return Boolean
   is (Self.Finalized);

   function Base (Self : Object) return GPR2.KB.Object
   is (GPR2.KB.Create
       (Flags       => GPR2.KB.Default_Flags,
        Default_KB  => not Self.Skip_Default_KB,
        Custom_KB   => Self.KB_Locations,
        Environment => Self.Environment));

   function Build_Path
     (Self : Object) return GPR2.Path_Name.Object
   is (Self.Build_Path);

   function Check_Shared_Lib
     (Self : Object) return Boolean
   is (not Self.Unchecked_Shared_Lib);

   function Config_Project
     (Self : Object) return GPR2.Path_Name.Object
   is (Self.Config_Project);

   function Config_Project_Log (Self : Object) return GPR2.Log.Object
   is (Self.Config_Project_Log);

   function Context
     (Self : Object) return GPR2.Context.Object
   is (Self.Context);

   function Filename
     (Self : Object) return GPR2.Path_Name.Object
   is (if Self.Project_File.Is_Defined
       then Self.Project_File
       else Self.Project_Base);

   function Config_Project_Has_Error
     (Self : Object) return Boolean
   is (Self.Config_Project_Has_Error);

   function Implicit_With (Self : Object) return GPR2.Path_Name.Set.Object
   is (Self.Implicit_With);

   function Project_File
     (Self : Object) return GPR2.Path_Name.Object
   is (Self.Project_File);

   function Project_Is_Defined (Self : Object) return Boolean
   is (Self.Project_Is_Defined);

   function RTS_Map (Self : Object) return GPR2.Containers.Lang_Value_Map
   is (Self.RTS_Map);

   function Src_Subdirs
     (Self : Object) return Ada.Strings.Unbounded.Unbounded_String
   is (Self.Src_Subdirs);

   function Src_Subdirs
     (Self : Object) return Optional_Name_Type
   is (GPR2.Optional_Name_Type (To_String (Self.Src_Subdirs)));

   function Subdirs
     (Self : Object) return Ada.Strings.Unbounded.Unbounded_String
   is (Self.Subdirs);

   function Subdirs
     (Self : Object) return GPR2.Optional_Name_Type
   is (GPR2.Optional_Name_Type (To_String (Self.Subdirs)));

   function Target
     (Self : Object) return GPR2.Name_Type
   is (GPR2.Name_Type (To_String (Self.Target)));

   Empty_Options : constant Object := (others => <>);

end GPR2.Options;
