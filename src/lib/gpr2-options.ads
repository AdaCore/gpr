--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  This package is provided to simplify and normalize GPR common switches
--  support.

--  Once options object is configured (by parsing the command line or calling
--  On_Switch/Finalize functions directly), it can be used to load a Tree.

with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Environment;
with GPR2.KB;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Registry.Exchange;

package GPR2.Options is

   use Ada.Strings.Unbounded;

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

      Resolve_Links,
      --  -eL, follows symlinks for project files

      No_Project,
      --  --no-project Do not use project file

      P,
      --  -Pproj<.gpr> or -P proj<.gpr> Use Project File <proj>

      Print_GPR_Registry,
      --  --print-gpr-registry
      --      Print to console package/attribute added by tool in JSON & exit

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

      X
      --  -Xnm=val or -X nm=val Specify an external reference for Project Files

     );

   procedure Add_Switch
     (Self     : in out Object;
      Switch   : Option;
      Param    : String := "";
      Index    : String := "";
      Override : Boolean := False);
   --  Add a switch to options.
   --  Switches format is -[-]<Switch>[ ][:<Index>][=]<Param>
   --  Usage_Error exception is raised if options set in invalid.
   --
   --  If you need to redefine an already defined switch that is supposed
   --  to not appear several times on the command line, then you need to
   --  set the "Override" parameter.

   procedure Add_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object);
   --  Adds the context to the options. Equivalent to adding the -X switch
   --  for each of the context items.

   function On_Extra_Arg (Self : in out Object; Arg : String) return Boolean;
   --  Use this function if gpr file can be provided directly.
   --  If Arg is not a valid file name with a gpr extension False is returned.
   --  If Arg is a valid file name and Project_File was never set, it is set
   --    and True is returned, else Usage_Error is raised.

   -----------------------------------------------------
   --  Object's getters. (Requires finalized options) --
   -----------------------------------------------------

   function Project_File (Self : Object) return GPR2.Path_Name.Object;
   --  Returns the project file found in the arguments

   function No_Project (Self : Object) return Boolean;
   --  Whether --no-project has been specified

   function Base
     (Self        : Object;
      Environment : GPR2.Environment.Object;
      Flags       : GPR2.KB.Parsing_Flags := GPR2.KB.Default_Flags)
      return GPR2.KB.Object;
   --  Loads the KB according to --db and --db- switches and returns it.

   function Resolve_Links (Self : Object) return Boolean;
   --  Whether the symlinks resolution is activated (option -eL)

   function Config_Project (Self : Object) return GPR2.Path_Name.Object;
   --  Returns path name object used as configuration file used loading
   --  configuration project or as Config_Project argument used to save
   --  configuration file generated by load project in auto-configuration

   function Create_Config_Project (Self : Object) return Boolean;
   --  If config project is defined, this tells whether the project file is
   --  created in case it doesn't exist.

   function Build_Path (Self : Object) return GPR2.Path_Name.Object;
   --  Returns Build_Path argument used loading the project

   function Root_Path (Self : Object) return GPR2.Path_Name.Object;
   --  Returns Root_Path argument used for loading the project

   function Subdirs (Self : Object) return Unbounded_String;
   --  Returns Subdirs argument used loading the project

   function Subdirs
     (Self : Object) return GPR2.Optional_Name_Type;
   --  Returns Subdirs argument used loading the project

   function Src_Subdirs (Self : Object) return GPR2.Optional_Name_Type;
   --  Returns Src_Subdirs argument used loading the project

   function Context (Self : Object) return GPR2.Context.Object;
   --  Load context for the project as specified by the user via the -X option

   function Implicit_With (Self : Object) return GPR2.Path_Name.Set.Object;
   --  Returns Implicit_With argument used loading the project

   function Target (Self : Object) return GPR2.Name_Type;
   --  Returns Target argument loading the project

   function RTS_Map (Self : Object) return GPR2.Containers.Lang_Value_Map;
   --  Returns Language_Runtimes argument loading the project

   function User_Specified_Project_Search_Path
     (Self : Object) return GPR2.Path_Name.Set.Object;

   procedure Print_GPR_Registry
     (Self   : Object;
      Format : GPR2.Project.Registry.Exchange.Export_Format :=
                  GPR2.Project.Registry.Exchange.K_JSON_COMPACT);
   --  If '--print-gpr-registry' switch in command line print
   --  package/attributes added by current tool and exit.

   Print_GPR_Registry_Option : constant String := "--print-gpr-registry";
   --  Option to be used to print registered package/attribute & exit

private

   type Object is tagged record
      --  Project file and context:

      Project_File             : GPR2.Path_Name.Object;
      Resolve_Links            : Boolean := False;

      Prj_Got_On_Extra_Arg     : Boolean := False;
      --  True if project defined not using -P option

      No_Project               : Boolean := False;
      --  If defined, then process Project_File like it is located in the
      --  Project_Base.

      --  Project tree modifiers

      Root_Path             : GPR2.Path_Name.Object;
      Build_Path            : GPR2.Path_Name.Object;
      Src_Subdirs           : Ada.Strings.Unbounded.Unbounded_String;
      Subdirs               : Ada.Strings.Unbounded.Unbounded_String;
      Implicit_With         : GPR2.Path_Name.Set.Object;

      Context               : GPR2.Context.Object;

      --  Conf/Autoconf

      Config_Project        : GPR2.Path_Name.Object;
      Create_Missing_Config : Boolean := False;
      Target                : Ada.Strings.Unbounded.Unbounded_String :=
                                Ada.Strings.Unbounded.To_Unbounded_String
                                  ("all");
      RTS_Map               : GPR2.Containers.Lang_Value_Map;
      Skip_Default_KB       : aliased Boolean := False;
      KB_Locations          : GPR2.Path_Name.Set.Object;

      --  Misc

      Print_GPR_Registry    : Boolean := False;

      Search_Paths          : GPR2.Path_Name.Set.Object;
   end record;

   function Base
     (Self        : Object;
      Environment : GPR2.Environment.Object;
      Flags       : GPR2.KB.Parsing_Flags := GPR2.KB.Default_Flags)
      return GPR2.KB.Object
   is (GPR2.KB.Create
        (Flags       => Flags,
         Default_KB  => not Self.Skip_Default_KB,
         Custom_KB   => Self.KB_Locations,
         Environment => Environment));

   function Build_Path (Self : Object) return GPR2.Path_Name.Object is
     (Self.Build_Path);

   function Root_Path (Self : Object) return GPR2.Path_Name.Object is
     (Self.Root_Path);

   function Config_Project (Self : Object) return GPR2.Path_Name.Object is
     (Self.Config_Project);

   function Create_Config_Project (Self : Object) return Boolean is
     (Self.Create_Missing_Config);

   function Project_File (Self : Object) return GPR2.Path_Name.Object is
     (Self.Project_File);

   function No_Project (Self : Object) return Boolean is
     (Self.No_Project);

   function Implicit_With (Self : Object) return GPR2.Path_Name.Set.Object is
     (Self.Implicit_With);

   function RTS_Map (Self : Object) return GPR2.Containers.Lang_Value_Map is
     (Self.RTS_Map);

   function Src_Subdirs (Self : Object) return Optional_Name_Type is
     (GPR2.Optional_Name_Type (To_String (Self.Src_Subdirs)));

   function Subdirs (Self : Object) return Unbounded_String is
     (Self.Subdirs);

   function Subdirs (Self : Object) return GPR2.Optional_Name_Type is
     (GPR2.Optional_Name_Type (To_String (Self.Subdirs)));

   function Context (Self : Object) return GPR2.Context.Object is
     (Self.Context);

   function Target (Self : Object) return GPR2.Name_Type is
     (GPR2.Name_Type (To_String (Self.Target)));

   function User_Specified_Project_Search_Path
     (Self : Object) return GPR2.Path_Name.Set.Object
   is (Self.Search_Paths);

   function Resolve_Links (Self : Object) return Boolean is
     (Self.Resolve_Links);

   Empty_Options : constant Object := (others => <>);

end GPR2.Options;
