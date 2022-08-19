--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Iterator_Interfaces;

with GPR2.Containers;
with GPR2.Context;
with GPR2.File_Readers;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Configuration;
pragma Elaborate (GPR2.Project.Configuration);
--  Elaborate to avoid a circular dependency due to default Elaborate_Body

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Source;
with GPR2.Project.Unit_Info;
with GPR2.Project.View.Set;
with GPR2.Project.View.Vector;
with GPR2.Source_Info;
with GPR2.View_Ids;
with GPR2.View_Ids.DAGs;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

private with GPR2.Project.Definition;
private with GPR2.Unit;

package GPR2.Project.Tree is

   use GPR2.Context;

   use type GPR2.Project.View.Object;

   type Object is tagged limited private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => View.Object;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Is_Windows_Target (Tree : Object) return Boolean
     with Pre => Tree.Is_Defined;
   --  Returns true if tree's target is window

   function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are the same tree

   procedure Load
     (Self             : in out Object;
      Filename         : Path_Name.Object;
      Context          : GPR2.Context.Object;
      Config           : Configuration.Object      := Configuration.Undefined;
      Build_Path       : Path_Name.Object          := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type        := No_Name;
      Src_Subdirs      : Optional_Name_Type        := No_Name;
      Check_Shared_Lib : Boolean                   := True;
      Absent_Dir_Error : Boolean                   := False;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Pre_Conf_Mode    : Boolean                   := False;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference)
     with Pre => Filename.Is_Defined;
   --  Loads a root project
   --  Filename: if Filename is a file path, then Load_Autoconf will use it as
   --   root project. If Filename is a directory path, then implicit projects
   --   are searched there. If not such implicit project is found, then the
   --   tree is loaded with an empty project.
   --  Context: list of values to use to fill externals.
   --  Config: the configuration to use to load the tree.
   --  Build_Path: if defined, indicate the directory to use to build the
   --   project tree (out of tree build).
   --  Subdirs: if specified, this value is used as subdirectory for
   --   lib/obj/exec directories
   --  Src_Subdirs: if specified, prepend obj/<project namne>_<value> or
   --   obj/<value> to the list of source directories, if they exist.
   --  Check_Shared_Lib: checks for shared library compatibilities
   --  Absent_Dir_Error: whether a missing directory should be treated as an
   --   error or a warning.
   --  Implicit_With: a list of implicitly withed projects.
   --  Pre_Conf_Mode: set in autoconf mode to disable most errors when trying
   --   to load a tree without configuration.
   --  File_Reader: if set, then it is used when parsing Ada sources or
   --  GPR projects. Else default file reader is used.

   procedure Load_Configuration
     (Self     : in out Object;
      Filename : Path_Name.Object)
     with Pre => Filename.Is_Defined;
   --  Loads a configuration project for this tree

   procedure Load_Autoconf
     (Self              : in out Object;
      Filename          : Path_Name.Object;
      Context           : GPR2.Context.Object;
      Build_Path        : Path_Name.Object        := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type      := No_Name;
      Src_Subdirs       : Optional_Name_Type      := No_Name;
      Check_Shared_Lib  : Boolean                 := True;
      Absent_Dir_Error  : Boolean                 := False;
      Implicit_With     : GPR2.Path_Name.Set.Object :=
                            GPR2.Path_Name.Set.Empty_Set;
      Target            : Optional_Name_Type      := No_Name;
      Language_Runtimes : Containers.Lang_Value_Map :=
                            Containers.Lang_Value_Maps.Empty_Map;
      Base              : GPR2.KB.Object          := GPR2.KB.Undefined;
      Config_Project    : GPR2.Path_Name.Object   := GPR2.Path_Name.Undefined;
      File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                            GPR2.File_Readers.No_File_Reader_Reference)
       with Pre => Filename.Is_Defined;
   --  Loads a tree in autoconf mode.
   --  If Filename is a file path, then Load_Autoconf will use it as
   --  root project. If Filename is a directory path, then implicit projects
   --  are searched there. If not such implicit project is found, then the
   --  tree is loaded with an empty project.
   --  If Target is specified, then we use it directly instead of fetching
   --  the root project attribute.
   --  Same with the Language_Runtime map: for each language Lang in the
   --  project tree, if the map has an association (Lang,RTS) then we use it
   --  instead of any attribute Runtime (Lang) declared in the root project.
   --  Typically this is useful to enforce precedence of the command-line
   --  options --target and --RTS[:lang].
   --  If Project_Dir is defined, the main project file being parsed is deemed
   --  to be in this directory, even if it is not the case. Project_Dir is
   --  defined when a gpr tool is invoked without a project file and is using
   --  an implicit project file that is virtually in the Project_Dir, but is
   --  physically in another directory.
   --  Base is the knowledge base object used to configure the toolchain for
   --  the project.
   --  If File_Reader is set, then it is used when parsing Ada sources or
   --  GPR projects. Else default file reader is used.

   procedure Unload (Self : in out Object;
                     Full : Boolean := True);
   --  Unloads the tree and free all associated objects (projects, sources,
   --  etc...).
   --  If Full is set, then the cached parsed projects objects will also be
   --  unloaded.

   function Root_Project (Self : Object) return View.Object
     with Pre  => Self.Is_Defined,
          Post => Root_Project'Result.Is_Defined;
   --  Returns the root project for the given tree

   function Has_Configuration (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a configuration project is loaded on this tree

   function Configuration (Self : Object) return Configuration.Object
     with Pre => Self.Is_Defined and then Self.Has_Configuration;
   --  Returns the configuration project for the given tree

   function Has_Runtime_Project (Self : Object) return Boolean;
   --  Returns True if a runtime project is loaded on this tree

   function Runtime_Project (Self : Object) return View.Object
     with Pre => Self.Is_Defined and then Self.Has_Runtime_Project;
   --  Returns the runtime project for the given tree

   function Target
     (Self      : Object;
      Canonical : Boolean := False) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the target for the project tree

   function Runtime
     (Self : Object; Language : Language_Id) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the runtime selected for the given language or the empty string
   --  if no specific runtime has been configured for this project tree.

   function Ordered_Views (Self : Object) return View.Vector.Object
     with Pre => Self.Is_Defined;

   function Has_Messages (Self : Object) return Boolean;
   --  Returns whether some messages are present for this project tree

   function Log_Messages (Self : Object) return not null access Log.Object
     with Post => not Self.Has_Messages
                  or else not Log_Messages'Result.Is_Empty;
   --  Returns the Logs, this contains information, warning and error messages
   --  found while handling the project.

   procedure Append_Message
     (Self    : in out Object;
      Message : GPR2.Message.Object)
     with Pre  => Self.Is_Defined,
          Post => Self.Log_Messages.Count in Self.Log_Messages.Count'Old
                                          .. Self.Log_Messages.Count'Old + 1;
   --  Adds new message into the Log of Self, does nothing if message already
   --  present.

   --  Context

   --  Note that the context of the project tree corresponds to the context of
   --  the root project view.

   function Has_Context (Self : Object) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True if the project tree has some context. If any of the project
   --  in the tree has some external variables then a context is present. A
   --  project without context is fully static has it does not reference any
   --  external (and so modifiable) variables.

   function Context (Self : Object) return Context.Object
     with Pre  => Self.Is_Defined,
          Post => Self.Has_Context = (Context'Result /= GPR2.Context.Empty);
   --  Returns the Context for the given project tree

   function Add_Tool_Prefix
     (Self      : Object;
      Tool_Name : Name_Type) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns Tool_Name for native compilation otherwise
   --  it returns the tool prefixed with the target-triplet,
   --  for example x86_64-linux-gnu-gcc.

   procedure Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null)
     with Pre => Self.Is_Defined;
   --  Sets the context for the project tree. The callback Changed is called
   --  for any project view which is impacted by this change of context, i.e.
   --  if the project view references directly or indirectly an external
   --  variable.

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return View.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Has_Element (Position : Cursor) return Boolean;

   package Project_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Is_Root (Position : Cursor) return Boolean;
   --  Returns True if the cursor is pointing to the root project

   type Constant_Reference_Type
     (View : not null access constant Project.View.Object) is private
     with Implicit_Dereference => View;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
     with Pre => Self.Is_Defined and then Position /= No_Element;

   function Iterate
     (Self   : Object;
      Kind   : Iterator_Control := Default_Iterator;
      Filter : Filter_Control   := Default_Filter;
      Status : Status_Control   := Default_Status)
      return Project_Iterator.Forward_Iterator'Class
     with Pre => Self.Is_Defined;
   --  Iterates over all project views in the tree given the iterator kind
   --  (only the project with or without imports) and the filter which can be
   --  used to iterate over only some specific projects (only the library
   --  projects for example).

   --  Unit/View

   procedure Record_View
     (Self   : in out Object;
      View   : GPR2.Project.View.Object;
      Source : Path_Name.Object;
      Unit   : Name_Type)
     with Pre => Self.Is_Defined;
   --  Records the view in which unit is defined

   procedure Clear_View
     (Self : in out Object;
      Unit : Unit_Info.Object)
     with Pre => Self.Is_Defined;
   --  Clears the view set for the given unit

   function Get_View
     (Self : Object;
      Unit : Name_Type) return Project.View.Object
     with Pre => Self.Is_Defined;
   --  Gets the view in which unit is defined, returns Undefined if the unit
   --  has not been found.

   function Get_View
     (Self   : Object;
      Source : Path_Name.Object) return Project.View.Object
     with Pre => Self.Is_Defined;
   --  Gets the view in which source file is defined, returns Undefined if the
   --  source file has not been found.
   --  If Update is True and view with this source is not found, than Update
   --  sources in all views and try to find again.

   procedure Invalidate_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined)
     with Pre => Self.Is_Defined;
   --  Invalidates the sources for all views in the tree if View is undefined
   --  or the source in the given view otherwise. This is needed when some
   --  sources are added or removed from the view. It is not required to call
   --  Update_Sources below, when the routine Sources on one of the views of
   --  the tree will be called, the set of sources will be recomputed.

   procedure Update_Sources
     (Self          : Object;
      Stop_On_Error : Boolean := True;
      With_Runtime  : Boolean := False;
      Backends      : Source_Info.Backend_Set := Source_Info.All_Backends)
     with Pre => Self.Is_Defined;
   --  Ensures that all views' sources are up-to-date. This is needed before
   --  computing the dependencies of a source in the project tree. This routine
   --  is called where needed and is there for internal use only.
   --  If Stop_On_Error is True then exception Project_Error is raised in case
   --  an error message exists in log after processing one of project files in
   --  tree. If Stop_On_Error is False then no exception is raised and errors
   --  can be discovered from the Log.Object taken from Log_Messages call.
   --  Backends parameter defines the set of parsers that can be used to parse
   --  the source information.

   procedure Register_Project_Search_Path
     (Self : in out Object;
      Dir  : Path_Name.Object)
     with Pre => Dir.Is_Defined;
   --  Adds a project search path for this tree

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns the Tree project search paths

   function Archive_Suffix (Self : Object) return Filename_Type
     with Pre => Self.Is_Defined;
   --  Returns archive suffix for the project tree

   function Object_Suffix
     (Self     : Object;
      Language : Language_Id := Ada_Language) return Filename_Type
     with Pre => Self.Is_Defined;
   --  Returns object suffix for language in project tree

   function Dependency_Suffix
     (Self     : Object;
      Language : Language_Id := Ada_Language) return Filename_Type
     with Pre => Self.Is_Defined;
   --  Returns dependency suffix for language in project tree

   function Subdirs (Self : Object) return Filename_Optional
     with Pre => Self.Is_Defined;
   --  Returns the subdirs parameter <sub> of the project tree such that, for
   --  each project, the actual {executable,object,library} directories are
   --  {<exec>,<obj>,<lib>}/<sub>.

   function Has_Src_Subdirs (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the Src_Subdirs has been defined

   function Src_Subdirs (Self : Object) return Filename_Optional
     with Pre => Self.Is_Defined and then Self.Has_Src_Subdirs;
   --  Returns the src_subdirs parameter <sub> of the project tree such that,
   --  for each project, the actual source directories list will be prepended
   --  with {object_dir}/<sub>.

   function Build_Path (Self : Object) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Path to build tree

   function Reference (Self : Object) return access Object;
   --  Returns access to itself

   function Get_File
     (Self             : Object;
      Base_Name        : Simple_Name;
      View             : Project.View.Object := Project.View.Undefined;
      Use_Source_Path  : Boolean := True;
      Use_Object_Path  : Boolean := True;
      Predefined_Only  : Boolean := False;
      Return_Ambiguous : Boolean := True) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Return absolute path of source/object/project file found in Self or in
   --  View when defined.
   --
   --  If a source file matches Base_Name and Use_Source_Path is true, it
   --  is always returned.
   --  Set Predefined_Only to True to disable looking in the project sources
   --  and only look in the predefined source files.
   --
   --  Otherwise, the file will be searched for in the source dirs and/or
   --  object dirs of either a specific Project or in the whole project tree.
   --  As a special case, if Base_Name ends with '.gpr', it is also looked
   --  for among the already loaded project, even if their directory is outside
   --  the source dirs and object dirs.
   --
   --  If no such file is found, Undefined is returned.
   --
   --  The matching from base source names to full path names is potentially
   --  ambiguous when using aggregate projects, because it is valid to have
   --  multiple files with the same base name within a given project tree.
   --  In such an ambiguous case, this function will return Undefined.
   --  To lift this ambiguity, and if you know which project the file is found
   --  in, you must pass a View argument. The file must be a direct source
   --  of that project.
   --
   --  If a given full path is part of the sources for several projects, this
   --  is considered as ambiguous, because the associated object file,
   --  for instance, is different. In this case the returned value is
   --  set to the common source file if Return_Ambiguous is set to True
   --  otherwise Undefined is returned.

   Target_Name : constant Name_Type;
   --  Native host target

   ------------------------
   -- Internal functions --
   ------------------------

   function Instance_Of
      (Self        : Object;
       Instance_Id : GPR2.View_Ids.View_Id) return View.Object
     with Pre => Self.Is_Defined
                   and then GPR2.View_Ids.Is_Defined (Instance_Id);
   --  Given a view id return the effective view that should be used. The
   --  function is mainly used to get the effective view in case a project has
   --  been extended using extends all.

   procedure Reindex_Unit (Self : in out Object; From, To : Name_Type);
   --  Change name of unit in view index used to get view by unit name

   function Target_From_Command_Line
     (Self       : Object;
      Normalized : Boolean := False) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the target specified via --target. If not specified, then
   --  return "all";

   function Runtime_From_Command_Line
     (Self : Object; Language : Language_Id) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the runtime selected for the given language by the command
   --  line via --RTS:lang. Returns No_Name if not specified.

   function Get_KB (Self : Object) return GPR2.KB.Object
     with Pre => Self.Is_Defined;

   function File_Reader
     (Self : Object) return GPR2.File_Readers.File_Reader_Reference
     with Pre => Self.Is_Defined;

   function Source_Directories
     (Self             : Object;
      View             : Project.View.Object := Project.View.Undefined;
      Externally_Built : Boolean := False) return GPR2.Path_Name.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns the source directories for a given View subtree
   --  If View is undefined use root project.
   --  If Externally_Built is False then source directories belonging
   --  to project marked "Externally_Built" will not be returned.

   procedure For_Each_Source
     (Self             : Object;
      View             : Project.View.Object := Project.View.Undefined;
      Action           : access procedure (Source : Project.Source.Object);
      Language         : Language_Id := No_Language;
      Externally_Built : Boolean := False)
     with Pre => Self.Is_Defined;
   --  Call Action for all the sources for a given View subtree.
   --  If View is undefined use root project.
   --  If Language is No_Language then all sources are returned.
   --  If Externally_Built is False then source directories belonging
   --  to project marked "Externally_Built" will not be returned.
   --  Note that this routine ensure that the sources are loaded.

private

   package PC renames Project.Configuration;
   package PRA renames GPR2.Project.Registry.Attribute;

   package Name_View is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, View.Object);
   --  Map to find in which view a unit/source is defined

   package Filename_View is
     new Ada.Containers.Indefinite_Ordered_Maps (Filename_Type, View.Object);
   --  Map to find in which view a unit/source is defined

   function Key
     (View : Project.View.Object; Source : Simple_Name) return String
   is
     (Path_Name.To_OS_Case (View.Namespace_Root.Path_Name.Value)
      & '|' & Path_Name.To_OS_Case (String (Source)));

   function Key
     (View : Project.View.Object; Unit : GPR2.Unit.Object) return String
   is
     (Path_Name.To_OS_Case (View.Namespace_Root.Path_Name.Value)
      & (if Unit.Kind in GPR2.Unit.Spec_Kind then 'S' else 'B')
      & To_Lower (Unit.Name));

   function Key (Item : Source.Object) return String is
     (Key (Item.View, Item.Path_Name.Simple_Name));

   function To_Hash (Item : Source.Object) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (Key (Item)));

   function Same_Key (Left, Right : Source.Object) return Boolean is
     (Key (Left) = Key (Right));

   package Id_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (GPR2.View_Ids.View_Id, View.Object,
      Hash            => GPR2.View_Ids.Hash,
      Equivalent_Keys => GPR2.View_Ids."=");
   --  Maps View_Ids to View objects

   package Source_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Source.Object,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Source."=");

   type Two_Contexts is array (Context_Kind) of GPR2.Context.Object;
   --  Root and Aggregate contexts

   type Project_Descriptor_Kind is (Project_Path, Project_Definition);

   type Project_Descriptor (Kind : Project_Descriptor_Kind) is record
      case Kind is
         when Project_Path =>
            Path : GPR2.Path_Name.Object;
         when Project_Definition =>
            Data : GPR2.Project.Definition.Data;
      end case;
   end record;

   type Object is tagged limited record
      Self              : access Object := null;
      Root              : View.Object;
      Conf              : Project.Configuration.Object;
      Base              : GPR2.KB.Object;
      Runtime           : View.Object;
      Units             : Name_View.Map;
      Sources           : Filename_View.Map;
      Messages          : aliased Log.Object;
      Search_Paths      : Path_Name.Set.Object :=
                            Default_Search_Paths (True);
      Implicit_With     : Path_Name.Set.Object;
      Build_Path        : Path_Name.Object;
      Subdirs           : Unbounded_String;
      Src_Subdirs       : Unbounded_String;
      Check_Shared_Lib  : Boolean := True;
      Absent_Dir_Error  : Boolean := False;
      Pre_Conf_Mode     : Boolean := True;
      Views_Set         : View.Set.Object;
      --  All projects in registration order
      Context           : Two_Contexts;
      --  Root and aggregate contexts
      View_Ids          : aliased Id_Maps.Map;
      View_DAG          : GPR2.View_Ids.DAGs.DAG;
      Sources_Loaded    : Boolean := False;
      --  Configuration items from command line
      Explicit_Target   : Unbounded_String;
      Explicit_Runtimes : Containers.Lang_Value_Map;
      File_Reader_Ref   : GPR2.File_Readers.File_Reader_Reference;
   end record;

   procedure Load
     (Self             : in out Object;
      Root_Project     : Project_Descriptor;
      Context          : GPR2.Context.Object;
      Config           : PC.Object                 := PC.Undefined;
      Build_Path       : Path_Name.Object          := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type        := No_Name;
      Src_Subdirs      : Optional_Name_Type        := No_Name;
      Check_Shared_Lib : Boolean                   := True;
      Absent_Dir_Error : Boolean                   := False;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Pre_Conf_Mode    : Boolean                   := False;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference);
   --  Common implementation for loading a project either from an actual
   --  file or from a manually built root project data.

   procedure Load_Autoconf
     (Self              : in out Object;
      Root_Project      : Project_Descriptor;
      Context           : GPR2.Context.Object;
      Build_Path        : Path_Name.Object        := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type      := No_Name;
      Src_Subdirs       : Optional_Name_Type      := No_Name;
      Check_Shared_Lib  : Boolean                 := True;
      Absent_Dir_Error  : Boolean                 := False;
      Implicit_With     : GPR2.Path_Name.Set.Object :=
                            GPR2.Path_Name.Set.Empty_Set;
      Target            : Optional_Name_Type      := No_Name;
      Language_Runtimes : Containers.Lang_Value_Map :=
                            Containers.Lang_Value_Maps.Empty_Map;
      Base              : GPR2.KB.Object          := GPR2.KB.Undefined;
      Config_Project    : GPR2.Path_Name.Object   := GPR2.Path_Name.Undefined;
      File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                            GPR2.File_Readers.No_File_Reader_Reference);

   function "=" (Left, Right : Object) return Boolean
   is (Left.Self = Right.Self);

   package Project_View_Store renames GPR2.Project.View.Vector.Vector;

   type Cursor is record
      Current : Project_View_Store.Cursor;
      Tree    : access Object := null;
   end record;

   No_Element : constant Cursor :=
                  (Current => Project_View_Store.No_Element,
                   Tree    => null);

   type Constant_Reference_Type
     (View : not null access constant Project.View.Object) is record
      --  We need to keep the underlying reference so that it is not cleared
      --  upon return of the getter, and so that the container has the proper
      --  busy state
      Ref : Project.View.Set.Set.Constant_Reference_Type (View);
   end record;

   Undefined : constant Object := (others => <>);

   Target_Name : constant Name_Type := KB.Default_Target;

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Windows_Target (Tree : Object) return Boolean is
     (Tree.Has_Configuration
        and then
      Tree.Configuration.Corresponding_View.Has_Attribute
        (PRA.Shared_Library_Suffix)
        and then
      Tree.Configuration.Corresponding_View.Attribute
        (PRA.Shared_Library_Suffix).Value_Equal (".dll"));
      --  ??? We may also check that the Tree target name constains mingw or
      --  windows.

   function Archive_Suffix (Self : Object) return Filename_Type is
     (if Self.Has_Configuration
      then Self.Configuration.Archive_Suffix
      else ".a");

   function Object_Suffix
     (Self     : Object;
      Language : Language_Id := Ada_Language) return Filename_Type
   is (if Self.Has_Configuration
       then Self.Configuration.Object_File_Suffix (Language)
       else ".o");

   function Dependency_Suffix
     (Self     : Object;
      Language : Language_Id := Ada_Language) return Filename_Type
   is
     (if Self.Has_Configuration
      then Self.Configuration.Dependency_File_Suffix (Language)
      elsif Language = Ada_Language then ".ali" else ".d");

   function Reference (Self : Object) return access Object is
     (Self'Unrestricted_Access);

   function Subdirs (Self : Object) return Filename_Optional is
     (Filename_Optional (To_String (Self.Subdirs)));

   function Src_Subdirs (Self : Object) return Filename_Optional is
     (Filename_Optional (To_String (Self.Src_Subdirs)));

   function Build_Path (Self : Object) return Path_Name.Object is
     (Self.Build_Path);

   function Runtime_From_Command_Line
     (Self : Object; Language : Language_Id) return Optional_Name_Type is
     (if Self.Explicit_Runtimes.Contains (Language)
      then Optional_Name_Type (Self.Explicit_Runtimes.Element (Language))
      else No_Name);

   function Get_KB (Self : Object) return GPR2.KB.Object is
     (Self.Base);

   function File_Reader
     (Self : Object)
      return GPR2.File_Readers.File_Reader_Reference is
     (Self.File_Reader_Ref);

end GPR2.Project.Tree;
