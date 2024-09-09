--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Iterator_Interfaces;

with GPR2.Environment;
with GPR2.Containers;
with GPR2.Context;
with GPR2.File_Readers;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
pragma Elaborate (GPR2.Project.Configuration);
--  Elaborate to avoid a circular dependency due to default Elaborate_Body

with GPR2.Build.Tree_Db;
with GPR2.Build.View_Db;
with GPR2.Path_Name.Set;
with GPR2.Project.Registry.Attribute;
limited with GPR2.Project.Tree;
with GPR2.Project.View.Set;
with GPR2.Project.View.Vector;
with GPR2.Reporter;
with GPR2.Reporter.Console;
with GPR2.View_Ids;
with GPR2.View_Ids.DAGs;
with GPR2.View_Internal;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Holders;

private package GPR2.Tree_Internal is

   use GPR2.Context;
   use GPR2.Project;

   use type Ada.Containers.Count_Type;
   use type GPR2.Project.View.Object;

   type Object is tagged limited private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
     Iterator_Element  => View.Object;
   type Object_Access is access all Object;

   type Project_Descriptor_Kind is (Project_Path, Project_Definition);

   type Project_Descriptor (Kind : Project_Descriptor_Kind) is record
      case Kind is
         when Project_Path =>
            Path : GPR2.Path_Name.Object;
         when Project_Definition =>
            Data : GPR2.View_Internal.Data;
      end case;
   end record;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   Get : access function
           (Tree : GPR2.Project.Tree.Object) return Object_Access;

   Set : access function
           (Tree : Object_Access) return GPR2.Project.Tree.Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Is_Windows_Target (Tree : Object) return Boolean
     with Pre => Tree.Is_Defined;
   --  Returns true if tree's target is window

   function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are the same tree

   procedure Load
     (Self             : in out Object;
      Root_Project     : Project_Descriptor;
      Context          : GPR2.Context.Object;
      With_Runtime     : Boolean                   := False;
      Config           : Configuration.Object      := Configuration.Undefined;
      Build_Path       : Path_Name.Object          := Path_Name.Undefined;
      Root_Path        : Path_Name.Object          := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type        := No_Name;
      Src_Subdirs      : Optional_Name_Type        := No_Name;
      Check_Shared_Lib : Boolean                   := True;
      Absent_Dir_Error : Error_Level               := Warning;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Resolve_Links    : Boolean                   := False;
      Pre_Conf_Mode    : Boolean                   := False;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference;
      Environment      : GPR2.Environment.Object :=
                           GPR2.Environment.Process_Environment);
   --  Loads a root project
   --  Filename: if Filename is a file path, then Load_Autoconf will use it as
   --   root project. If Filename is a directory path, then implicit projects
   --   are searched there. If not such implicit project is found, then the
   --   tree is loaded with an empty project.
   --  Context: list of values to use to fill externals.
   --  With_Runtime: whether runtime sources are considered.
   --  Config: the configuration to use to load the tree.
   --  Build_Path: if defined, indicate the directory to use to build the
   --   project tree (out of tree build).
   --  Subdirs: if specified, this value is used as subdirectory for
   --   lib/obj/exec directories
   --  Src_Subdirs: if specified, prepend obj/<project name>_<value> or
   --   obj/<value> to the list of source directories, if they exist.
   --  Check_Shared_Lib: checks for shared library compatibilities
   --  Absent_Dir_Error: whether a missing directory should be treated as an
   --   error or a warning.(
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

   procedure Restrict_Autoconf_To_Languages
     (Self  : in out Object;
      Langs : Containers.Language_Set);
   --  Sets a list of languages that auto-configuration will be reduced to
   --  from the actual set of languages used in project tree. Empty set of
   --  languages means regular auto-configuration with no reductions.

   procedure Load_Autoconf
     (Self              : in out Object;
      Root_Project      : Project_Descriptor;
      Context           : GPR2.Context.Object;
      With_Runtime      : Boolean                 := False;
      Build_Path        : Path_Name.Object        := Path_Name.Undefined;
      Root_Path         : Path_Name.Object        := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type      := No_Name;
      Src_Subdirs       : Optional_Name_Type      := No_Name;
      Check_Shared_Lib  : Boolean                 := True;
      Absent_Dir_Error  : Error_Level             := Warning;
      Implicit_With     : GPR2.Path_Name.Set.Object :=
                            GPR2.Path_Name.Set.Empty_Set;
      Resolve_Links     : Boolean                 := False;
      Target            : Optional_Name_Type      := No_Name;
      Language_Runtimes : Containers.Lang_Value_Map :=
                            Containers.Lang_Value_Maps.Empty_Map;
      Base              : GPR2.KB.Object          := GPR2.KB.Undefined;
      Config_Project    : GPR2.Path_Name.Object   := GPR2.Path_Name.Undefined;
      File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                            GPR2.File_Readers.No_File_Reader_Reference;
      Environment       : GPR2.Environment.Object :=
                            GPR2.Environment.Process_Environment);
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

   procedure Unload
     (Self : in out Object;
      Full : Boolean := True);
   --  Unloads the tree and free all associated objects (projects, sources,
   --  etc...).
   --  If Full is set, then the cached parsed projects objects will also be
   --  unloaded.

   function Root_Project (Self : Object) return View.Object;
   --  Returns the root project for the given tree

   function Namespace_Root_Projects (Self : Object) return View.Set.Object;
   --  Returns the list of namespace root projects: either the root project
   --  for regular trees, or the root of the subtrees for an aggregate project.

   function Has_Configuration (Self : Object) return Boolean;
   --  Returns True if a configuration project is loaded on this tree

   function Configuration (Self : Object) return Configuration.Object;
   --  Returns the configuration project for the given tree

   function Has_Runtime_Project (Self : Object) return Boolean;
   --  Returns True if a runtime project is loaded on this tree

   function Runtime_Project (Self : Object) return View.Object;
   --  Returns the runtime project for the given tree

   function Target
     (Self      : Object;
      Canonical : Boolean := False) return Name_Type;
   --  Returns the target for the project tree

   function Runtime
     (Self : Object; Language : Language_Id) return Optional_Name_Type;
   --  Returns the runtime selected for the given language or the empty string
   --  if no specific runtime has been configured for this project tree.

   function With_Runtime (Self : Object) return Boolean;
   --  Whether creation of the runtime project was requested when loading the
   --  tree.

   function Artifacts_Dir (Self : Object) return Path_Name.Object;
   --  Tries to return a directory that can be used to store artifacts that
   --  are global to the tree.
   --  This returns the object directory of the root view if available, else
   --  it returns the root view's project directory.
   --  **Important note** project directories may not be writable, as only
   --  object dirs are required to have read/write access. So this function
   --  needs to be used with care.

   function Ordered_Views (Self : Object) return View.Vector.Object;

   function Log_Messages (Self : Object) return not null access Log.Object;
   --  Returns the Logs, this contains information, warning and error messages
   --  found while handling the project.

   procedure Append_Message
     (Self    : in out Object;
      Message : GPR2.Message.Object);
   --  Adds new message into the Log of Self, does nothing if message already
   --  present.

   procedure Set_Reporter
     (Self : in out Object; Reporter : GPR2.Reporter.Object'Class);
   --  Sets the reporter used by the tree and all tree-related operations,
   --  such as loading or working with sources, to output the logs.

   function Reporter (Self : Object) return GPR2.Reporter.Object'Class;
   --  Returns the tree reporter

   --  Context

   --  Note that the context of the project tree corresponds to the context of
   --  the root project view.

   function Has_Context (Self : Object) return Boolean;
   --  Returns True if the project tree has some context. If any of the project
   --  in the tree has some external variables then a context is present. A
   --  project without context is fully static has it does not reference any
   --  external (and so modifiable) variables.

   function Context (Self : Object) return Context.Object;
   --  Returns the Context for the given project tree

   type Two_Contexts is array (Context_Kind) of GPR2.Context.Object;
   --  Root and Aggregate contexts

   function Contexts (Self : Object) return Two_Contexts;

   procedure Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null);
   --  Sets the context for the project tree. The callback Changed is called
   --  for any project view which is impacted by this change of context, i.e.
   --  if the project view references directly or indirectly an external
   --  variable.

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return View.Object;

   function Has_Element (Position : Cursor) return Boolean;

   package Project_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator is new Project_Iterator.Forward_Iterator with private;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Is_Root (Position : Cursor) return Boolean;
   --  Returns True if the cursor is pointing to the root project

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return View.Object;

   function Iterate
     (Self   : Object;
      Kind   : Project.Iterator_Control := Project.Default_Iterator;
      Filter : Project.Filter_Control   := Project.Default_Filter;
      Status : Project.Status_Control   := Project.Default_Status)
      return Project_Iterator.Forward_Iterator'Class;
   --  Iterates over all project views in the tree given the iterator kind
   --  (only the project with or without imports) and the filter which can be
   --  used to iterate over only some specific projects (only the library
   --  projects for example).

   --  Views

   --  Artifacts database

   Init_Tree_Database : access procedure
     (Self : in out Build.Tree_Db.Object;
      Tree : Object);
   --  Private view of Tree_Db's initialization procedure

   function Artifacts_Database
     (Self : Object) return Build.Tree_Db.Object_Access;

   function Has_Artifacts_Database
     (Self : Object) return Boolean;

   function Artifacts_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object
     with Inline;

   function Artifacts_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object
     with Inline;

   function Source_Option (Self : Object) return Optional_Source_Info_Option;
   --  Retrieve the level of source information currently requested for
   --  the tree database.

   procedure Clear_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined);
   --  Invalidates the sources for all views in the tree if View is undefined
   --  or the source in the given view otherwise. This is needed when some
   --  sources are added or removed from the view. It is not required to call
   --  Update_Sources below, when the routine Sources on one of the views of
   --  the tree will be called, the set of sources will be recomputed.

   procedure Update_Sources
     (Self     : Object;
      Option   : Source_Info_Option := Sources_Units;
      Messages : out GPR2.Log.Object);
   --  Ensures that all views' sources are up-to-date.
   --  Option selects the information that will be gathered on the sources. The
   --   more information is requested, the slower is the update operation.
   --  Messages is used to report errors or info about the update.

   procedure Register_Project_Search_Path
     (Self : in out Object;
      Dir  : Path_Name.Object);
   --  Adds a project search path for this tree

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object;
   --  Returns the Tree project search paths

   function Archive_Suffix (Self : Object) return Filename_Type;
   --  Returns archive suffix for the project tree

   function Object_Suffix
     (Self     : Object;
      Language : Language_Id := Ada_Language) return Filename_Type;
   --  Returns object suffix for language in project tree

   function Dependency_Suffix
     (Self     : Object;
      Language : Language_Id := Ada_Language) return Filename_Type;
   --  Returns dependency suffix for language in project tree

   function Subdirs (Self : Object) return Filename_Optional;
   --  Returns the subdirs parameter <sub> of the project tree such that, for
   --  each project, the actual {executable,object,library} directories are
   --  {<exec>,<obj>,<lib>}/<sub>.

   function Has_Src_Subdirs (Self : Object) return Boolean;
   --  Returns True if the Src_Subdirs has been defined

   function Src_Subdirs (Self : Object) return Filename_Optional;
   --  Returns the src_subdirs parameter <sub> of the project tree such that,
   --  for each project, the actual source directories list will be prepended
   --  with {object_dir}/<sub>.

   function Build_Path (Self : Object) return Path_Name.Object;
   --  Path to build tree

   function Root_Path (Self : Object) return Path_Name.Object;
   --  Path to root to consider for the build tree

   function Reference (Self : Object) return access Object;
   --  Returns access to itself

   function Find_Project
     (Self      : Object;
      Base_Name : Simple_Name) return Path_Name.Object;
   --  Search for the project file named "Base_Name" through the tree's
   --  hierarchy or the project paths. If not found, Path_Name.Undefined is
   --  returned

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

   function Target_From_Command_Line
     (Self       : Object;
      Normalized : Boolean := False) return Name_Type;
   --  Returns the target specified via --target. If not specified, then
   --  return "all";

   function Runtime_From_Command_Line
     (Self : Object; Language : Language_Id) return Optional_Name_Type;
   --  Returns the runtime selected for the given language by the command
   --  line via --RTS:lang. Returns No_Name if not specified.

   function Get_KB (Self : Object) return GPR2.KB.Object;

   function File_Reader
     (Self : Object) return GPR2.File_Readers.File_Reader_Reference;

   function Environment (Self : Object) return GPR2.Environment.Object;
   --  Returns used environment.

   function Get_View
      (Self : Object;
       Id   : View_Ids.View_Id)
       return Project.View.Object;
   --  Given a View_Id Id returns the associated view if it exists. Returns
   --  Project.View.Undefined otherwise.

   function Resolve_Links (Self : Object) return Boolean;
   --  Returns whether the project should be resolving links

private

   procedure Set_Environment
     (Self : in out Object; Environment : GPR2.Environment.Object);

   package PC renames Project.Configuration;
   package PRA renames GPR2.Project.Registry.Attribute;

   package Name_View is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, View.Object);
   --  Map to find in which view a unit/source is defined

   package Filename_View is
     new Ada.Containers.Indefinite_Ordered_Maps (Filename_Type, View.Object);
   --  Map to find in which view a unit/source is defined

   package Id_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (GPR2.View_Ids.View_Id, View.Object,
      Hash            => GPR2.View_Ids.Hash,
      Equivalent_Keys => GPR2.View_Ids."=");
   --  Maps View_Ids to View objects

   type All_Search_Paths is record
      Default    : Path_Name.Set.Object :=
                     Default_Search_Paths
                       (True, GPR2.Environment.Process_Environment);
      Registered : Path_Name.Set.Object;
      Appended   : Path_Name.Set.Object;
      All_Paths  : Path_Name.Set.Object :=
                     Default_Search_Paths
                       (True, GPR2.Environment.Process_Environment);
   end record;

   use GPR2.Reporter;
   package Reporter_Holders is new Ada.Containers.Indefinite_Holders
     (GPR2.Reporter.Object'Class);

   type Object is tagged limited record
      Self              : access Object := null;
      Root              : View.Object;
      Conf              : Project.Configuration.Object;
      Base              : GPR2.KB.Object;
      Tree_Db           : Build.Tree_Db.Object;
      Runtime           : View.Object;
      With_Runtime      : Boolean := False;
      Messages          : aliased Log.Object;
      Search_Paths      : All_Search_Paths;
      Implicit_With     : Path_Name.Set.Object;
      Resolve_Links     : Boolean := False;
      Build_Path        : Path_Name.Object;
      Root_Path         : Path_Name.Object;
      Subdirs           : Unbounded_String;
      Src_Subdirs       : Unbounded_String;
      Check_Shared_Lib  : Boolean := True;
      Absent_Dir_Error  : Error_Level := Warning;
      Pre_Conf_Mode     : Boolean := True;
      Views_Set         : View.Set.Object;
      --  All projects in registration order
      Context           : Two_Contexts;
      --  Root and aggregate contexts
      View_Ids          : aliased Id_Maps.Map;
      View_DAG          : GPR2.View_Ids.DAGs.DAG;
      --  Configuration items from command line
      Explicit_Target   : Unbounded_String;
      Explicit_Runtimes : Containers.Lang_Value_Map;
      Langs_Of_Interest : Containers.Language_Set;
      --  Languages that auto-configuration should be reduced to
      File_Reader_Ref   : GPR2.File_Readers.File_Reader_Reference;
      Environment       : GPR2.Environment.Object :=
                            GPR2.Environment.Process_Environment;
      Reporter_Holder   : Reporter_Holders.Holder :=
                            Reporter_Holders.To_Holder
                              (GPR2.Reporter.Console.Create);
   end record;

   function "=" (Left, Right : Object) return Boolean is
     (Left.Self = Right.Self);

   package Project_View_Store renames GPR2.Project.View.Vector.Vector;

   type Cursor is record
      Current : Project_View_Store.Cursor;
      Tree    : access Object := null;
   end record;

   type Iterator is new Project_Iterator.Forward_Iterator with record
      Views : Project_View_Store.Vector;
      Tree  : access Object;
   end record;

   No_Element : constant Cursor :=
                  (Current => Project_View_Store.No_Element,
                   Tree    => null);

   type Constant_Reference_Type
     (View : not null access constant Project.View.Object)
   is record
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
      --  ??? We may also check that the Tree target name contains mingw or
      --  windows.

   function Contexts (Self : Object) return Two_Contexts is
     (Self.Context);

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

   function Root_Path (Self : Object) return Path_Name.Object is
      (Self.Root_Path);

   function Runtime_From_Command_Line
     (Self : Object; Language : Language_Id) return Optional_Name_Type is
     (if Self.Explicit_Runtimes.Contains (Language)
      then Optional_Name_Type (Self.Explicit_Runtimes.Element (Language))
      else No_Name);

   function With_Runtime (Self : Object) return Boolean is
     (Self.With_Runtime);

   function Has_Artifacts_Database
     (Self : Object) return Boolean is
     (Self.Tree_Db.Is_Defined);

   function Artifacts_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object is
     (Self.Tree_Db.View_Database (View.Id));

   function Artifacts_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object is
     (Self.Tree_Db.View_Database (View));

   function Get_KB (Self : Object) return GPR2.KB.Object is
     (Self.Base);

   function File_Reader
     (Self : Object)
      return GPR2.File_Readers.File_Reader_Reference is
     (Self.File_Reader_Ref);

   function Environment (Self : Object) return GPR2.Environment.Object is
      (Self.Environment);

   function Source_Option (Self : Object) return Optional_Source_Info_Option is
     (Self.Tree_Db.Source_Option);

   function Resolve_Links (Self : Object) return Boolean is
     (Self.Resolve_Links);

   function Reporter (Self : Object) return GPR2.Reporter.Object'Class is
     (Self.Reporter_Holder.Element);

end GPR2.Tree_Internal;
