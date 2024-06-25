--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Finalization;
with Ada.Iterator_Interfaces;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Tree_Db;
with GPR2.Build.View_Db;
with GPR2.Containers;
with GPR2.Context;
with GPR2.File_Readers;
with GPR2.KB;
with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Configuration;
pragma Elaborate (GPR2.Project.Configuration);
--  Elaborate to avoid a circular dependency due to default Elaborate_Body
with GPR2.Project.View.Set;
with GPR2.Project.View.Vector;
with GPR2.View_Ids;

private with GNATCOLL.Refcount;
private with GPR2.Tree_Internal;

package GPR2.Project.Tree is

   use type GPR2.Context.Object;

   type Verbosity_Level is
     (Quiet,
      Minimal,
      Errors,
      Warnings_And_Errors,
      Info,
      Linter);
   --  Quiet: do not display anything
   --  Minimal: display only messages concerning new files or directories
   --  Warnings_And_Errors: in case the configuration or tree logs contain
   --    warnings or errors, display them
   --  Info: also display informational messages
   --  Linter: also display gpr linter messages

   Verbosity : Verbosity_Level := Warnings_And_Errors;
   --  Indicates the global verbosity expected by the user of this library.
   --  Logs will be displayed only if the verbosity level is verbose enough
   --  according to the message level (see GPR2.Message).
   --  The active message reporter is used to report such message, that will
   --  be displayed on the console by default (see GPR2.Message.Reporter).

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
     Iterator_Element  => View.Object;
   --  Note: the project tree object is actually a Refcounted object.
   --  This object is null by default, only a call to Load,
   --  Register_Project_Search_Path or Restrict_Autoconf_To_Languages will
   --  create the actual value.
   --  This means that if a copy of an instance of Object is done before the
   --  underlying value is created, the copy will remain null.
   --  This also means that after the initialisation of the object is done,
   --  copies become actually aliases, meaning that any change on one instance
   --  will modify all copies of it.

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   overriding function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are the same tree

   procedure Create (Self : in out Object)
     with Post => Self.Is_Defined;
   --  Creates an empty tree structure

   procedure Register_Project_Search_Path
     (Self : in out Object;
      Dir  : Path_Name.Object);
   --  Adds a project search path for this tree

   procedure Restrict_Autoconf_To_Languages
     (Self  : in out Object;
      Langs : GPR2.Containers.Language_Set);
   --  Sets a list of languages that auto-configuration will be reduced to
   --  from the actual set of languages used in project tree. Empty set of
   --  languages means regular auto-configuration with no reductions.

   function Load
     (Self                   : in out Object;
      Options                : GPR2.Options.Object'Class;
      With_Runtime           : Boolean := False;
      Absent_Dir_Error       : GPR2.Error_Level := GPR2.Warning;
      Allow_Implicit_Project : Boolean := True;
      Environment            : GPR2.Environment.Object :=
                                 GPR2.Environment.Process_Environment;
      Config                 : GPR2.Project.Configuration.Object :=
                                 GPR2.Project.Configuration.Undefined;
      File_Reader            : GPR2.File_Readers.File_Reader_Reference :=
                                 GPR2.File_Readers.No_File_Reader_Reference)
      return Boolean;
   --  Load a project tree using configured options.
   --  If successful, Tree contains loaded project tree.
   --  If Tree is undefined on entry, project search paths are automatically
   --   registered.
   --  Load messages are appended to Log.
   --  With_Runtime: whether the runtime sources are looked for when updating
   --   the sources.
   --  Absent_Dir_Error: whether a missing directory should be treated as an
   --   error or a warning.
   --  Environment allows passing explictely environment variables to the
   --   tree.
   --  Config allows passing explictely the configuration project. If defined
   --   then options --config or --autoconf are ignored.
   --  Verbosiuty indicates the level of messages that can be displayed
   --   to the active mexsage reporter (by default the console).

   procedure Unload (Self : in out Object);
   --  Clears the internal structure of the Object

   function Root_Project (Self : Object) return View.Object
     with Pre  => Self.Is_Defined;
   --  Returns the root project for the given tree.

   function Namespace_Root_Projects (Self : Object) return View.Set.Object
     with Pre  => Self.Is_Defined;
   --  Returns the list of namespace root projects: either the root project
   --  for regular trees, or the root of the subtrees for an aggregate project.

   function Has_Configuration (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a configuration project is loaded on this tree

   function Configuration (Self : Object) return Configuration.Object
     with Pre => Self.Is_Defined and then Self.Has_Configuration;
   --  Returns the configuration project for the given tree

   function Has_Runtime_Project (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a runtime project is loaded on this tree

   function Runtime_Project (Self : Object) return View.Object
     with Pre => Self.Is_Defined;
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

   function Artifacts_Dir (Self : Object) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Tries to return a directory that can be used to store artifacts that
   --  are global to the tree.
   --  This returns the object directory of the root view if available, else
   --  it returns the root view's project directory.
   --  **Important note** project directories may not be writable, as only
   --  object dirs are required to have read/write access. So this function
   --  needs to be used with care.

   function Ordered_Views (Self : Object) return View.Vector.Object
     with Pre => Self.Is_Defined;

   function Has_Messages (Self : Object) return Boolean;
   --  Returns whether some messages are present for this project tree

   function Log_Messages (Self : Object) return not null access Log.Object
     with Pre  => Self.Is_Defined,
          Post => not Self.Has_Messages
                  or else not Log_Messages'Result.Is_Empty;
   --  Returns the Logs, this contains information, warning and error messages
   --  found while handling the project.

   --  Context

   --  Note that the context of the project tree corresponds to the context of
   --  the root project view.

   function Has_Context (Self : Object) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True if the project tree has some context. If any of the project
   --  in the tree has some external variables then a context is present. A
   --  project without context is fully static as it does not reference any
   --  external (and so modifiable) variables.

   function Context (Self : Object) return Context.Object
     with Pre  => Self.Is_Defined,
          Post => Self.Has_Context = (Context'Result /= GPR2.Context.Empty);
   --  Returns the Context for the given project tree

   function Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null)
     return Boolean
     with Pre => Self.Is_Defined;
   --  Sets the context for the project tree. The callback Changed is called
   --  for any project view which is impacted by this change of context, i.e.
   --  if the project view references directly or indirectly an external
   --  variable.
   --  If the new context generate an error while re-loading the project tree
   --  then False is returned.

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

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return View.Object
     with Pre => Self.Is_Defined and then Position /= No_Element;

   function Iterate
     (Self   : Object;
      Kind   : Project.Iterator_Control := Project.Default_Iterator;
      Filter : Project.Filter_Control   := Project.Default_Filter;
      Status : Project.Status_Control   := Project.Default_Status)
      return Project_Iterator.Forward_Iterator'Class
     with Pre => Self.Is_Defined;
   --  Iterates over all project views in the tree given the iterator kind
   --  (only the project with or without imports) and the filter which can be
   --  used to iterate over only some specific projects (only the library
   --  projects for example).

   --  Views

   --  Artifacts database

   function Artifacts_Database
     (Self : Object) return Build.Tree_Db.Object_Access
     with Pre => Self.Is_Defined;

   function Has_Artifacts_Database
     (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function Artifacts_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object
     with Pre => Self.Is_Defined and then View.Kind in With_Object_Dir_Kind,
          Inline;

   function Artifacts_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object
     with Pre => Self.Is_Defined, Inline;

   function Source_Option (Self : Object) return Optional_Source_Info_Option;
   --  Retrieve the level of source information currently requested for
   --  the tree database.

   procedure Clear_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined)
     with Pre  => Self.Is_Defined,
          Post => (if not View.Is_Defined
                   then Self.Source_Option = No_Source);
   --  Invalidates the sources for all views in the tree if View is undefined
   --  or the source in the given view otherwise. This is needed when some
   --  sources are added or removed from the view. It is not required to call
   --  Update_Sources below, when the routine Sources on one of the views of
   --  the tree will be called, the set of sources will be recomputed.

   procedure Update_Sources
     (Self     : Object;
      Option   : Source_Info_Option := Sources_Units)
     with Pre => Self.Is_Defined;
   --  Ensures that all views' sources are up-to-date.
   --  Option selects the information that will be gathered on the sources. The
   --   more information is requested, the slower is the update operation.

   function Update_Sources
     (Self     : Object;
      Option   : Source_Info_Option := Sources_Units) return Boolean
     with Pre => Self.Is_Defined;
   --  Same as above, and returns False upon error detected.

   procedure Update_Sources
     (Self     : Object;
      Messages : out GPR2.Log.Object;
      Option   : Source_Info_Option := Sources_Units)
     with Pre => Self.Is_Defined;
   --  Same as above and returns the messages generated during the load
   --  operation.

   procedure For_Each_Ada_Closure
     (Self              : Object;
      Action            : access procedure
                            (Unit : Build.Compilation_Unit.Object);
      Mains             : Containers.Filename_Set :=
                            Containers.Empty_Filename_Set;
      All_Sources       : Boolean := False;
      Root_Project_Only : Boolean := False;
      Externally_Built  : Boolean := False)
     with Pre => Self.Is_Defined and then Self.Source_Option >= Sources_Units;
   --  Call action for each source of the closure of the loaded tree (Mains
   --  or library interfaces and their dependencies).
   --.
   --  Mains:
   --    used to limit the entry points of the closure to the sources or
   --    units specified in this parameter
   --  All_Sources (-U command line option):
   --    process also sources that are not in Main
   --  Root_Project_Only (--no-subproject command line option):
   --    will return only sources from the root project.
   --  Externally_Built:
   --    if not set, units defined in externally built views will be ignored.
   --
   --  Note that if Root_Project_Only is set and the root project is an
   --    aggregate project, then the closure is considered empty
   --
   --  Raises Usage_Error when Mains is specified and All_Sources is set.

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns the Tree project search paths

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

   function Find_Project
     (Self      : Object;
      Base_Name : Simple_Name) return Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Search for the project file named "Base_Name" through the tree's
   --  hierarchy or the project paths. If not found, Path_Name.Undefined is
   --  returned

   Target_Name : constant Name_Type;
   --  Native host target

   function Get_View
      (Self : Object;
       Id   : GPR2.View_Ids.View_Id)
       return Project.View.Object;
   --  Given a View_Id Id returns the associated view if it exists. Returns
   --  Project.View.Undefined otherwise.

   function Get_KB (Self : Object) return GPR2.KB.Object
     with Pre => Self.Is_Defined;

private

   package Pools is new GNATCOLL.Refcount.Headers.Typed (Tree_Internal.Object);
   subtype Tree_Internal_Access is Pools.Element_Access;

   use type Pools.Element_Access;

   type Object is new Ada.Finalization.Controlled with record
      Tree : Tree_Internal_Access;
   end record;
   pragma Finalize_Storage_Only (Object);

   overriding procedure Adjust (Self : in out Object);
   overriding procedure Finalize (Self : in out Object);

   Undefined : constant Object := (Ada.Finalization.Controlled with
                                   others => <>);

   overriding function "=" (Left, Right : Object) return Boolean is
     (Left.Tree = Right.Tree);

   type Cursor is record
      Internal : Tree_Internal.Cursor;
   end record;

   No_Element : constant Cursor :=
                  (Internal => Tree_Internal.No_Element);

   Target_Name : constant Name_Type := Tree_Internal.Target_Name;

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Subdirs (Self : Object) return Filename_Optional is
     (Self.Tree.Subdirs);

   function Has_Src_Subdirs (Self : Object) return Boolean is
      (Self.Tree.Has_Src_Subdirs);

   function Src_Subdirs (Self : Object) return Filename_Optional is
      (Self.Tree.Src_Subdirs);

   function Build_Path (Self : Object) return Path_Name.Object is
      (Self.Tree.Build_Path);

   function Has_Artifacts_Database
     (Self : Object) return Boolean is
     (Self.Tree.Has_Artifacts_Database);

   function Artifacts_Database
     (Self : Object) return Build.Tree_Db.Object_Access is
     (Self.Tree.Artifacts_Database);

   function Artifacts_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object is
     (Self.Tree.Artifacts_Database (View));

   function Artifacts_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object is
     (Self.Tree.Artifacts_Database (View));

   function Source_Option (Self : Object) return Optional_Source_Info_Option is
     (Self.Tree.Source_Option);

   function Namespace_Root_Projects (Self : Object) return View.Set.Object is
     (Self.Tree.Namespace_Root_Projects);

   function Root_Project (Self : Object) return View.Object is
     (Self.Tree.Root_Project);

   function Has_Configuration (Self : Object) return Boolean is
     (Self.Tree.Has_Configuration);

   function Configuration (Self : Object) return Project.Configuration.Object
   is (Self.Tree.Configuration);

   function Has_Runtime_Project (Self : Object) return Boolean is
      (Self.Tree.Has_Runtime_Project);

   function Runtime_Project (Self : Object) return View.Object is
     (Self.Tree.Runtime_Project);

   function Target
     (Self : Object; Canonical : Boolean := False) return Name_Type
   is (Self.Tree.Target (Canonical));

   function Runtime
     (Self : Object; Language : Language_Id) return Optional_Name_Type
   is (Self.Tree.Runtime (Language));

   function Artifacts_Dir (Self : Object) return Path_Name.Object
   is (Self.Tree.Artifacts_Dir);

   function Ordered_Views (Self : Object) return View.Vector.Object is
     (Self.Tree.Ordered_Views);

   function Has_Messages (Self : Object) return Boolean is
     (Self.Is_Defined and then not Self.Tree.Log_Messages.Is_Empty);

   function Log_Messages (Self : Object) return not null access Log.Object is
     (Self.Tree.Log_Messages);

   function Has_Context (Self : Object) return Boolean is
     (Self.Tree.Has_Context);

   function Context (Self : Object) return GPR2.Context.Object is
     (Self.Tree.Context);

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object is
     (Self.Tree.Project_Search_Paths);

   function Find_Project
     (Self : Object; Base_Name : Simple_Name) return Path_Name.Object
   is (Self.Tree.Find_Project (Base_Name));

   function Get_View
     (Self : Object; Id : GPR2.View_Ids.View_Id) return Project.View.Object
   is (Self.Tree.Get_View (Id));

   function Get_KB (Self : Object) return GPR2.KB.Object is
     (Self.Tree.Get_KB);

   function Has_Element (Position : Cursor) return Boolean is
     (Tree_Internal.Has_Element (Position.Internal));

   function Element (Position : Cursor) return View.Object is
     (Tree_Internal.Element (Position.Internal));

   function Is_Root (Position : Cursor) return Boolean is
     (Tree_Internal.Is_Root (Position.Internal));

   function Constant_Reference
     (Self : aliased Object; Position : Cursor) return View.Object
   is (Self.Tree.Constant_Reference (Position.Internal));

   type Iterator is new Project_Iterator.Forward_Iterator with record
      Internal : GPR2.Tree_Internal.Iterator;
   end record;

   overriding function First (Iter : Iterator) return Cursor is
      (Cursor'(Internal => Iter.Internal.First));

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor is
      (Cursor'(Internal => Iter.Internal.Next (Position.Internal)));

   function Iterate
     (Self   : Object;
      Kind   : Project.Iterator_Control := Project.Default_Iterator;
      Filter : Project.Filter_Control   := Project.Default_Filter;
      Status : Project.Status_Control   := Project.Default_Status)
      return Project_Iterator.Forward_Iterator'Class
   is (Iterator'(Internal =>
                    Tree_Internal.Iterator
                      (Self.Tree.Iterate (Kind, Filter, Status))));

end GPR2.Project.Tree;
