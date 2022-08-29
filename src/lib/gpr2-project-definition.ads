--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with GPR2.Context;
with GPR2.Log;
with GPR2.Project.Attribute_Cache;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Pack;
with GPR2.Project.Parser.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Project.View.Set;
with GPR2.Project.Unit_Info.Set;
with GPR2.Source_Info;
with GPR2.Source_Reference;
with GPR2.Unit;
with GPR2.View_Ids;
with GPR2.View_Ids.Set;

limited with GPR2.Project.Tree;

private package GPR2.Project.Definition is

   use type View.Object;
   use type Path_Name.Object;

   --  Tree contains the Project parser object. This is shared by all projects
   --  view in all loaded trees. That is there is always a single instance of
   --  the project parser object.
   --  Imports contains the list of all imported projects for Project.

   type Tree is record
      Project  : GPR2.Project.Parser.Object;
      Imports  : GPR2.Project.Parser.Set.Object;
      Extended : GPR2.Project.Parser.Object;
   end record;

   package Simple_Name_Source is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Simple_Name, Project.Source.Set.Cursor, GPR2.Hash, GPR2."=",
        Project.Source.Set."=");
   --  Map to find in which view a source is defined

   package Unit_Source is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Project.Source.Set.Cursor, Ada.Strings.Hash, "=",
        "=" => Project.Source.Set."=");
   --  Map to find in which view a unit is defined

   package Unit_Name_To_Sloc is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Name_Type, Source_Reference.Object, GPR2.Hash, GPR2."=",
        Source_Reference."=");
   --  Used for the Interface_Units container which will initially store all
   --  the units from the Library_Interface attribute, as a mapping from
   --  unit names to slocs.

   package Source_Path_To_Sloc is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Filename_Type, Source_Reference.Object, GPR2.Hash, GPR2."=",
        Source_Reference."=");
   --  Same as above but for the Interfaces attribute, so here we are using
   --  Filename_Type instead of Name_Type since we're dealing with
   --  filenames.

   function Key (Unit : GPR2.Unit.Object) return String is
     ((if Unit.Kind in GPR2.Unit.Spec_Kind then 'S' else 'B')
       & To_Lower (Unit.Name));
   --  Key function used as index to Unit_Source

   package Project_Vector is new Ada.Containers.Vectors
     (Positive, View.Object);

   package Project_View_Store is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, View.Object);

   type Dir_Cache_Value is record
      Is_Set : Boolean := False;
      Value  : GPR2.Path_Name.Object;
   end record;
   --  Used to cache directory attributes that require otherwise
   --  postprocessing each time they are retrieved (Call to
   --  Apply_Root_And_Subdirs).

   type Cacheable_Dir_Attrs is
     (Exec_Dir, Library_Ali_Dir, Library_Dir, Library_Src_Dir, Object_Dir);

   type Dir_Cache_List is array (Cacheable_Dir_Attrs) of Dir_Cache_Value;

   --  Data contains a project view data. We have all the attributes, variables
   --  and packages with the final values as parsed with the project's context
   --  in the given tree. Imports here are the project views corresponding to
   --  the imports in Trees.
   --
   --  Either a Data has a context or is referencing another containing
   --  the context. This is used for aggregate project which can be used to
   --  refine the global context by setting some external values with the
   --  corresponding attribute. So both the root project and all aggregate
   --  projects have a context. All other projects are referencing a project
   --  which own a context.

   type Data is new Definition_Base with record
      Trees           : Tree;
      --  Raw parsed values for the project

      --  View hierarchy:

      Tree              : access Project.Tree.Object;
      --  The project tree for this view
      Root_View         : Weak_Reference;
      --  Either root aggregated project view, or just root view of the tree
      Extending       : Weak_Reference;
      --  If defined, the view that is extending this definition
      Extended_Root     : View.Object;
      --  If defined, the root view (in case of extends all) of the extended
      --  views.
      Extended          : View.Set.Object;
      --  The set of views that are extended
      Imports           : Project_View_Store.Map;
      --  List of imported views
      Limited_Imports   : Project_View_Store.Map;
      --  List of limited imported views
      Closure           : Project_View_Store.Map;
      --  The list of views sources from self can see
      Agg_Libraries     : GPR2.View_Ids.Set.Object;
      --  List of aggregate libraries that use this view
      Aggregated        : Project_Vector.Vector;
      --  List of projects that are aggregated by this view

      --  View values

      Attrs             : Project.Attribute.Set.Object;
      --  The view's raw attributes
      Vars              : Project.Variable.Set.Object;
      --  The view's variables
      Packs             : Project.Pack.Set.Map;
      --  The view's raw packages
      Types             : Project.Typ.Set.Object;
      --  The view's type definitions

      Sources           : Project.Source.Set.Object;
      Sources_Map       : Simple_Name_Source.Map;
      Units_Map         : Unit_Source.Map;
      Units             : Unit_Info.Set.Object;
      Unique_Id         : GPR2.View_Ids.View_Id;

      --  Some general information

      Context           : GPR2.Context.Context_Kind := GPR2.Context.Root;
      --  Use the aggregate context including External attributes or only the
      --  root context.

      --  Cached values for faster retrieval of attributes

      Interface_Sources : Source_Path_To_Sloc.Map;
      --  Source basenames that are part of the library interface
      Interface_Units   : Unit_Name_To_Sloc.Map;
      --  Source unit names that are part of the library interface
      Cache             : Attribute_Cache.Object;
      --  Attribute's final values cache
      Dir_Cache         : Dir_Cache_List;
      --  View's directories cache, havily used when loading sources and
      --  retrieving build artifacts.
   end record;

   type Ref is access all Data;

   type Const_Ref is access constant Data;

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Tree package --
   --------------------------------------------------------------

   Register : access function
     (Def : in out Definition.Data) return Project.View.Object;
   --  Register view definition in the project tree

   Get_Context : access function
     (View : Project.View.Object) return Context.Object;
   --  Returns context of the project view

   Are_Sources_Loaded : access function
     (Tree : Project.Tree.Object) return Boolean;
   --  Returns True if the sources are loaded into project tree

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.View package --
   --------------------------------------------------------------

   Set : access procedure
     (Ref : out View.Object; Def : Definition_Base'Class);
   --  Convert definition to view to register

   Get : access function (View : Project.View.Object) return Ref;
   --  Returns the project data definition reference for the given view.
   --  This routine should be used only when we need to change view definition
   --  for the read only object.

   Get_RO : access function (View : Project.View.Object) return Const_Ref;
   --  Returns the project data definition constant reference for the given
   --  view.

   Get_RW : access function (View : in out Project.View.Object) return Ref;
   --  Returns the project data definition reference to modify view

   Refcount : access function (View : Project.View.Object) return Natural;
   --  Returns reference counter of the view

   Weak : access function (View : Project.View.Object) return Weak_Reference;
   --  Get weak reference from view. Need to avoid circular references

   Strong : access function (View : Weak_Reference) return Project.View.Object;
   --  Get view from weak reference

   Change_Actual_View : access function
     (Self : Source.Object; View : Project.View.Object) return Source.Object;

   -----------------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Configuration package --
   -----------------------------------------------------------------------

   Bind_Configuration_To_Tree : access procedure
     (Config : in out Configuration.Object;
      Tree   : not null access Project.Tree.Object);

   -------------------------------------------
   -- Helper routines for GPR2.Project.View --
   -------------------------------------------

   function Has_Packages
     (Def  : Data;
      Name : Package_Id) return Boolean
   is
     (if Name = Project_Level_Scope
      then not Def.Packs.Is_Empty
      else Def.Packs.Contains (Name));
   --  Returns true if the project view definition has some packages defined

   function Has_Types
     (Def  : Data;
      Name : Optional_Name_Type) return Boolean
   is
     (if Name = No_Name
      then not Def.Types.Is_Empty
      else Def.Types.Contains (Name));

   function Is_Extended (Def : Data) return Boolean
   is (not Def.Extending.Was_Freed);

   procedure Update_Sources
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean;
      Backends      : Source_Info.Backend_Set)
     with Pre => View.Is_Defined;
   --  Ensure that the view definition sources are up-to-date. This is needed
   --  before computing the dependencies of a source in the project tree. This
   --  routine is called where needed and is there for internal use only.
   --  If Stop_On_Error is True and an error occurred on reading the sources,
   --  then the exception Project_Error raised. If Stop_On_Error is False then
   --  no exception is raised and errors can be discovered only from the
   --  Log.Object taken from the View.Tree.Log_Messages call.
   --  Backends parameter defines the set of parser that can be used to parse
   --  the source information.

   procedure Update_Sources_List
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean)
   with Pre => View.Is_Defined;
   --  Populate the list of sources for the given view

   procedure Update_Sources_Parse
     (Def : in out Data; Backends : Source_Info.Backend_Set);
   --  Parse the project's source dependency file and populate the
   --  corresponding source_info.

   procedure Foreach
     (Base_Dir          : GPR2.Path_Name.Object;
      Messages          : in out GPR2.Log.Object;
      Directory_Pattern : GPR2.Filename_Optional;
      Source            : GPR2.Source_Reference.Value.Object;
      File_CB           : access procedure
                            (File      : GPR2.Path_Name.Object;
                             Timestamp : Ada.Calendar.Time);
      Directory_CB      : access procedure
                            (Directory       : GPR2.Path_Name.Object;
                             Is_Root_Dir     : Boolean;
                             Do_Dir_Visit    : in out Boolean;
                             Do_Subdir_Visit : in out Boolean) := null)
     with Pre => (File_CB /= null or else Directory_CB /= null)
                   and then Base_Dir.Is_Defined and then Base_Dir.Is_Directory;
   --  Visit Directory_Pattern (recursive if "**" at end) calling callbacks
   --  on each directory/file visited.
   --  When entering a Directory, Directory_CB callback can avoid Directory's
   --  files to be handled. If recursive mode, sub directories are visited if
   --  Do_Subdir_Visit is True.
   --  Is_Root_Dir is set when entering the top level dir.
   --  File_CB is called for each regular file found.
   --  Source reference is used when messages added to Self.Tree's log

   procedure Source_Directories_Walk
     (View      : Project.View.Object;
      Source_CB : access procedure
                    (Dir_Reference : GPR2.Source_Reference.Value.Object;
                     Source        : GPR2.Path_Name.Object;
                     Timestamp     : Ada.Calendar.Time);
      Dir_CB    : access procedure (Dir_Name : GPR2.Path_Name.Object));
   --  Walks the source directories of Self and calls Source_CB on every
   --  file found, and Dir_CB on each directory found, if the callbacks are
   --  defined.

   procedure Sources_Map_Insert
     (Def : in out Data;
      Src : Project.Source.Object;
      C   : Project.Source.Set.Cursor);
   --  Insert source into simple filename index if it is not yet inserted

   function Is_Sources_Loaded (View : Project.View.Object) return Boolean
     with Pre => View.Is_Defined;
   --  Return True if Sources already updated once

   procedure Check_Same_Name_Extended (View : Project.View.Object);
   --  Report "cannot extend a project with the same name" errors

   procedure Check_Aggregate_Library_Dirs (View : Project.View.Object);
   --  Report aggregate library (ALI)? directory cannot be shared with
   --  (object|library) directory of aggregated project errors

   procedure Check_Excluded_Source_Dirs (View : Project.View.Object);
   --  Check that excluded source dirs are actual directories.

   procedure Check_Package_Naming (View : Project.View.Object);
   --  For all tree's views check Casing, Dot_Replacement, Spec_Suffix,
   --  Body_Suffix and Separate_Suffix naming package attributes value.

   procedure Clear_Cache (Def : in out Data);
   --  Used during the reload of a tree to clear values cached in the view

   procedure Disable_Cache (Def : in out Data);
   --  Used during parsing, where attributes can change as we parse.

   procedure Enable_Cache (Def : in out Data);
   --  Start using cache to store attribute values

end GPR2.Project.Definition;
