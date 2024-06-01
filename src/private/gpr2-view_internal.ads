--
--  Copyright (C) 2019-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Pack_Internal;
with GPR2.Project_Parser.Set;
with GPR2.Project.Attribute_Cache;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Project.View.Set;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Value;
with GPR2.View_Base_Internal;
with GPR2.View_Ids;
with GPR2.View_Ids.Set;

limited with GPR2.Tree_Internal;

private package GPR2.View_Internal is

   use GPR2.Project;
   use type Project.View.Object;
   use type Path_Name.Object;

   --  Tree contains the Project parser object. This is shared by all projects
   --  view in all loaded trees. That is there is always a single instance of
   --  the project parser object.
   --  Imports contains the list of all imported projects for Project.

   type Tree is record
      Project  : GPR2.Project_Parser.Object;
      Imports  : GPR2.Project_Parser.Set.Object;
      Extended : GPR2.Project_Parser.Object;
   end record;

   package Project_Vector is new Ada.Containers.Vectors
     (Positive, View.Object);

   package Project_View_Store is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, View.Object);

   type Dir_Cache_Value is record
      Is_Set : Boolean := False;
      Value  : GPR2.Path_Name.Object;
   end record;
   --  Used to cache directory attributes that otherwise require
   --  post-processing each time they are retrieved (Call to
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

   type Data is new View_Base_Internal.Definition_Base with record
      Trees           : Tree;
      --  Raw parsed values for the project

      --  View hierarchy:

      Tree              : access Tree_Internal.Object;
      --  The project tree for this view
      Root_Views        : View_Ids.Set.Set;
      --  Either root aggregated project view, or just root view of the tree
      Extending         : View_Base_Internal.Weak_Reference;
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
      Agg_Libraries     : GPR2.View_Ids.Set.Set;
      --  List of aggregate libraries that use this view
      Aggregated        : Project_Vector.Vector;
      --  List of projects that are aggregated by this view

      --  View values

      Attrs             : Project.Attribute.Set.Object;
      --  The view's raw attributes
      Vars              : Project.Variable.Set.Object;
      --  The view's variables
      Packs             : Pack_Internal.Set.Map;
      --  The view's raw packages
      Types             : Project.Typ.Set.Object;
      --  The view's type definitions

      Unique_Id         : GPR2.View_Ids.View_Id;

      --  Some general information

      Context           : GPR2.Context.Context_Kind := GPR2.Context.Root;
      --  Use the aggregate context including External attributes or only the
      --  root context.

      --  Cached values for faster retrieval of attributes

      Languages         : Containers.Language_Set;
      --  Languages as Language_Ids defined for the view
      Interface_Sources : Containers.Source_Path_To_Sloc.Map;
      --  Source basenames that are part of the library interface
      Interface_Units   : Containers.Unit_Name_To_Sloc.Map;
      --  Source unit names that are part of the library interface
      Cache             : Attribute_Cache.Object;
      --  Attribute's final values cache
      Dir_Cache         : Dir_Cache_List;
      --  View's directories cache, heavily used when loading sources and
      --  retrieving build artifacts.
   end record;

   type Ref is access all Data;

   type Const_Ref is access constant Data;

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Tree package --
   --------------------------------------------------------------

   Register : access function
     (Def : in out View_Internal.Data) return Project.View.Object;
   --  Register view definition in the project tree

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.View package --
   --------------------------------------------------------------

   Set : access procedure
     (Ref : out View.Object; Def : Data);
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

   Weak : access function (View : Project.View.Object)
            return View_Base_Internal.Weak_Reference;
   --  Get weak reference from view. Need to avoid circular references

   Strong : access function
              (View : View_Base_Internal.Weak_Reference)
                return Project.View.Object;
   --  Get view from weak reference

   -----------------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Configuration package --
   -----------------------------------------------------------------------

   Bind_Configuration_To_Tree : access
     procedure (Config : in out Project.Configuration.Object;
                Tree   : not null access Tree_Internal.Object);

   Configuration_Externals : access
     function (Config : Project.Configuration.Object)
       return GPR2.Project_Parser.Externals_Map;

   -------------------------------------------
   -- Helper routines for GPR2.Project.View --
   -------------------------------------------

   function Get_Context
     (View : Project.View.Object) return Context.Object;

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

   procedure Foreach
     (Base_Dir          : GPR2.Path_Name.Object;
      Messages          : in out GPR2.Log.Object;
      Directory_Pattern : GPR2.Filename_Optional;
      Source            : GPR2.Source_Reference.Value.Object;
      File_CB           : access procedure
                            (File      : GPR2.Path_Name.Full_Name;
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

   procedure Check_Aggregate_Library_Dirs (Tree : Tree_Internal.Object);
   --  Report aggregate library (ALI)? directory cannot be shared with
   --  (object|library) directory of aggregated project errors

   procedure Check_Excluded_Source_Dirs (Tree : Tree_Internal.Object);
   --  Check that excluded source dirs are actual directories.

   procedure Check_Package_Naming (Tree : Tree_Internal.Object);
   --  For all tree's views check Casing, Dot_Replacement, Spec_Suffix,
   --  Body_Suffix and Separate_Suffix naming package attributes value.

   procedure Clear_Cache (Def : in out Data);
   --  Used during the reload of a tree to clear values cached in the view

   procedure Disable_Cache (Def : in out Data);
   --  Used during parsing, where attributes can change as we parse.

   procedure Enable_Cache (Def : in out Data);
   --  Start using cache to store attribute values

end GPR2.View_Internal;
