--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  A specific view of a project as seen in a tree. A project view is retrieved
--  from a tree, see Project.Tree package. A project view differ from a
--  standard project object from the parser as it gives access to the actual
--  attributes, variables and packages values depending on the current context
--  for the corresponding tree. It also gives the sources for the views and
--  this include sources from extended project or aggregated project if needed.

with Ada.Calendar;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids;

limited with GPR2.Build.Compilation_Unit;
limited with GPR2.Build.Compilation_Unit.Maps;
limited with GPR2.Build.Source.Sets;
limited with GPR2.Build.Source;
limited with GPR2.Build.View_Db;
limited with GPR2.Project.Tree;
limited with GPR2.Project.View.Set;
limited with GPR2.Project.View.Vector;

private with GPR2.Pack_Internal;
private with GPR2.View_Base_Internal;

package GPR2.Project.View is

   package PRA renames GPR2.Project.Registry.Attribute;

   use GPR2.Context;

   type Object is tagged private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Id (Self : Object) return GPR2.View_Ids.View_Id
     with Pre => Self.Is_Defined;
   --  Returns a unique Id for Self

   function "<" (Left, Right : Object) return Boolean;
   --  Ordering a project object to be able to build an ordered map for example

   function Path_Name (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Full pathname of the corresponding project file

   function Dir_Name (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Full directory name of the corresponding project file

   function Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  The name of the project

   function Tree (Self : Object) return GPR2.Project.Tree.Object
     with Pre => Self.Is_Defined;

   function Qualifier (Self : Object) return Project_Kind
     with Pre => Self.Is_Defined;
   --  The qualifier as specified in the project file

   function Kind (Self : Object) return Project_Kind
     with Pre  => Self.Is_Defined,
          Post => Kind'Result = Self.Qualifier
                  or else Self.Qualifier = K_Standard;
   --  The actual kind of the project file. This may be different if the
   --  Qualifier is not specified.

   function Has_Imports (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project has some imports (either standard or
   --  limited).

   function Closure
     (Self               : Object;
      Include_Self       : Boolean := False;
      Include_Extended   : Boolean := False;
      Include_Aggregated : Boolean := False)
      return GPR2.Project.View.Vector.Object
     with Pre => Self.Is_Defined;
   --  Returns the list of views that are withed or limited withed by Self
   --  recursively.
   --  If Include_Self is set, then Self will be part of the
   --  returned set.
   --  If Include_Aggregated is set, then the projects aggregated by any
   --  aggregate library in the closure will also be part of this closure.

   function Imports
     (Self : Object; Recursive : Boolean := False) return Set.Object
     with Pre => Self.Is_Defined;
   --  Returns all imported project views

   function Limited_Imports
     (Self : Object; Recursive : Boolean := False) return Set.Object
     with Pre => Self.Is_Defined;
   --  Returns all limited imported project views

   function Is_Extending
     (Self : Object; Parent : Object'Class := Undefined) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project is extending another project.
   --  If Parent is defined then returns True only if the Parent project is in
   --  the ancestors transitively.

   function Is_Extending_All (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project is extending all another project

   function Extended (Self : Object) return Set.Object
     with Pre => Self.Is_Defined and then Self.Is_Extending;
   --  Returns the extended projects
   --  In case of simple extension, this will contain only one view, but in
   --  case of extends all, the views of the subtree are returned.

   function Extended_Root (Self : Object) return Object
     with Pre => Self.Is_Defined and then Self.Is_Extending;
   --  Returns the root view of the extended subtree. In case of extends
   --  all this will thus return the project that is explicitly extended.

   function Is_Extended (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the view is extended by another project

   function Extending (Self : Object) return Object
     with Pre  => Self.Is_Defined and then Self.Is_Extended,
          Post => Extending'Result.Is_Extending;
   --  Return the extending view

   function Is_Namespace_Root (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Whether this view is either the root of the tree or the root
   --  project of an aggregated subtree.

   function Aggregated
     (Self      : Object;
      Recursive : Boolean := True) return Set.Object
     with Pre => Self.Is_Defined and then Self.Kind in Aggregate_Kind;
   --  Get the aggregated views.
   --  In case Self is an aggregate project and recursive is set, if
   --  one of the aggregated project is also an "aggregate project" then
   --  its aggregated views are also returned.
   --  In case Self is an aggregate library, this parameter has no effect.

   function Aggregate_Libraries (Self : Object) return Set.Object
     with Pre => Self.Is_Defined;
   --  Returns the list of aggregate library projects that contain Self

   function Is_Aggregated_In_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self is part of an aggregate library project

   function View_For (Self : Object; Name : Name_Type) return Object
     with Pre => Self.Is_Defined;
   --  Returns the view for the given name accessible from Self context. This
   --  can be either an import project, an extends project or the special
   --  projects Runtime or Config if defined in the corresponding project tree.

   --  Context

   function Has_Context (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project tree has some context. If any of the project
   --  in the tree has some external variables then a context is present. A
   --  project without context is fully static has it does not reference any
   --  external (and so modifiable) variables.

   function Context (Self : Object) return Context.Object
     with Pre  => Self.Is_Defined,
          Post => Self.Has_Context = (Context'Result /= GPR2.Context.Empty);
   --  Returns the Context for the given project tree

   function Context (Self : Object) return Context_Kind
     with Pre => Self.Is_Defined;

   function Has_Aggregate_Context (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self context is from aggregated subtree

   --  Attributes

   function Has_Attribute
     (Self   : Object;
      Name   : Q_Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index) return Boolean
     with Pre => Self.Is_Defined;
   --  Check whether an attribute with Name and Index has been defined, either
   --  at the top-level project if Pkg is not defined, in within the package
   --  named Pkg.
   --  At_Pos denotes the unit index in the source file represented by Index

   function Attribute
     (Self   : Object;
      Name   : Q_Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index)
      return Project.Attribute.Object
     with Pre  => Self.Is_Defined;
   --  Compute the final value for the attribute Name in package Pkg.
   --  If the Attribute is not defined in the View, and has no default value,
   --  then Project.Attribute.Undefined is returned.

   function Check_Attribute
     (Self   : Object;
      Name   : Q_Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index;
      Result : out Project.Attribute.Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True and set Result if the value of the attribute
   --  exists, after all resolutions are applied (inheritance, default value
   --  according to project kind, etc.).
   --  Returns whether the result is defined.

   function Attributes
     (Self          : Object;
      Name          : Q_Attribute_Id;
      With_Defaults : Boolean := True;
      With_Config   : Boolean := True)
      return Project.Attribute.Set.Object
     with Pre => Self.Is_Defined;
   --  Retrieve all top-level attributes that have the given name.
   --
   --  Name:          the attribute to retrieve.
   --  With_Defaults: whether default values are considered.
   --  With_Config:   whether attributes defined from the config file are
   --                 considered.
   --
   --  Note:  blob index, if defined in the project, is returned as-is (not
   --         resolved with regard to the source list). Their value may be then
   --         invalid as they are not properly merged with any potential
   --         inherited attribute.
   --  Note2: if With_Defaults is set, then the default values that accept
   --         any index won't be generated.

   function Attributes
     (Self          : Object;
      Pack          : Package_Id             := Project_Level_Scope;
      With_Defaults : Boolean                := True;
      With_Config   : Boolean                := True)
      return Project.Attribute.Set.Object
     with Pre => Self.Is_Defined;
   --  Get the list of attributes

   function Compiler_Prefix (Self : Object; Language : Language_Id)
     return String
     with Pre => Self.Is_Defined;
   --  Returns the compiler prefix for the given language, including the
   --  separator dash, or an empty string if no prefix is defined for
   --  the specified language.

   --  Types

   function Has_Types
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project view has some types defined

   function Types (Self : Object) return Typ.Set.Object
     with Pre  => Self.Is_Defined,
          Post => (if Self.Has_Types then not Types'Result.Is_Empty);
   --  Get the list of all types defined

   --  Variables

   function Has_Variables
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project view has some variables defined

   function Has_Variables
     (Self : Object;
      Pack : Package_Id;
      Name : Optional_Name_Type := No_Name) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the package has some variables defined

   function Variables (Self : Object) return Variable.Set.Object
     with Pre  => Self.Is_Defined,
          Post => (if Self.Has_Variables then not Variables'Result.Is_Empty);
   --  Get the list of all variables defined

   function Variables
     (Self : Object; Pack : Package_Id) return Variable.Set.Object
     with Pre  => Self.Is_Defined,
          Post => (if Self.Has_Variables (Pack)
                   then not Variables'Result.Is_Empty);
   --  Get the list of all variables defined in Pck

   function Variable (Self : Object; Name : Name_Type) return Variable.Object
     with Pre  => Self.Is_Defined and then Self.Has_Variables (Name),
          Post => Variable'Result.Is_Defined;
   --  Returns the variable with the given name

   function Variable
     (Self : Object;
      Pack : Package_Id;
      Name : Name_Type) return Project.Variable.Object
     with Pre  => Self.Is_Defined and then Self.Has_Variables (Pack, Name),
          Post => Variable'Result.Is_Defined;

   function Namespace_Roots (Self : Object) return Set.Object
     with Pre  => Self.Is_Defined and then Self.Kind /= K_Aggregate;
   --  Root of the projects namespace subtree. It is either root aggregated
   --  project view or just root view of the tree. Source simple filenames
   --  have to be unique in the name-space of this subtree.

   --  Packages

   function Has_Package
     (Self           : Object;
      Name           : Package_Id;
      Check_Extended : Boolean := True;
      With_Defaults  : Boolean := True;
      With_Config    : Boolean := True) return Boolean
     with Pre => Self.Is_Defined;
   --  If Name is set to No_Name then return True if the view defined some
   --  packages, otherwise check for the specified package.
   --  Check_Extended: if set, verifies if Self inherits the package from
   --  its hierarchy.
   --  With_Defaults: if set, will match packages that are implicitly defined
   --  via their default attribute values.

   function Packages
     (Self          : Object;
      With_Defaults : Boolean := True;
      With_Config   : Boolean := True) return GPR2.Containers.Package_Id_List
     with Pre  => Self.Is_Defined,
          Inline;
   --  Get the list of packages defined in the project or inherited from the
   --  extended view.
   --  With_Defaults: controls whether packages defined implicitly by
   --  attributes default values are taken into account.

   --  Sources

   function Has_Languages (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project view has languages attribute defined or has
   --  default Languages attribute value. The case where it returns false is a
   --  project prohibited to have the Language attribute, aggregate for
   --  example.

   function Languages (Self : Object) return Containers.Source_Value_List
     with Pre  => Self.Is_Defined;
   --  Returns the languages used on this project

   function Language_Ids (Self : Object) return Containers.Language_Set
     with Pre => Self.Is_Defined;
   --  Returns the languages used by this project as a set of Language id

   function Is_Compilable
     (Self : Object;
      Lang : Language_Id) return Boolean
     with Pre => Self.Is_Defined;
   --  Whether the language has a compiler driver defined for the view

   function View_Db (Self : Object) return GPR2.Build.View_Db.Object
     with Pre => Self.Is_Defined, Inline;
   --  Return the artifacts database for the view. If the view don't have
   --  an object directory, then Undefined is returned.

   function Source_Directories (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns the source dir paths for a given project

   procedure Source_Directories_Walk
     (Self      : Object;
      Source_CB : access procedure
                    (Dir_Index : Natural;
                     Source    : GPR2.Path_Name.Full_Name;
                     Timestamp : Ada.Calendar.Time);
      Dir_CB    : access procedure (Dir_Name : GPR2.Path_Name.Full_Name);
      Messages  : in out GPR2.Log.Object)
     with Pre => Self.Is_Defined;
   --  Walks the source directories of Self and calls Source_CB on every
   --  file found, and Dir_CB on each directory found, if the callbacks are
   --  defined.
   --  Parameters for Source_CB:
   --  * Dir_index : the index of the Source_Dirs list that triggers the call.
   --    It is used to determine visibility of the source in case of
   --    overriding.
   --  * Source: the path to the source.
   --  * Timestamp: the timestamp of the source at the time we found it.

   function Skipped_Sources
     (Self : Object) return Containers.Filename_Source_Reference
     with Pre => Self.Is_Defined;
   --  List of source basenames to ignore when loading the list of sources:
   --  they are mentioned in ignored case statements, so should be skipped so
   --  as to not interfere with the case statement that is selected.
   --  e.g.:
   --  Val := "True";
   --  case Val is
   --     when "True" =>
   --        for Body ("Foo") use "foo__ok.adb";
   --     when "False" =>
   --        for Body ("Foo") use "foo__not_ok.adb";
   --  end case;
   --
   --  In the above example. "foo__not_ok.adb" needs to be skipped.

   function Sources
     (Self            : Object;
      Interface_Only  : Boolean := False;
      Compilable_Only : Boolean := False)
      return Build.Source.Sets.Object
     with Pre => Self.Is_Defined, Inline;
   --  Returns the sources for the view, note that this routine ensure that
   --  the sources are loaded.
   --  Interface_Only: only sources part of a library interface are returned
   --  Compilable_Only: only sources that can be compiled are returned

   function Source_Path
     (Self            : Object;
      Name            : GPR2.Simple_Name;
      Allow_Spec_File : Boolean;
      Allow_Unit_Name : Boolean) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Get full path name corresponding to the given name
   --  name can be the filename with or without body/spec extension
   --  Set allow_spec_file if spec file can also be returned.
   --  Set Allow_Unit_Name if Name may denote a unit, The main part of the
   --   unit is then returned, whatever the value of Allow_Spec_File.
   --  Returns body file when body & spec files are found.

   function Has_Source
     (Self : Object; Filename : GPR2.Simple_Name) return Boolean
     with Pre => Self.Is_Defined;
   --  Return True if source with such filename found in project namespace
   --  subtree.

   function Source
     (Self : Object; Filename : GPR2.Simple_Name)
      return Build.Source.Object
     with Pre => Self.Is_Defined;
   --  Returns source by simple filename. The search is performed in the view
   --  only.
   --  If the source with such simple filename is not found in the subtree,
   --  then GPR2.Project.Source.Undefined is returned.

   function Visible_Source
     (Self      : Object;
      Filename  : GPR2.Simple_Name;
      Ambiguous : out Boolean)
      return Build.Source.Object
     with Pre => Self.Is_Defined, Inline;
   --  Similar to Source but the source is looked up in the complete closure
   --  of Self.

   function Visible_Source
     (Self      : Object;
      Filename  : GPR2.Simple_Name)
      return Build.Source.Object
     with Pre => Self.Is_Defined, Inline;
   --  Same as above but ignores the Ambiguous status

   function Visible_Source
     (Self : Object;
      Path : GPR2.Path_Name.Object)
      return Build.Source.Object
     with Pre => Self.Is_Defined, Inline;
   --  Similar to Source but the source is looked up in the complete closure
   --  of Self, using its full path.
   --  The source may not be naturally visible though, since using the full
   --  path its basename may be overloaded so a query with the base name would
   --  then give a different result.

   function Visible_Sources
     (Self : Object) return GPR2.Build.Source.Sets.Object;
   --  Return all sources visible by Self

   function Interface_Units
     (Self : Object) return GPR2.Containers.Unit_Name_To_Sloc.Map
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Return a map of interface units defined by the view to their
   --  definition in the project file.

   function Interface_Sources
     (Self : Object) return GPR2.Containers.Source_Path_To_Sloc.Map
     with Pre => Self.Is_Defined;
   --  Return a map of interface sources defined by the view to their
   --  definition in the project file.

   function Interface_Closure
     (Self : Object) return GPR2.Build.Compilation_Unit.Maps.Map
     with Pre => Self.Is_Defined;
   --  Return the set of Ada units that are part of the interface of the
   --  project. If the project is a library, the Interfaces or
   --  the Library_Interface attributes are checked. Otherwise, only the
   --  Interfaces attribute is checked.
   --  If no interface is defined for the library, then the returned map is
   --  empty.

   --  Some common attributes redefined here and when some pathname are
   --  relative to the view, the proper value is returned. Following
   --  routines are for internal use only and convert from a View unique Id.

   --  Units

   function Filename_For_Unit
     (Self      : Object;
      Unit_Name : Name_Type;
      Kind      : Valid_Unit_Kind) return Simple_Name
     with Pre => Self.Is_Defined and then Self.Kind in With_Source_Dirs_Kind;
   --  Given an Unit_Name and a Kind for this unit returns the Simple_Name of
   --  an existing compilation unit from Self or the Theorical Simple_Name of
   --  the unit if it does not exist.

   function Units
     (Self                  : Object;
      With_Externally_Built : Boolean := False)
      return GPR2.Build.Compilation_Unit.Maps.Map
     with Pre => Self.Is_Defined and then Self.Is_Namespace_Root;
   --  Returns all the known units for the view. Note that the list of units
   --  is populated only when Update_Sources is called.
   --  If With_Externally_Built is unset, then units defined by externally
   --  built views won't be returned.

   function Unit
     (Self : Object; Name : Name_Type) return Build.Compilation_Unit.Object
     with Pre => Self.Is_Defined and then Self.Is_Namespace_Root;
   --  Lookup the compilation unit "Name" in the subtree whose root view is
   --  Self.

   function Unit_Part
     (Self : Object; Name : Name_Type; Is_Spec : Boolean)
      return Build.Compilation_Unit.Unit_Location
     with Pre => Self.Is_Defined and then Self.Is_Namespace_Root;
   --  Lookup the specified compilation unit part, and return the
   --  corresponding source file location.

   function Own_Units
     (Self : Object) return GPR2.Build.Compilation_Unit.Maps.Map
     with Pre => Self.Is_Defined;
   --  Returns all the units owned by the view. Note that the list of units
   --  is populated only when Update_Sources is called.

   function Own_Unit
     (Self : Object; Name : Name_Type) return Build.Compilation_Unit.Object
     with Pre => Self.Is_Defined;
   --  Lookup the compilation unit "Name", owned by Self (meaning that the main
   --  unit of the compilation unit belongs to Self.

   function Is_Abstract (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project is an abstract project, or if it is a
   --  regular project that defines explicitly at least:
   --  * either an empty list of languages
   --  * an empty list of source directories
   --  Note: if one of the above attributes is set to a non-empty list while
   --  the other is empty, then the view is not considered abstract anymore.

   function Is_Externally_Built (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project is externally built

   function Is_Runtime (Self : Object) return Boolean;
   --  Returns True if the project describes the runtime

   function Has_Mains (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project has some mains defined

   function Mains
     (Self : Object) return GPR2.Build.Compilation_Unit.Unit_Location_Vector
     with Pre  => Self.Is_Defined;
   --  returns the list of main bodies

   function Suffixed_Simple_Name
     (Self : Object;
      Name : String;
      Lang : Language_Id := Ada_Language) return Simple_Name;
   --  If the provided name contains any standard suffix (.ada, .adb, .c)
   --  or any declared convention in the Naming package then it returns the
   --  value as is.
   --  Otherwise, the suffix for the specified language, "Lang",
   --  is applied to the name. If no suffixes are defined for this language,
   --  it returns the value as is.

   function Executables (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre  => Self.Is_Defined,
          Post => not Self.Has_Mains or else Executables'Result.Length > 0;
   --  Returns the mains's binary full pathname

   function Library_Name (Self : Object) return Simple_Name
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns the library name

   function Library_Kind (Self : Object) return Name_Type
     with Pre  => Self.Is_Defined
                  and then Self.Kind in GPR2.Library_Kind,
          Post => Self.Has_Attribute (PRA.Library_Kind)
                  or else Library_Kind'Result = "static";
   --  Returns the library kind, "static" if the corresponding attribute is not
   --  defined.

   function Is_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project is library

   function Is_Static_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns True if the library is a static one, so either static or
   --  static-pic.

   function Is_Shared_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns True if the library is a shared one

   function Has_Library_Interface (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns whether the optional library interface attribute is defined

   function Has_Interfaces (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether the optional interfaces attribute is defined

   function Has_Any_Interfaces (Self : Object) return Boolean
   with
     Pre  => Self.Is_Defined,
     Post =>
       Has_Any_Interfaces'Result = Self.Has_Interfaces
         or else (Self.Kind in GPR2.Library_Kind
                  and then Self.Has_Library_Interface);
   --  Returns whether any interface is defined either using the
   --  Library_Interface or Interfaces attribute.

   function Has_Library_Version (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns whether the optional library version name is defined

   function Library_Filename
     (Self            : Object;
      Without_Version : Boolean := False) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns the actual file name for the library.
   --  If Without_Version is set, then the attribute Library_Version attribute
   --  is ignored.

   function Library_Filename_Variants
     (Self : Object) return GPR2.Containers.Filename_Set
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns the list of symbolic links that will point to Library_Filename
   --  on non-windows platforms. This is non-empty only when Library_Version is
   --  specified and will follow the .so schema installation in case
   --  Library_Version complies with this naming schema.
   --  Only simple names are returned here.

   function Library_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined
                 and then Self.Kind in GPR2.Library_Kind
                 and then Self.Has_Library_Version;
   --  Returns the library version filename

   function Library_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns the library directory, note that this may be different than
   --  getting the Library_Dir attribute value as the result here is always
   --  a path-name with proper resolution for relative directory specification.

   function Library_Ali_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns the library directory, note that this may be different than
   --  getting the Library_Ali_Dir attribute value as the result here is always
   --  a path-name with proper resolution for relative directory specification.

   function Has_Library_Src_Directory (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Whether a Library_Src_Dir attribute is defined for Self

   function Library_Src_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Has_Library_Src_Directory;
   --  Defines the location (absolute or relative to the project directory)
   --  where the sources of the interface units are copied at installation
   --  time.

   function Library_Standalone (Self : Object) return Standalone_Library_Kind
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns the kind for the standalone library

   function Is_Library_Standalone (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Kind in GPR2.Library_Kind;
   --  Returns whether the library is standalone

   function Include_Path
     (Self : Object; Language : Language_Id) return GPR2.Path_Name.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns the list of source paths visible for Self for a given language

   function Object_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre =>
       Self.Is_Defined
       and then Self.Kind in With_Object_Dir_Kind;
   --  As above but for the Object_Dir attribute

   function Has_Source_Subdirectory (Self : Object) return Boolean
     with Pre => Self.Is_Defined
                 and then Self.Kind
                   not in K_Configuration | K_Abstract | Aggregate_Kind;
   --  Returns True if a Src_Subdires is setup in the corresponding tree

   function Source_Subdirectory (Self : Object) return GPR2.Path_Name.Object
     with Pre  => Self.Is_Defined
                  and then Self.Has_Source_Subdirectory
                  and then Self.Kind
                    not in K_Configuration | K_Abstract | Aggregate_Kind,
          Post => Source_Subdirectory'Result.Is_Defined;
   --  Returns source subdirectory in object directory if parameter Src_Subdirs
   --  is defined on Tree.Load call.

   function Executable_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre =>
       Self.Is_Defined
       and then Self.Kind in K_Standard | K_Library | K_Aggregate_Library;
   --  As above but for the Exec_Dir attribute

   function Executable_Suffix (Self : Object) return Filename_Optional
     with Pre => Self.Is_Defined;
   --  Returns executable suffix for this project

   function Executable
     (Self   : Object;
      Source : Simple_Name;
      At_Pos : Unit_Index) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Returns the full pathname of the main executable for the given main

   function Main
     (Self       : Object;
      Executable : Simple_Name) return Build.Compilation_Unit.Unit_Location
     with Pre  => Self.Is_Defined;
   --  Returns the body unit corresponding to the given executable result

   --  To ease the use of some attributes (some have synonyms for example)
   --  below are direct access to them.

   function Has_Spec_Suffix
     (Self     : Object;
      Language : Language_Id) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True is package naming Self contains a Spec_Suffix attribute

   function Spec_Suffix
     (Self     : Object;
      Language : Language_Id) return Project.Attribute.Object
     with Pre  => Self.Is_Defined
                  and then Self.Has_Spec_Suffix (Language),
          Post => Spec_Suffix'Result.Is_Defined;
   --  Handles Spec_Suffix and Specification_Suffix

   function Has_Archive_Builder
     (Self : Object) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True if Self contains an Archive_Builder attribute

   function Archive_Builder (Self : Object) return Project.Attribute.Object
     with Pre  => Self.Is_Defined
                  and then Self.Has_Archive_Builder,
          Post => Archive_Builder'Result.Is_Defined;
   --  Handles Archive_Builder

   function Has_Body_Suffix
     (Self     : Object;
      Language : Language_Id) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True is package naming Self contains a Body_Suffix attribute

   function Body_Suffix
     (Self     : Object;
      Language : Language_Id) return Project.Attribute.Object
     with Pre  => Self.Is_Defined
                  and then Self.Has_Body_Suffix (Language),
          Post => Body_Suffix'Result.Is_Defined;
   --  Handles Body_Suffix and Implementation_Suffix

   function Has_Separate_Suffix
     (Self : Object) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True is package naming Self contains a Separate_Suffix attribute

   function Separate_Suffix
     (Self : Object) return Project.Attribute.Object
     with Pre  => Self.Is_Defined
                  and then Self.Has_Separate_Suffix,
          Post => Separate_Suffix'Result.Is_Defined;
   --  Handles Separate_Suffix

   function Has_Specification
     (Self : Object;
      Unit : Value_Type) return Boolean
     with Pre  => Self.Is_Defined;
   --  Return True if package Naming Self has an attribute Specification or
   --  Spec defined for the given unit.

   function Specification
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object
     with Pre  => Self.Is_Defined
                  and then Self.Has_Specification (Unit),
          Post => Specification'Result.Is_Defined;
   --  Handles Spec, Specification, this is only defined for the Ada language

   function Has_Implementation
     (Self : Object;
      Unit : Value_Type) return Boolean
     with Pre  => Self.Is_Defined;
   --  Return True if package Naming Self has an attribute Implementation or
   --  Body defined for the given unit.

   function Implementation
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object
     with Pre  => Self.Is_Defined
               and then Self.Has_Implementation (Unit),
       Post => Implementation'Result.Is_Defined;
   --  Handles Body, Implementation, this is only defined for the Ada language

   function Casing (Self : Object) return Project.Attribute.Object
     with Pre  => Self.Is_Defined,
          Post => Casing'Result.Is_Defined;

   function Raw_Attributes
     (Self : Object;
      Pack : Package_Id) return Project.Attribute.Set.Object
     with Inline;
   --  Internal function used to retrieve the unprocessed list of attributes
   --  defined in a package.

   function Apply_Root_And_Subdirs
     (Self : Object; Dir_Attr : Q_Attribute_Id) return GPR2.Path_Name.Object
     with Pre => (Dir_Attr.Pack = Project_Level_Scope
                  and then Dir_Attr in PRA.Object_Dir | PRA.Library_Ali_Dir |
                                       PRA.Library_Dir | PRA.Exec_Dir |
                                       PRA.Library_Src_Dir);
   --  Apply project path and subdir option for library, object and executable
   --  directories defined in attribute Dir_Attr.
   --  Internal use: the accessors Object_Directory, Executable_Directory and
   --  so on already handle the out-of-tree builds and the subdirs.

   function Check_Parent (Self : Object; Parent : out Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True and set Parent if Self has parent view, otherwise returns
   --  False.

   procedure Check_Mains
     (Self     : Object;
      Messages : in out Log.Object)
     with Pre => Self.Is_Defined and then Self.Is_Namespace_Root;
   --  Check the validity of the Main attribute values and fill appropriate
   --  error/warning in case they are not valid.

private

   use View_Base_Internal;

   type Object is new Definition_References.Ref with
     null record;

   function Pack
     (Self : Object;
      Name : Package_Id) return Pack_Internal.Object;
   --  Get the package with the given Name

   Undefined : constant Object :=
                 (Definition_References.Null_Ref with null record);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function "<" (Left, Right : Object) return Boolean is
     (Left.Get.Id < Right.Get.Id);

   function Is_Library (Self : Object) return Boolean is
     (Self.Kind in GPR2.Library_Kind
      and then not Self.Is_Aggregated_In_Library);

   function Library_Name (Self : Object) return Simple_Name is
     (Simple_Name (Self.Attribute (PRA.Library_Name).Value.Text));

   function Has_Library_Version (Self : Object) return Boolean is
     (Self.Has_Attribute (PRA.Library_Version));

   function Has_Library_Interface (Self : Object) return Boolean is
     (Self.Has_Attribute (PRA.Library_Interface)
      and then not Self.Attribute (PRA.Library_Interface).Values.Is_Empty);

   function Has_Interfaces (Self : Object) return Boolean is
     (Self.Has_Attribute (PRA.Interfaces)
      and then not Self.Attribute (PRA.Interfaces).Values.Is_Empty);

   function Has_Any_Interfaces (Self : Object) return Boolean
   is ((Self.Is_Library and then Self.Has_Library_Interface)
       or else Self.Has_Interfaces);

   function Is_Abstract (Self : Object) return Boolean is
     (Self.Kind = K_Abstract);

   function Is_Library_Standalone (Self : Object) return Boolean is
      (Self.Library_Standalone /= No);

   function Is_Shared_Library (Self : Object) return Boolean is
     (not Self.Is_Static_Library);

   function Dir_Name (Self : Object) return GPR2.Path_Name.Object is
     (Self.Get.Path);

   --  Naming package accessor

   package PA  renames Project.Attribute;
   package PAI renames Project.Attribute_Index;

   function Has_Separate_Suffix
     (Self : Object) return Boolean
   is (Self.Has_Attribute (PRA.Naming.Separate_Suffix));

   function Separate_Suffix
     (Self : Object) return Project.Attribute.Object
   is (Self.Attribute (PRA.Naming.Separate_Suffix));

   function Has_Spec_Suffix
     (Self     : Object;
      Language : Language_Id) return Boolean
   is (Self.Has_Attribute (PRA.Naming.Spec_Suffix, PAI.Create (Language)));

   function Has_Archive_Builder
     (Self : Object) return Boolean
   is (Self.Has_Attribute (PRA.Archive_Builder));

   function Archive_Builder (Self : Object) return Project.Attribute.Object is
     (Self.Attribute (PRA.Archive_Builder));

   function Spec_Suffix
     (Self     : Object;
      Language : Language_Id) return Project.Attribute.Object
   is (Self.Attribute (PRA.Naming.Spec_Suffix, PAI.Create (Language)));

   function Has_Body_Suffix
     (Self     : Object;
      Language : Language_Id) return Boolean
   is (Self.Has_Attribute (PRA.Naming.Body_Suffix, PAI.Create (Language)));

   function Body_Suffix
     (Self     : Object;
      Language : Language_Id) return Project.Attribute.Object
   is (Self.Attribute (PRA.Naming.Body_Suffix, PAI.Create (Language)));

   function Has_Implementation
     (Self : Object;
      Unit : Value_Type) return Boolean
   is (Self.Has_Attribute (PRA.Naming.Body_N, PAI.Create (Unit)));

   function Implementation
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object
   is (Self.Attribute (PRA.Naming.Body_N, PAI.Create (Unit)));

   function Has_Specification
     (Self : Object;
      Unit : Value_Type) return Boolean
   is (Self.Has_Attribute (PRA.Naming.Spec, PAI.Create (Unit)));

   function Specification
     (Self : Object;
      Unit : Value_Type) return Project.Attribute.Object
   is (Self.Attribute (PRA.Naming.Spec, PAI.Create (Unit)));

   function Casing
     (Self : Object) return Project.Attribute.Object
   is (Self.Attribute (PRA.Naming.Casing));

   function Raw_Attributes
     (Self : Object;
      Pack : Package_Id) return Project.Attribute.Set.Object
   is (Self.Pack (Pack).Attrs);

   function Has_Library_Src_Directory (Self : Object) return Boolean is
     (Self.Is_Library
      and then Self.Is_Library_Standalone
      and then Self.Has_Attribute (PRA.Library_Src_Dir));

end GPR2.Project.View;
