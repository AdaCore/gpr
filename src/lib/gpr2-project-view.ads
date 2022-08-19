--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  A specific view of a project as seen in a tree. A project view is retrieved
--  from a tree, see Project.Tree package. A project view differ from a
--  standard project object from the parser as it gives access to the actual
--  attributes, variables and packages values depending on the current context
--  for the corresponding tree. It also gives the sources for the views and
--  this include sources from extended project or aggregated project if needed.

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name.Set;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.Unit_Info.Set;
with GPR2.Source_Reference.Value;
with GPR2.Unit;
with GPR2.View_Ids;

limited with GPR2.Project.Source.Set;
limited with GPR2.Project.Tree;
limited with GPR2.Project.View.Set;

private with GPR2.Project.Pack;
private with GPR2.Project.Registry.Pack;

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

   function Is_Extension_Of (Self : Object; View : Object) return Boolean;
   --  Returns whether Self extends View

   function Is_Extended_By (Self : Object; View : Object) return Boolean
     with Pre => Self.Is_Defined and then View.Is_Defined;
   --  Returns whether Self is extended by View

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

   function Qualifier (Self : Object) return Project_Kind
     with Pre => Self.Is_Defined;
   --  The qualifier as specified in the project file

   function Kind (Self : Object) return Project_Kind
     with Pre  => Self.Is_Defined,
          Post => Kind'Result = Self.Qualifier
                  or else Self.Qualifier = K_Standard;
   --  The actual kind of the project file. This may be different if the
   --  Qualifier is not specified.

   function Tree (Self : Object) return not null access Project.Tree.Object
     with Pre => Self.Is_Defined;
   --  Returns the corresponding project tree

   function Signature (Self : Object) return Context.Binary_Signature;
   --  Returns the signature for the view

   function Has_Imports (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project has some imports

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
   --  all this will thus return the project that is explicitely extended.

   function Is_Extended (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the view is extended by another project

   function Extending (Self : Object) return Object
     with Pre  => Self.Is_Defined and then Self.Is_Extended,
          Post => Extending'Result.Is_Extending;
   --  Return the extending view

   function Is_Main
     (Self : Object; Source : Project.Source.Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the source is the main unit of the view

   function Aggregated (Self : Object) return Set.Object
     with Pre => Self.Is_Defined and then Self.Kind in Aggregate_Kind;

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
     with Pre  => Self.Is_Defined;

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

   function Attribute_Location
     (Self  : Object;
      Name  : Q_Attribute_Id;
      Index : Attribute_Index.Object := Attribute_Index.Undefined)
      return Source_Reference.Object'Class
     with
       Pre => Self.Is_Defined;
   --  Returns the source location of the attribute definition in the view if
   --  defined, or the view's location (e.g. path_name, 0, 0) if not.
   --  To be used in particular when generating Messages.

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

   function Typ (Self : Object; Name : Name_Type) return Typ.Object
     with Pre  => Self.Is_Defined and then Self.Has_Types (Name),
          Post => Typ'Result.Is_Defined;
   --  Returns the type with the given name

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

   function Namespace_Root (Self : Object) return Object
     with Pre  => Self.Is_Defined,
          Post => Namespace_Root'Result.Is_Defined;
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

   function Has_Language (Self : Object; Name : Name_Type) return Boolean
     with Pre => Self.Is_Defined;
   --  Whether Name is a language used by Self

   function Has_Languages (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project view has languages attribute defined or has
   --  default Languages attribute value. The case where it returns false is a
   --  project prohibited to have the Language attribute, aggregate for
   --  example.

   function Languages (Self : Object) return Containers.Source_Value_List
     with Pre  => Self.Is_Defined;
   --  Returns the languages used on this project

   function Source_Directories (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre => Self.Is_Defined
                 and then Self.Qualifier in K_Standard | K_Library;
   --  Returns the source dir paths for a given project

   function Has_Sources (Self : Object) return Boolean
     with Pre  => Self.Is_Defined,
          Post => (if Self.Kind in K_Abstract | K_Aggregate
                   then not Has_Sources'Result);
   --  Returns true if the project view has some sources

   function Sources
     (Self            : Object;
      Interface_Only  : Boolean := False;
      Compilable_Only : Boolean := False)
      return Project.Source.Set.Object
     with Pre => Self.Is_Defined, Inline;
   --  Returns the sources for the view, note that this routine ensure that
   --  the sources are loaded.
   --  Interface_Only: only sources part of a library interface are returned
   --  Compilable_Only: only sources that can be compiled are returned

   function Source
     (Self : Object; File : GPR2.Path_Name.Object) return Project.Source.Object
     with Pre => Self.Is_Defined;
   --  Get project source object corresponding to the given File

   function Source_Path
     (Self : Object; Filename : GPR2.Simple_Name) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Get full path name corresponding to the given filename

   function Source_Path
     (Self            : Object;
      Name            : GPR2.Simple_Name;
      Allow_Spec_File : Boolean;
      Allow_Unit_Name : Boolean) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Get full path name corresponding to the given name
   --  name can be the filename with or without body/spec extension
   --  Set allow_spec_file if spec file can also be returned.
   --  Set allow_unit_name if name can also be a unit name.
   --  Returns body file when body & spec files are found.

   function Has_Source
     (Self : Object; Filename : GPR2.Simple_Name) return Boolean
     with Pre => Self.Is_Defined;
   --  Return True if source with such filename found in project namespace
   --  subtree.

   function Source
     (Self : Object; Filename : GPR2.Simple_Name) return Project.Source.Object
     with Pre => Self.Is_Defined;
   --  Returns source by simple filename. The search is performed in the view
   --  only.
   --  If the source with such simple filename is not found in the subtree,
   --  then GPR2.Project.Source.Undefined is returned.

   function Check_Source
     (Self     : Object;
      Filename : GPR2.Simple_Name;
      Result   : out Project.Source.Constant_Access) return Boolean
     with Pre => Self.Is_Defined;
   function Check_Source
     (Self     : Object;
      Filename : GPR2.Simple_Name;
      Result   : in out Project.Source.Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Get the source by simple filename from the subtree of the View.
   --  Return True on success and set Result.
   --  Return False if source not found and keep Result untouched.
   --  This routine is faster than using Has_Source and Source above as
   --  avoiding one access to the underlying structure.

   function Check_Source_Unit
     (Self   : Object;
      Unit   : GPR2.Unit.Object;
      Result : in out Project.Source.Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Get the source object by the unit from the same projects subtree where
   --  the View is.
   --  Return True on success and set Result.
   --  Return False if source not found and remain Result untouched.

   function Check_Parent (Self : Object; Parent : out Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True and set Parent if Self has parent view, otherwise returns
   --  False.

   procedure Invalidate_Sources (Self : in out Object)
     with Pre => Self.Is_Defined;
   --  Invalidate the sources for the view. This means that the Sources routine
   --  above will have to recompute the proper sources list for the view. This
   --  is needed when some sources are added or removed from the view.

   --  Some common attributes redefined here and when some pathname are
   --  relative to the view, the proper value is returned. Following
   --  routines are for internal use only and convert from a View unique Id.

   --  Units

   function Units (Self : Object) return Unit_Info.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns all the known units for the view. Note that the list of units
   --  is populated only when Update_Sources is called.

   function Unit
     (Self : Object; Name : Name_Type) return Unit_Info.Object
     with Pre => Self.Is_Defined;

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

   function Mains (Self : Object) return GPR2.Unit.Source_Unit_Vectors.Vector
     with Pre  => Self.Is_Defined,
          Post => not Self.Has_Mains or else Mains'Result.Length > 0;
   --  returns the list of main bodies

   function Executables (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre  => Self.Is_Defined,
          Post => not Self.Has_Mains or else Executables'Result.Length > 0;
   --  Returns the mains's binary full pathname

   function Library_Name (Self : Object) return Simple_Name
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the library name

   function Library_Kind (Self : Object) return Name_Type
     with Pre  => Self.Is_Defined
                  and then Self.Is_Library,
          Post => Self.Has_Attribute (PRA.Library_Kind)
                  or else Library_Kind'Result = "static";
   --  Returns the library kind, "static" if the corresponding attribute is not
   --  defined.

   function Is_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project is library

   function Is_Static_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns True if the library is a static one, so either static or
   --  static-pic.

   function Is_Shared_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns True if the library is a shared one

   function Has_Library_Interface (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Retruns whether the optional library interface attribute is defined

   function Has_Interfaces (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns whether the optional interfaces attribute is defined

   function Has_Any_Interfaces (Self : Object) return Boolean
     with Pre  => Self.Is_Defined and then Self.Is_Library,
          Post => Has_Any_Interfaces'Result =
                    Self.Has_Interfaces or else Self.Has_Library_Interface;
   --  Returns whether any interface is defined either using the
   --  Library_Interface or Interfaces attribute.

   function Has_Library_Version (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns whether the optional library version name is defined

   function Library_Major_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined
                 and then Self.Is_Library and then not Self.Is_Static_Library
                 and then Self.Has_Library_Version;
   --  Returns the library major name if it exists. That is, if the project
   --  Library_Version exists and is set to libxyz.so.1.2 for example then the
   --  returned value is libxyz.so.1. If no major version is computable an
   --  undefined path-name is returned.

   function Library_Filename (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the actual file name for the library

   function Library_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined
                 and then Self.Is_Library
                 and then Self.Has_Library_Version;
   --  Returns the library version filename

   function Library_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the library directory, note that this may be different than
   --  getting the Library_Dir attribute value as the result here is always
   --  a path-name with proper resolution for relative directory specification.

   function Library_Ali_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the library directory, note that this may be different than
   --  getting the Library_Ali_Dir attribute value as the result here is always
   --  a path-name with proper resolution for relative directory specification.

   function Library_Src_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Defines the location (absolute or relative to the project directory)
   --  where the sources of the interface units are copied at installation
   --  time.

   function Library_Standalone (Self : Object) return Standalone_Library_Kind
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the kind for the standalone library

   function Is_Library_Standalone (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns whether the library is standalone

   function Object_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre =>
       Self.Is_Defined
       and then Self.Kind not in K_Configuration | K_Abstract;
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

   function Object_Artifact_Extensions
     (Self : Object; Language : Language_Id) return Containers.Value_Set
     with Pre => Self.Is_Defined;
   --  Returns set of object artefacts extensions for the cleanup

   function Source_Artifact_Extensions
     (Self : Object; Language : Language_Id) return Containers.Value_Set
     with Pre => Self.Is_Defined;
   --  Returns set of source artefacts extensions for the cleanup

   function Binder_Artifacts
     (Self     : Object;
      Name     : Simple_Name;
      Language : Language_Id := No_Language)
      return GPR2.Path_Name.Set.Object
     with Pre => Self.Is_Defined
                 and then (not Self.Is_Library
                           or else Self.Library_Name = Name
                           or else Self.Is_Aggregated_In_Library);
   --  Returns binder artifact files from main procedure name for standard
   --  project or from library name for library project.

   function Artifacts (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns artifact files taken from Artifacts_In_Object_Dir and
   --  Artifacts_In_Exec_Dir attributes.

   function Executable
     (Self   : Object;
      Source : Simple_Name;
      At_Pos : Unit_Index) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;
   --  Returns the full pathname of the main executable for the given main

   function Main
     (Self       : Object;
      Executable : Simple_Name) return GPR2.Unit.Source_Unit_Identifier
     with Pre  => Self.Is_Defined;
   --  Returns the body unit corresponding to the given executable result

   procedure Reindex_Unit (Self : Object; From, To : Name_Type)
     with Pre  => Self.Is_Defined;
   --  Change name of unit in unit index used to get unit info by unit name

   procedure Hide_Unit_Body (Self : Object; Unit : Name_Type)
     with Pre  => Self.Is_Defined;
   --  Remove unit body from unit info index

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

   function Raw_Attributes
     (Self : Object;
      Pack : Package_Id) return Project.Attribute.Set.Object
     with Inline;
   --  Internal function used to retrieve the unprocessed list of attributes
   --  defined in a package.

private

   type Object is new Definition_References.Ref with null record;

   function Clean_Attribute_List
     (Self     : Object;
      Name     : Q_Attribute_Id;
      Language : Language_Id) return Containers.Value_Set;
   --  Returns union of the attribute lists of the Clean packages from the
   --  configuration view, extending view if it exists and Self view.

   function Pack
     (Self : Object;
      Name : Package_Id) return Project.Pack.Object;
   --  Get the package with the given Name

   Undefined : constant Object :=
                 (Definition_References.Null_Ref with null record);

   function Object_Artifact_Extensions
     (Self : Object; Language : Language_Id) return Containers.Value_Set
   is
     (Self.Clean_Attribute_List (PRA.Clean.Object_Artifact_Extensions,
                                 Language));

   function Source_Artifact_Extensions
     (Self : Object; Language : Language_Id) return Containers.Value_Set
   is
     (Self.Clean_Attribute_List (PRA.Clean.Source_Artifact_Extensions,
                                 Language));

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function "<" (Left, Right : Object) return Boolean is
     (Left.Get.Id < Right.Get.Id);

   function Is_Library (Self : Object) return Boolean is
     (Self.Kind in K_Library | K_Aggregate_Library);

   function Library_Name (Self : Object) return Simple_Name is
     (Simple_Name (Self.Attribute (PRA.Library_Name).Value.Text));

   function Has_Library_Version (Self : Object) return Boolean is
     (Self.Has_Attribute (PRA.Library_Version));

   function Has_Library_Interface (Self : Object) return Boolean is
     (Self.Has_Attribute (PRA.Library_Interface));

   function Has_Interfaces (Self : Object) return Boolean is
     (Self.Has_Attribute (PRA.Interfaces)
      and then not Self.Attribute (PRA.Interfaces).Values.Is_Empty);

   function Has_Any_Interfaces (Self : Object) return Boolean is
     (Self.Has_Library_Interface or else Self.Has_Interfaces);

   function Is_Abstract (Self : Object) return Boolean is
     (Self.Kind = K_Abstract);

   function Is_Library_Standalone (Self : Object) return Boolean is
      (Self.Library_Standalone /= No);

   function Dir_Name (Self : Object) return GPR2.Path_Name.Object is
     (Self.Get.Path);

   --  Naming package accessor

   package PA  renames Project.Attribute;
   package PRP renames Project.Registry.Pack;
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

   function Raw_Attributes
     (Self : Object;
      Pack : Package_Id) return Project.Attribute.Set.Object
   is (Self.Pack (Pack).Attrs);

end GPR2.Project.View;
