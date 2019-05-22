------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  A specific view of a project as seen in a tree. A project view is retrieved
--  from a tree, see Project.Tree package. A project view differ from a
--  standard project object from the parser as it gives access to the actual
--  attributes, variables and packages values depending on the current context
--  for the corresponding tree. It also gives the sources for the views and
--  this include sources from extended project or aggregated project if needed.

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;

limited with GPR2.Project.Source.Set;
limited with GPR2.Project.Tree;
limited with GPR2.Project.View.Set;
limited with GPR2.Unit.Set;

package GPR2.Project.View is

   use type Containers.Count_Type;
   use type Context.Object;
   use type Pack.Object;

   type Object is tagged private;

   subtype Project_View is Object;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function "<" (Left, Right : View.Object) return Boolean;
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
     (Self      : Object;
      Recursive : Boolean := False) return GPR2.Project.View.Set.Object
     with Pre => Self.Is_Defined and then Self.Has_Imports;
   --  Returns all imported project views

   function Is_Extending (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project is extending another project

   function Is_Extending_All (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the project is extending all another project

   function Extended (Self : Object) return Object
     with Pre => Self.Is_Defined and then Self.Is_Extending;
   --  Returns the extended project

   function Is_Extended (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the view is extended by another project

   function Aggregated (Self : Object) return GPR2.Project.View.Set.Object
     with Pre => Self.Is_Defined and then Self.Kind in Aggregate_Kind;

   function Aggregate (Self : Object) return GPR2.Project.View.Object
     with Pre  => Self.Is_Defined and then Self.Is_Aggregated,
          Post => Aggregate'Result.Kind in Aggregate_Kind;

   function Is_Aggregated (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self is part of an aggregate project

   function Is_Aggregated_In_Library (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self is part of an aggregate library project

   function View_For (Self : Object; Name : Name_Type) return View.Object
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

   --  Attributes

   function Has_Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type         := No_Value) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project view has some attributes defined. If Name
   --  and/or Index are set it returns True if an attribute with the given
   --  Name and/or Index is defined.

   function Check_Attribute
     (Self   : Object;
      Name   : Name_Type;
      Index  : Value_Type := No_Value;
      Result : out Attribute.Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True and set Result to attribute if attribute exists or has
   --  default value, returns False and set Result to Undefined otherwise.

   function Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type := No_Value) return Attribute.Set.Object
     with Post =>
       (if Self.Has_Attributes (Name) then not Attributes'Result.Is_Empty);
   --  Get the list of attributes, possibly an empty list if it does not
   --  contain attributes or if Name and Index does not match any attribute.

   function Attribute
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := No_Value) return Attribute.Object
     with
       Pre =>
         Self.Is_Defined
         and then Self.Has_Attributes (Name, Index)
         and then Self.Attributes (Name, Index).Length = 1;
   --  Returns the Attribute with the given Name and possibly Index

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

   function Variables (Self : Object) return Variable.Set.Object
     with Pre  => Self.Is_Defined,
          Post => (if Self.Has_Variables then not Variables'Result.Is_Empty);
   --  Get the list of all variables defined

   function Variable (Self : Object; Name : Name_Type) return Variable.Object
     with Pre  => Self.Is_Defined and then Self.Has_Variables (Name),
          Post => Variable'Result.Is_Defined;
   --  Returns the variable with the given name

   --  Packages

   function Has_Packages
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project view has some packages defined

   function Packages (Self : Object) return Pack.Set.Object
     with Pre  => Self.Is_Defined and then Self.Has_Packages,
          Post => Packages'Result.Length > 0;
   --  Get the list of packages defined in the project

   function Pack (Self : Object; Name : Name_Type) return Pack.Object
     with Pre  => Self.Is_Defined and then Self.Has_Packages (Name),
          Post => Pack'Result.Is_Defined;
   --  Get the package with the given Name

   function Naming_Package (Self : Object) return Project.Pack.Object
     with Pre  => Self.Is_Defined,
          Post => Naming_Package'Result.Is_Defined;
   --  Returns the Naming package for the current view. This is either
   --  the view Naming package, the project's tree Naming package from the
   --  loaded configuration project if any and finally the default Naming
   --  package.

   --  Sources

   function Has_Languages (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project view has languages attribute defined or has
   --  default Languages attribute value. The case where it returns false is a
   --  project prohibited to have the Language attribute, aggregate for
   --  example.

   function Languages (Self : Object) return Containers.Source_Value_List
     with Pre => Self.Is_Defined and then Self.Has_Languages;
   --  Returns the languages used on this project, this is not necessary the
   --  content of the Languages attribute as if not defined it returns the
   --  default language Ada. But the languages attribute can be set to the
   --  empty list (no language defined).

   function Source_Directories (Self : Object) return Project.Attribute.Object
     with Pre => Self.Is_Defined
                 and then Self.Qualifier not in K_Aggregate | K_Abstract;
   --  Returns the sources dirs for the project view. This is only defined for
   --  project having sources. If not defined in the project itself, the view
   --  does have the project directory has source dir.

   function Has_Sources (Self : Object) return Boolean
     with Pre  => Self.Is_Defined,
          Post => (if Self.Kind = K_Abstract then not Has_Sources'Result);
   --  Returns true if the project view has some sources

   type Source_Kind is (K_All, K_Interface_Only, K_Not_Interface);

   function Sources
     (Self        : Object;
      Filter      : Source_Kind := K_All;
      Need_Update : Boolean := True) return Project.Source.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns all the sources for the view, note that this routine ensure that
   --  the current sources are up-to-date by calling Update_Sources below.

   function Source
     (Self        : Object;
      File        : GPR2.Path_Name.Object;
      Need_Update : Boolean := True) return Project.Source.Object
     with Pre => Self.Is_Defined;
   --  Get project source object corresponding to the given File

   procedure Invalidate_Sources (Self : in out Object)
     with Pre => Self.Is_Defined;
   --  Invalidate the sources for the view. This means that the Sources routine
   --  above will have to recompute the proper sources list for the view. This
   --  is needed when some sources are added or removed from the view.

   --  Some common attributes redefined here and when some pathname are
   --  relative to the view, the proper value is returned. Following
   --  routines are for internal use only and convert from a View unique Id.

   --  Units

   function Units (Self        : Object;
                   Need_Update : Boolean := True) return GPR2.Unit.Set.Object
     with Pre => Self.Is_Defined;
   --  Returns all the units for the view, note that this routine ensure that
   --  the current sources and units are up-to-date by calling Update_Sources.

   function Unit (Self         : Object;
                  Name         : Name_Type;
                  Need_Update  : Boolean := True) return GPR2.Unit.Object
     with Pre => Self.Is_Defined;

   function Is_Externally_Built (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project is externally built

   function Has_Mains (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns true if the project has some mains defined

   function Mains (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre  => Self.Is_Defined and then Self.Has_Mains,
          Post => Mains'Result.Length > 0;
   --  Returns the mains's binary full pathname

   function Library_Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the library name

   function Library_Kind (Self : Object) return Name_Type
     with Pre  => Self.Is_Defined
                  and then Self.Is_Library,
          Post => Self.Has_Attributes (Project.Registry.Attribute.Library_Kind)
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

   function Has_Library_Version (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns whether the optional library version name is defined

   function Library_Major_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Is_Library;
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
   --  Returns the library directory, note that this may be difference than
   --  getting the Library_Dir attribute value as the result here is always
   --  a path-name with proper resolution for relative directory specification.

   function Library_Ali_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the library directory, note that this may be difference than
   --  getting the Library_Ali_Dir attribute value as the result here is always
   --  a path-name with proper resolution for relative directory specification.

   function Library_Standalone (Self : Object) return Standalone_Library_Kind
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns the kind for the standalone library

   function Is_Library_Standalone (Self : Object) return Boolean
     with Pre => Self.Is_Defined and then Self.Is_Library;
   --  Returns whether the library is standalone

   function Object_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre =>
       Self.Is_Defined
       and then Self.Kind in K_Standard | K_Library | K_Aggregate_Library;
   --  As above but for the Object_Dir attribute

   function Executable_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre =>
       Self.Is_Defined
       and then Self.Kind in K_Standard | K_Library | K_Aggregate_Library;
   --  As above but for the Exec_Dir attribute

   function Executable_Suffix (Self : Object) return String
     with Pre => Self.Is_Defined;
   --  Returns executable suffix for this project

   function Binder_Prefix
     (Self : Object; Language : Name_Type) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Prefix to be used for the binder exchange file name for the language.
   --  Used to have different binder exchange file names when binding different
   --  languages.

   procedure Release (Self : in out Object);
   --  Releases the project view and release all associated memory

private

   type Object is new Definition_References.Ref with null record;

   Undefined : constant Object :=
                 Object'(Definition_References.Null_Ref with null record);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function "<" (Left, Right : Object) return Boolean is
     (Left.Get.Id < Right.Get.Id);

   function Is_Library (Self : Object) return Boolean is
      (Self.Kind in K_Library | K_Aggregate_Library);

   function Library_Name (Self : Object) return Name_Type is
     (Name_Type
        (Self.Attribute (Registry.Attribute.Library_Name).Value.Text));

   function Has_Library_Version (Self : Object) return Boolean is
     (Self.Has_Attributes (Registry.Attribute.Library_Version));

   function Has_Library_Interface (Self : Object) return Boolean is
     (Self.Has_Attributes (Registry.Attribute.Library_Interface));

   function Is_Library_Standalone (Self : Object) return Boolean is
      (Self.Library_Standalone /= No);

   function Dir_Name (Self : Object) return GPR2.Path_Name.Object is
     (Self.Get.Path);

end GPR2.Project.View;
