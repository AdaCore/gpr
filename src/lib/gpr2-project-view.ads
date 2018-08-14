------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
with GPR2.Project.Variable.Set;

limited with GPR2.Project.Source.Set;
limited with GPR2.Project.Tree;
limited with GPR2.Project.View.Set;

private with GPR2.Project.Registry.Attribute;

package GPR2.Project.View is

   use type Containers.Count_Type;
   use type Context.Object;
   use type Attribute.Object;
   use type Variable.Object;

   type Object is tagged private;

   subtype Project_View is Object;

   Undefined : constant Object;

   function "<" (Left, Right : View.Object) return Boolean;
   --  Ordering a project object to be able to build an ordered map for example

   function Path_Name (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self /= Undefined;
   --  Full pathname of the corresponding project file

   function Name (Self : Object) return Name_Type
     with Pre => Self /= Undefined;
   --  The name of the project

   function Qualifier (Self : Object) return Project_Kind
     with Pre => Self /= Undefined;
   --  The qualifier as specified in the project file

   function Kind (Self : Object) return Project_Kind
     with Pre  => Self /= Undefined,
          Post => Kind'Result = Self.Qualifier
                  or else Self.Qualifier = K_Standard;
   --  The actual kind of the project file. This may be different if the
   --  Qualifier is not specified.

   function Tree (Self : Object) return not null access Project.Tree.Object
     with Pre => Self /= Undefined;
   --  Returns the corresponding project tree

   function Signature (Self : Object) return Context.Binary_Signature;
   --  Returns the signature for the view

   function Has_Imports (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the project has some imports

   function Imports
     (Self      : Object;
      Recursive : Boolean := False) return GPR2.Project.View.Set.Object
     with Pre => Self /= Undefined and then Self.Has_Imports;
   --  Returns all imported project views

   function Has_Extended (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the project is extending another project

   function Extended (Self : Object) return Object
     with Pre => Self /= Undefined and then Self.Has_Extended;
   --  Returns the extended project

   function Is_Extended_All (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the project is extending all another project

   function Aggregated (Self : Object) return GPR2.Project.View.Set.Object
     with Pre => Self /= Undefined and then Self.Kind in Aggregate_Kind;

   function View_For
     (Self : Object;
      Name : Name_Type) return View.Object
     with Pre => Self /= Undefined;
   --  Returns the view for the given name accessible from Self context. This
   --  can be either an import project, an extends project or the special
   --  projects Runtime or Config if defined in the corresponding project tree.

   --  Context

   function Has_Context (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the project tree has some context. If any of the project
   --  in the tree has some external variables then a context is present. A
   --  project without context is fully static has it does not reference any
   --  external (and so modifiable) variables.

   function Context (Self : Object) return Context.Object
     with Pre  => Self /= Undefined,
          Post => Self.Has_Context = (Context'Result /= GPR2.Context.Empty);
   --  Returns the Context for the given project tree

   --  Attributes

   function Has_Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := "";
      Index : String := "") return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project view has some attributes defined. If Name
   --  and/or Index are set it returns True if an attribute with the given
   --  Name and/or Index is defined.

   function Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := "";
      Index : String := "") return Attribute.Set.Object
     with Post =>
       (if Self.Has_Attributes (Name) then not Attributes'Result.Is_Empty);
   --  Get the list of attributes, possibly an empty list if it does not
   --  contain attributes or if Name and Index does not match any attribute.

   function Attribute
     (Self  : Object;
      Name  : Name_Type;
      Index : String := "") return Attribute.Object
     with
       Pre =>
         Self /= Undefined
         and then Self.Has_Attributes (Name, Index)
         and then Self.Attributes (Name, Index).Length = 1;
   --  Returns the Attribute with the given Name and possibly Index

   function Has_Variables
     (Self : Object;
      Name : Optional_Name_Type := "") return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project view has some variables defined

   function Variables (Self : Object) return Variable.Set.Object
     with Pre  => Self /= Undefined,
          Post => (if Self.Has_Variables then not Variables'Result.Is_Empty);
   --  Get the list of variables, possibly an empty list if it does not
   --  contain variables.

   function Variable (Self : Object; Name : Name_Type) return Variable.Object
     with Pre  => Self /= Undefined and then Self.Has_Variables (Name),
          Post => Variable'Result /= Project.Variable.Undefined;

   --  Packages

   function Has_Packages
     (Self : Object;
      Name : Optional_Name_Type := "") return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project view has some packages defined

   function Packages (Self : Object) return Pack.Set.Object
     with Post => (if Self.Has_Packages then not Packages'Result.Is_Empty);
   --  Get the list of packages, possibly an empty list if it does not
   --  contain packages.

   --  Sources

   function Source_Directories (Self : Object) return Project.Attribute.Object
     with Pre => Self /= Undefined
                 and then Self.Qualifier not in K_Aggregate | K_Abstract;
   --  Returns the sources dirs for the project view. This is only defined for
   --  project having sources. If not defined in the project itself, the view
   --  does have the project directory has source dir.

   function Has_Sources (Self : Object) return Boolean
     with Pre  => Self /= Undefined,
          Post => (if Self.Kind = K_Abstract then not Has_Sources'Result);
   --  Returns true if the project view has some sources

   type Source_Kind is (K_All, K_Interface_Only, K_Not_Interface);

   function Sources
     (Self   : Object;
      Filter : Source_Kind := K_All) return Project.Source.Set.Object
     with Pre => Self /= Undefined;
   --  Returns all the sources for the view, note that this routine ensure that
   --  the current sources are up-to-date by calling Update_Sources below.

   function Source
     (Self : Object; File : GPR2.Path_Name.Object) return Project.Source.Object
     with Pre => Self /= Undefined;
   --  Get project source object corresponding to the given File

   procedure Invalidate_Sources (Self : Object)
     with Pre => Self /= Undefined;
   --  Invalidate the sources for the view. This means that the Sources routine
   --  above will have to recompute the proper sources list for the view. This
   --  is needed when some sources are added or removed from the view.

   procedure Update_Sources (Self : Object)
     with Pre => Self /= Undefined;
   --  Ensure that the view sources are up-to-date. This is needed before
   --  computing the dependecies of a source in the project tree. This routine
   --  is called where needed and is there for internal use only.

   --  Some common attributes redefined here and when some pathname are
   --  relative to the view, the proper value is returned. Following
   --  routines are for internal use only and convert from a View unique Id.

   function Is_Externally_Built (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project is externally built

   function Has_Mains (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project has some mains defined

   function Mains (Self : Object) return GPR2.Path_Name.Set.Object
     with Pre  => Self /= Undefined and then Self.Has_Mains,
          Post => Mains'Result.Length > 0;
   --  Returns the mains's full pathname

   function Library_Name (Self : Object) return Name_Type
     with Pre => Self /= Undefined
                 and then Self.Kind in K_Library | K_Aggregate_Library;
   --  Returns the library name

   function Has_Library_Version (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Retruns whether the optional library version name is defined

   function Library_Major_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self /= Undefined
                 and then Self.Kind in K_Library | K_Aggregate_Library;
   --  Returns the library major name if it exists. That is, if the project
   --  Library_Version exists and is set to libxyz.so.1.2 for example then the
   --  returned value is libxyz.so.1. If no major version is computable an
   --  undefined path-naeme is returned.

   function Library_Filename (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self /= Undefined
                 and then Self.Kind in K_Library | K_Aggregate_Library;
   --  Returns the actual file name for the library

   function Library_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self /= Undefined
                 and then Self.Has_Library_Version
                 and then Self.Kind in K_Library | K_Aggregate_Library;
   --  Returns the library version filename

   function Library_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self /= Undefined
                 and then Self.Kind in K_Library | K_Aggregate_Library;
   --  Returns the library directory, note that this may be difference than
   --  getting the Library_Dir attribute value as the result here is always
   --  a path-name with proper resoluion for relative directory specification.

   function Library_Standalone (Self : Object) return Standalone_Library_Kind
     with Pre => Self /= Undefined
                 and then Self.Kind in K_Library | K_Aggregate_Library;
   --  Retruns the kind for the standalone library

   function Object_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre =>
       Self /= Undefined
       and then Self.Kind in K_Standard | K_Library | K_Aggregate_Library;
   --  As above but for the Object_Dir attribute

   function Executable_Directory (Self : Object) return GPR2.Path_Name.Object
     with Pre =>
       Self /= Undefined
       and then Self.Kind in K_Standard | K_Library | K_Aggregate_Library;
   --  As above but for the Exec_Dir attribute

   type Id is new Natural;
   --  A single unique Id which represent a reference to the view in the
   --  registry.

   function From_Id (Id : View.Id) return Object;
   --  Returns a View.Object given its internal Id unique reference

   procedure Release (Self : in out Object);
   --  Release the project view and release all associated memory

private

   type Object is tagged record
      Id : View.Id := 0;
   end record;

   function "<" (Left, Right : Object) return Boolean is
     (Left.Id < Right.Id);

   Undefined  : constant Object := (Id => 0);

   function Library_Name (Self : Object) return Name_Type is
     (Name_Type
        (Self.Attribute (GPR2.Project.Registry.Attribute.Library_Name).Value));

   function Has_Library_Version (Self : Object) return Boolean is
     (Self.Has_Attributes
        (GPR2.Project.Registry.Attribute.Library_Version));

end GPR2.Project.View;
