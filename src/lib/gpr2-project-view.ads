------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Variable.Set;

limited with GPR2.Project.Source.Set;

package GPR2.Project.View is

   use type Containers.Count_Type;
   use type Context.Object;
   use type Attribute.Object;

   type Object is tagged private;

   subtype Project_View is Object;

   Undefined : constant Object;

   function "<" (Left, Right : View.Object) return Boolean;
   --  Ordering a project object to be able to build an ordered map for example

   function Path_Name (Self : Object) return Path_Name_Type
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
   --  The actual kind of the project file. This may be different of the
   --  Qualifier is not specified.

   function Signature (Self : Object) return Context.Binary_Signature;
   --  Returns the signature for the view

   function Has_Imports (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns True if the project has some imports

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

   function Variables
     (Self : Object;
      Name : Optional_Name_Type := "") return Variable.Set.Object
     with Post => (if Self.Has_Variables then not Variables'Result.Is_Empty);
   --  Get the list of variables, possibly an empty list if it does not
   --  contain variables.

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

   function Has_Sources (Self : Object) return Boolean
     with Pre  => Self /= Undefined,
          Post => (if Self.Kind = K_Abstract then not Has_Sources'Result);
   --  Returns true if the project view has some sources

   function Sources (Self : Object) return Project.Source.Set.Object
     with Pre => Self /= Undefined;
   --  Returns all the sources for the view, note that this routine ensure that
   --  the current sources are up-to-date by calling Update_Sources below.

   function Source
     (Self : Object; File : Path_Name_Type) return Project.Source.Object
     with Pre => Self /= Undefined;
   --  Get project source object corresponding to the give File

   procedure Update_Sources (Self : Object)
     with Pre => Self /= Undefined;
   --  Ensure that the view sources are up-to-date. This is needed before
   --  computing the dependecies of a source in the project tree. This routine
   --  is called where needed and is there for internal use only.

   --  Following routines are for internal use only and convert from a View
   --  unique Id.

   type Id is new Natural;
   --  A single unique Id which represent a reference to the view in the
   --  registry.

   function From_Id (Id : View.Id) return Object;
   --  Returns a View.Object given its internal Id unique reference

private

   type Object is tagged record
      Id : View.Id := 0;
   end record;

   function "<" (Left, Right : Object) return Boolean is
     (Left.Id < Right.Id);

   Undefined  : constant Object := (Id => 0);

end GPR2.Project.View;
