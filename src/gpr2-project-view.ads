------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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

--  A specific view of a project as seen in a tree. A project view is retreived
--  from a tree, see Project.Tree package. A project view differ from a
--  standard project object from the parser as it does access to the actual
--  attributes, variables and packages values depending on the current context
--  for the corresponding tree.

with Ada.Containers;

with GPR2.Context;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Variable.Set;

package GPR2.Project.View is

   use type Ada.Containers.Count_Type;

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

   --  Attributes

   function Has_Attributes
     (Self  : Object;
      Name  : String := "";
      Index : String := "") return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project view has some attributes defined. If Name
   --  and/or Index are set it returns True if an attribute with the given
   --  Name and/or Index is defined.

   function Attributes
     (Self  : Object;
      Name  : String := "";
      Index : String := "") return Attribute.Set.Object
     with Post => (if Self.Has_Attributes then Attributes'Result.Length > 0);
   --  Get the list of attributes, possibly an empty list if it does not
   --  contain attributes or if Name and Index does not match any attribute.

   --  Variables

   function Has_Variables
     (Self : Object;
      Name : String := "") return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project view has some variables defined

   function Variables
     (Self : Object;
      Name : String := "") return Variable.Set.Object
     with Post => (if Self.Has_Variables then Variables'Result.Length > 0);
   --  Get the list of variables, possibly an empty list if it does not
   --  contain variables.

   --  Packages

   function Has_Packages (Self : Object) return Boolean
     with Pre => Self /= Undefined;
   --  Returns true if the project view has some packages defined

   function Packages (Self : Object) return Pack.Set.Object
     with Post => (if Self.Has_Packages then Packages'Result.Length > 0);
   --  Get the list of packages, possibly an empty list if it does not
   --  contain packages.

   --  Following routines are for internal use only and convert from a View
   --  unique Id.

   type Id is new Natural;
   --  A single unique Id which represent a reference to the view in the
   --  registry.

   function From_Id (Id : View.Id) return Object;
   --  Returns a View.Object given its internal Id unique reference

private

   type Object is tagged record
      Id : View.Id;
   end record;

   function "<" (Left, Right : Object) return Boolean is
     (Left.Id < Right.Id);

   Undefined  : constant Object := (Id => 0);

end GPR2.Project.View;
