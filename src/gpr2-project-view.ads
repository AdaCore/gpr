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
--  from a tree, see Project.Tree package.

with Ada.Containers;

with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Variable.Set;

package GPR2.Project.View is

   use type Ada.Containers.Count_Type;

   type Object is tagged private;

   function "<" (Left, Right : View.Object) return Boolean;

   Undefined : constant Object;

   function Path_Name (Project : Object) return Path_Name_Type
     with Pre => Project /= Undefined;

   function Name (Project : Object) return Name_Type
     with Pre => Project /= Undefined;

   function Qualifier (Project : Object) return Project_Qualifier
     with Pre => Project /= Undefined;

   --  Imports

   function Has_Imports (Project : Object) return Boolean
     with Pre => Project /= Undefined;

   --  Attributes

   function Has_Attributes (Project : Object) return Boolean
     with Pre => Project /= Undefined;

   function Attributes (Project : Object) return Attribute.Set.Object
     with Post =>
       (if Project.Has_Attributes then Attributes'Result.Length > 0);

   --  Variables

   function Has_Variables (Project : Object) return Boolean
     with Pre => Project /= Undefined;

   function Variables (Project : Object) return Variable.Set.Object
     with Post =>
       (if Project.Has_Variables then Variables'Result.Length > 0);

   --  Packages

   function Has_Packages (Project : Object) return Boolean
     with Pre => Project /= Undefined;

   function Packages (Project : Object) return Pack.Set.Object
     with Post =>
       (if Project.Has_Packages then Packages'Result.Length > 0);

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
