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

with Ada.Iterator_Interfaces;

with GPR2.Context;
with GPR2.Project.View;

private with GPR2.Project.Definition;

package GPR2.Project.Tree is

   use type GPR2.Context.Object;
   use type GPR2.Project.View.Object;
   use type Ada.Containers.Count_Type;

   type Object is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => View.Object;

   Undefined : constant Object;

   function Load (Filename : Path_Name_Type) return Object;
   --  Load a root-project

   function Root_Project (Tree : Object) return View.Object
     with Post => Root_Project'Result /= View.Undefined;
   --  Returns the root project for the given tree

   --  Context

   function Has_Context (Tree : Object) return Boolean
     with Pre  => Tree /= Undefined;

   function Context (Tree : Object) return Context.Object
     with Pre  => Tree /= Undefined,
          Post =>
            Has_Context (Tree) = (Context'Result /= GPR2.Context.Empty);

   procedure Set_Context
     (Tree    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null)
     with Pre  => Tree /= Undefined,
          Post => Tree.Context = Context;

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return View.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result /= View.Undefined
        else Element'Result = View.Undefined);

   function Has_Element (Position : Cursor) return Boolean;

   package Project_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Is_Root (Position : Cursor) return Boolean;
   --  Returns True if the cursor is pointing to the root project

   type Constant_Reference_Type
     (View : not null access constant Project.View.Object) is private
     with Implicit_Dereference => View;

   function Constant_Reference
     (Tree      : aliased in out Object;
      Position  : Cursor) return Constant_Reference_Type;

   function Iterate
     (Tree   : Object;
      Kind   : Iterator_Kind := I_Default;
      Filter : Project_Filter := F_Default)
      return Project_Iterator.Forward_Iterator'Class
     with Pre => Kind /= I_Invalid;

private

   type Object is tagged record
      Root    : View.Object;
      Context : GPR2.Context.Object;
   end record;

   type Cursor is record
      Views   : Definition.Project_View_Store.Vector;
      Current : Positive;
      Root    : View.Object;
   end record;

   type Constant_Reference_Type
     (View : not null access constant Project.View.Object) is null record;

   Undefined  : constant Object :=
                  (Root => View.Undefined, Context => GPR2.Context.Empty);
   No_Element : constant Cursor :=
                  (Definition.Project_View_Store.Empty_Vector,
                   1, View.Undefined);

end GPR2.Project.Tree;
