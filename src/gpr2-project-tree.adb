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

with Ada.Containers.Ordered_Sets; use Ada;
with Ada.Environment_Variables;

with GPR2.Project.Definition;
with GPR2.Parser.Project;

package body GPR2.Project.Tree is

   type Iterator (Kind : Iterator_Kind; Filter : Project_Filter) is
     new Project_Iterator.Forward_Iterator with
   record
     Root : Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor)  return Cursor;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased in out Object;
      Position : Cursor) return Constant_Reference_Type
   is
      pragma Unreferenced (Self);
   begin
      --  Constant reference is given by the constant reference of the
      --  element contained in the Views set at the current location.
      return Constant_Reference_Type'
        (View =>
           Definition.Project_View_Store.Constant_Reference
             (Position.Views, Position.Current).Element);
   end Constant_Reference;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is
   begin
      return Self.Context;
   end Context;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return View.Object is
   begin
      return Position.Views (Position.Current);
   end Element;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is

      use type View.Id;

      package Seen_Project is new Containers.Ordered_Sets (View.Object);

      Seen : Seen_Project.Set;
      --  Keep track of already seen projects. Better than using the P vector
      --  which is not efficient when checking if an element exists.

      Projects : Definition.Project_View_Store.Vector;
      --  Set of projects for the iterator which is returned in the Cursor and
      --  fill by the recursive procedure For_Project and For_Imports.

      procedure Append (View : Project.View.Object)
        with Post => Seen.Contains (View);
      --  Append into P if not already seen

      procedure For_Project (Id : View.Object);
      --  Handle project node

      procedure For_Imports (Id : View.Object);
      --  Handle import nodes

      ------------
      -- Append --
      ------------

      procedure Append (View : Project.View.Object) is
         Qualifier : constant Project_Qualifier := View.Qualifier;
      begin
         if not Seen.Contains (View) then
            --  Check if it corresponds to the current filter
            if (Qualifier = Q_Library
                and then Is_Set (Iter.Filter, F_Library))
              or else (Qualifier = Q_Standard
                       and then Is_Set (Iter.Filter, F_Standard))
              or else (Qualifier = Q_Abstract
                       and then Is_Set (Iter.Filter, F_Abstract))
              or else (Qualifier = Q_Aggregate
                       and then Is_Set (Iter.Filter, F_Aggregate))
              or else (Qualifier = Q_Aggregate_Library
                       and then Is_Set (Iter.Filter, F_Aggregate_Library))
            then
               Projects.Append (View);
            end if;

            Seen.Insert (View);
         end if;
      end Append;

      -----------------
      -- For_Imports --
      -----------------

      procedure For_Imports (Id : View.Object) is
      begin
         for I of Definition.Get (Id).Imports loop
            if Is_Set (Iter.Kind, I_Recursive) then
               For_Project (I);
            else
               Append (I);
            end if;
         end loop;
      end For_Imports;

      -----------------
      -- For_Project --
      -----------------

      procedure For_Project (Id : View.Object) is
      begin
         if Is_Set (Iter.Kind, I_Imported)
           or else Is_Set (Iter.Kind, I_Recursive)
         then
            For_Imports (Id);
         end if;

         Append (Id);
      end For_Project;

   begin
      For_Project (Iter.Root.Root);
      return Cursor'(Projects, 1, Iter.Root.Root);
   end First;

   -----------------
   -- Has_Context --
   -----------------

   function Has_Context (Self : Object) return Boolean is
   begin
      return Self.Context.Length > 0;
   end Has_Context;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Position : Cursor) return Boolean is
   begin
      return Position.Views (Position.Current) = Position.Root;
   end Is_Root;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self   : Object;
      Kind   : Iterator_Kind := I_Default;
      Filter : Project_Filter := F_Default)
      return Project_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Kind, Filter, Self);
   end Iterate;

   ----------
   -- Load --
   ----------

   function Load (Filename : Path_Name_Type) return Object is

      function Load (Filename : Path_Name_Type) return Definition.Data;

      ----------
      -- Load --
      ----------

      function Load
        (Filename : Path_Name_Type) return Definition.Data
      is

         procedure Register_View (Root_Project : Definition.Data);

         View : Definition.Data;

         -------------------
         -- Register_View --
         -------------------

         procedure Register_View (Root_Project : Definition.Data) is
         begin
            --  Register into registry
            for Project of Root_Project.Trees.Imports loop
               View.Imports.Append (Load (Project.Path_Name).Root);
            end loop;
         end Register_View;

      begin
         --  First load the root project

         View.Trees.Project := Parser.Project.Load (Filename);

         --  Now load all imported projects if any

         for Project_Name of View.Trees.Project.Imports loop
            View.Trees.Imports.Insert
              (Project_Name, Parser.Project.Load (Project_Name));
         end loop;

         --  Create all views

         Register_View (View);

         return View;
      end Load;

      Root_View : constant Definition.Data := Load (Filename);
      Context   : GPR2.Context.Object;

   begin
      --  Let's setup the full external environment for project

      for E of Root_View.Trees.Project.Externals loop
         --  Fill all known external in the environment variables
         if Environment_Variables.Exists (E) then
            Context.Include (E, Environment_Variables.Value (E));
         end if;
      end loop;

      return Tree : Object :=
        Object'(Root    => Definition.Register (Root_View),
                Context => Context)
      do
         Set_Context (Tree, Context);
      end return;
   end Load;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Iter);
      C : Cursor := Position;
   begin
      if C.Current < Natural (C.Views.Length) then
         C.Current := C.Current + 1;
         return C;
      else
         return No_Element;
      end if;
   end Next;

   ------------------
   -- Root_Project --
   ------------------

   function Root_Project (Self : Object) return View.Object is
   begin
      return Self.Root;
   end Root_Project;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null) is
   begin
      --  Register the context for this project tree

      Self.Context := Context;

      --  Propagate the change in the project Tree. That is for each project in
      --  the tree we need to update the corresponding view.

      for View of Self loop
         declare
            procedure Changed (Project : GPR2.Parser.Project.Object);
            --  Callback signaled when a project is changed, that is when one
            --  of its scenario variable has changed value.

            -------------
            -- Changed --
            -------------

            procedure Changed (Project : GPR2.Parser.Project.Object) is
               pragma Unreferenced (Project);
            begin
               if Set_Context.Changed /= null then
                  Set_Context.Changed (View);
               end if;
            end Changed;

            P_Data : Definition.Data := Definition.Get (View);

         begin
            Parser.Project.Parse
              (P_Data.Trees.Project,
               Context,
               P_Data.Attrs,
               P_Data.Vars,
               P_Data.Packs,
               Changed'Access);

            Definition.Set (View, P_Data);
         end;
      end loop;
   end Set_Context;

end GPR2.Project.Tree;
