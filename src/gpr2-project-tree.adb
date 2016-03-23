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
with Ada.Strings.Equal_Case_Insensitive;

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
         Qualifier : constant Project_Kind := View.Kind;
      begin
         if not Seen.Contains (View) then
            --  Check if it corresponds to the current filter
            if (Qualifier = K_Library
                and then Is_Set (Iter.Filter, F_Library))
              or else (Qualifier = K_Standard
                       and then Is_Set (Iter.Filter, F_Standard))
              or else (Qualifier = K_Abstract
                       and then Is_Set (Iter.Filter, F_Abstract))
              or else (Qualifier = K_Aggregate
                       and then Is_Set (Iter.Filter, F_Aggregate))
              or else (Qualifier = K_Aggregate_Library
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
      return not Self.Context.Is_Empty;
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

         View.Externals := View.Trees.Project.Externals;

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
      Context   : GPR2.Context.Object := GPR2.Context.Empty;

   begin
      --  Let's setup the full external environment for project

      for E of Root_View.Externals loop
         --  Fill all known external in the environment variables
         if Environment_Variables.Exists (E) then
            Context.Include (E, Environment_Variables.Value (E));
         end if;
      end loop;

      return Tree : Object :=
        Object'(Root    => Definition.Register (Root_View),
                Context => Context)
      do
         for View of Tree loop
            declare
               V_Data : Definition.Data := Definition.Get (View);
            begin
               --  Compute the external dependencies for the view. This is
               --  the set of external used in the project and in all imported
               --  project.

               for E of V_Data.Externals loop
                  if not V_Data.Externals.Contains (E) then
                     V_Data.Externals.Append (E);
                  end if;
               end loop;

               Definition.Set (View, V_Data);
            end;
         end loop;

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
            use type GPR2.Context.Binary_Signature;

            P_Data        : Definition.Data := Definition.Get (View);
            Old_Signature : constant GPR2.Context.Binary_Signature :=
                              P_Data.Sig;
            New_Signature : constant GPR2.Context.Binary_Signature :=
                              Context.Signature (P_Data.Externals);
         begin
            Parser.Project.Parse
              (P_Data.Trees.Project,
               Self,
               P_Data.Attrs,
               P_Data.Vars,
               P_Data.Packs);

            P_Data.Sig := New_Signature;
            Definition.Set (View, P_Data);

            --  Signal project change only if we have different and non default
            --  signature. That is if there is at least some external used
            --  otherwise the project is stable and won't change.

            if Old_Signature /= New_Signature
              and then P_Data.Sig /= GPR2.Context.Default_Signature
              and then Changed /= null
            then
               Changed (View);
            end if;
         end;
      end loop;
   end Set_Context;

   --------------
   -- View_For --
   --------------

   function View_For
     (Self : Object;
      Name : Name_Type;
      Ctx  : GPR2.Context.Object) return View.Object
   is
      use type GPR2.Context.Binary_Signature;
      Tree : Object := Self;
   begin
      for View of Tree loop
         if Strings.Equal_Case_Insensitive (View.Name, Name) then
            declare
               P_Data : constant Definition.Data := Definition.Get (View);
               P_Sig  : constant GPR2.Context.Binary_Signature :=
                          Ctx.Signature (P_Data.Externals);
            begin
               if View.Signature = P_Sig then
                  return View;
               end if;
            end;
         end if;
      end loop;

      return View.Undefined;
   end View_For;

end GPR2.Project.Tree;
