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
with GPR2.Project.Attribute.Set;

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

   function Recursive_Load
     (Filename     : Path_Name_Type;
      Context_View : View.Object;
      Status       : Definition.Relation_Status;
      Root_Context : out GPR2.Context.Object) return View.Object;
   --  Load a project filename recurivelly and returns the corresponding root
   --  view.

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
      return Self.Root_Project.Context;
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

      procedure For_Project (View : Project.View.Object);
      --  Handle project node

      procedure For_Imports (View : Project.View.Object);
      --  Handle import nodes

      procedure For_Aggregated (View : Project.View.Object);
      --  Handle aggregated nodes

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

      --------------------
      -- For_Aggregated --
      --------------------

      procedure For_Aggregated (View : Project.View.Object) is
      begin
         if View.Kind = K_Aggregate then
            for A of Definition.Get (View).Aggregated loop
               Append (A);
            end loop;
         end if;
      end For_Aggregated;

      -----------------
      -- For_Imports --
      -----------------

      procedure For_Imports (View : Project.View.Object) is
      begin
         for I of Definition.Get (View).Imports loop
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

      procedure For_Project (View : Project.View.Object) is
      begin
         if Is_Set (Iter.Kind, I_Imported)
           or else Is_Set (Iter.Kind, I_Recursive)
         then
            For_Imports (View);
         end if;

         Append (View);

         --  Now if View is an aggregate project we need to run through all
         --  aggregated projects.

         if Is_Set (Iter.Kind, I_Aggregated) then
            For_Aggregated (View);
         end if;
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
      return not Self.Root_Project.Context.Is_Empty;
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

      Root_Context : GPR2.Context.Object;

      Root_View : constant View.Object :=
                    Recursive_Load (Filename,
                                    View.Undefined,
                                    Definition.Root,
                                    Root_Context);

   begin
      return Tree : Object := Object'(Root => Root_View) do
         for View of Tree loop
            declare
               V_Data : Definition.Data := Definition.Get (View);
            begin
               --  Compute the external dependencies for the views. This is
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

         Set_Context (Tree, Root_Context);
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

   --------------------
   -- Recursive_Load --
   --------------------

   function Recursive_Load
     (Filename     : Path_Name_Type;
      Context_View : View.Object;
      Status       : Definition.Relation_Status;
      Root_Context : out GPR2.Context.Object) return View.Object

   is
      function Load (Filename : Path_Name_Type) return Definition.Data;
      --  Returns the Data definition for the given project

      ----------
      -- Load --
      ----------

      function Load (Filename : Path_Name_Type) return Definition.Data is
         Project : constant Parser.Project.Object :=
                     Parser.Project.Load (Filename);

         Data    : Definition.Data
                     (Has_Context =>
                        (Context_View = GPR2.Project.View.Undefined)
                      or else Project.Qualifier = K_Aggregate);
      begin
         Data.Trees.Project := Project;
         Data.Externals := Data.Trees.Project.Externals;

         --  Now load all imported projects if any

         for Project_Name of Data.Trees.Project.Imports loop
            Data.Trees.Imports.Insert
              (Project_Name, Parser.Project.Load (Project_Name));
         end loop;

         return Data;
      end Load;

      Data : Definition.Data := Load (Filename);
      View : Project.View.Object;

   begin
      --  Let's setup the full external environment for project

      for E of Data.Externals loop
         --  Fill all known external in the environment variables
         if Environment_Variables.Exists (E) then
            Root_Context.Include (E, Environment_Variables.Value (E));
         end if;
      end loop;

      --  If we have the root project, record the global context

      if Data.Has_Context
        and then Context_View = Project.View.Undefined
      then
         --  This is the root-view, assign the corresponding context
         Data.Context := Root_Context;
      end if;

      --  Create the view, needed to be able to reference it if it is an
      --  aggregate project as it becomes the new View_Context.

      Data.Context_View := Context_View;
      Data.Status       := Status;

      View := Definition.Register (Data);

      --  Now load all imported projects. If we have parsing the root
      --  project or an aggregate project then the context view become
      --  this project.

      for Project of Data.Trees.Imports loop
         Data.Imports.Append
           (Recursive_Load
              (Project.Path_Name,
               Context_View =>
                 (if Context_View = GPR2.Project.View.Undefined
                  then View
                  else Context_View),
               Status       => Definition.Imported,
               Root_Context => Root_Context));
      end loop;

      --  And record back new data for this view

      Definition.Set (View, Data);

      return View;
   end Recursive_Load;

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
      Changed : access procedure (Project : View.Object) := null)
   is

      procedure Set_View (View : Project.View.Object);
      --  Set the context for the given view

      --------------
      -- Set_View --
      --------------

      procedure Set_View (View : Project.View.Object) is
         use type GPR2.Context.Binary_Signature;

         P_Data        : Definition.Data := Definition.Get (View);
         Old_Signature : constant GPR2.Context.Binary_Signature :=
                           P_Data.Sig;
         New_Signature : constant GPR2.Context.Binary_Signature :=
                           Context.Signature (P_Data.Externals);
         Context       : constant GPR2.Context.Object :=
                           View.Context;
      begin
         Parser.Project.Parse
           (P_Data.Trees.Project,
            Self,
            Context,
            P_Data.Attrs,
            P_Data.Vars,
            P_Data.Packs);

         --  Now we can record the aggregated projects based on the possibly
         --  new Project_Files attribute value.

         if View.Qualifier = K_Aggregate then
            P_Data.Aggregated.Clear;

            for Project of P_Data.Attrs ("project_files").Values loop
               declare
                  Pathname : constant Path_Name_Type := Create (Project);
                  Ctx      : GPR2.Context.Object;
                  A_View   : constant GPR2.Project.View.Object :=
                               Recursive_Load
                                 (Pathname, View, Definition.Aggregated, Ctx);
               begin
                  --  Record aggregated view into the aggregate's view
                  P_Data.Aggregated.Append (A_View);
                  --  And set the aggregated view recursivelly
                  Set_View (A_View);
               end;
            end loop;

            --  And finaly also record the External definition if any into
            --  the aggregate project context.

            for C in P_Data.Attrs.Iterate_Filter ("external") loop
               declare
                  External : constant Attribute.Object :=
                               Attribute.Set.Set.Element (C);
               begin
                  P_Data.A_Context.Include (External.Index, External.Value);
               end;
            end loop;
         end if;

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
      end Set_View;

   begin
      --  Register the root context for this project tree

      declare
         Data : Definition.Data := Definition.Get (Self.Root_Project);
      begin
         Data.Context := Context;
         Definition.Set (Self.Root_Project, Data);
      end;

      --  Propagate the change in the project Tree. That is for each project in
      --  the tree we need to update the corresponding view. We do not handle
      --  the aggregated project here. Those projects are specifically in
      --  Set_View. This is needed as parsing the aggregate project may change
      --  the Project_Files attribute and so the actual aggregated project. So
      --  we cannot use the current aggregated project list.

      for View in Self.Iterate
        (Kind => I_Project or I_Imported or I_Recursive)
      loop
         Set_View (Element (View));
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
