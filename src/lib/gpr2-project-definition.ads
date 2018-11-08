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

with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.Context;
with GPR2.Parser.Project.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Pack.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Unit.Set;

limited with GPR2.Project.Tree;

private package GPR2.Project.Definition is

   use type View.Id;
   use type View.Object;
   use type Parser.Project.Object;

   --  Tree contains the Project parser object. This is shared by all projects
   --  view in all loaded tree. That is there is always a single instance of
   --  the project parser object.
   --  Imports contains the list of all imported projects for Project.

   type Tree is record
      Project : Parser.Project.Object;
      Imports : Parser.Project.Set.Object;
   end record;

   package Project_View_Store is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, View.Object);

   --  Data contains a project view data. We have all the attributes, variables
   --  and packages with the final values as parsed with the project's context
   --  in the given tree. Imports here are the project views corresponding to
   --  the imports in Trees.
   --
   --  Either a Data has a context or is referencing another with containing
   --  the context. This is used for aggregate project which can be used to
   --  refine the global context by setting some external values with the
   --  corresponding attribute. So both the root project and all aggregate
   --  projects have a context. All other projects are referencing a project
   --  which own a context.

   type Data (Has_Context : Boolean) is new Definition_Base (Has_Context)
   with record
      Trees             : Tree;

      --  Actual values for the view
      Extended          : View.Object;
      Imports           : Project_View_Store.Map;
      Aggregated        : Project_View_Store.Map;
      Attrs             : Project.Attribute.Set.Object;
      Vars              : Project.Variable.Set.Object;
      Packs             : Project.Pack.Set.Object;
      Types             : Project.Typ.Set.Object;

      Sources           : Project.Source.Set.Object;

      Units             : Unit.Set.Object;

      --  Some general information
      Context_View      : View.Object;
      --  The context view is the view that has context for this project. That
      --  is, Context_View will always point to a view with a context, either
      --  the Undefined view (means root project) which contains the context
      --  from the running environment or an aggregate project which has
      --  a context from the External attribute. Undefined is used for the
      --  root view to differentiate a root context from a root and aggregate
      --  project.

      --  The project tree for this view
      Tree              : access Project.Tree.Object;
   end record
     with Dynamic_Predicate =>
            --  Only a root-aggregate project can have a context defined via
            --  the External attribute.
            not Data.Has_Context
            or else (Data.Context_View = View.Undefined
                     and then Data.Status = Root)
            or else Data.A_Context.Is_Empty;

   type Ref is access all Data;

   type Const_Ref is access constant Data;

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.View package --
   --------------------------------------------------------------

   From_Id : access function
     (Id : View.Id; Tree : access Project.Tree.Object) return View.Object;
   --  Returns a View.Object given its internal Id unique reference

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Tree package --
   --------------------------------------------------------------

   Register : access function
     (Def : Definition.Data) return Project.View.Object;
   --  Register view definition in the project tree

   Get_RO : access function (View : Project.View.Object) return Const_Ref;
   --  Returns the project data definition constant reference for the given
   --  view.

   Get_RW : access function (View : in out Project.View.Object) return Ref;
   --  Returns the project data definition reference to modify view.

   Set : access procedure (View : Project.View.Object; Def : Data);
   --  Set the data definition for the view

   -----------------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Configuration package --
   -----------------------------------------------------------------------

   Bind_Configuration_To_Tree : access procedure
     (Config : in out Configuration.Object; Tree : access Project.Tree.Object);

end GPR2.Project.Definition;
