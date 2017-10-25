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

with Ada.Containers.Indefinite_Ordered_Maps;

with GNAT.MD5;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Parser.Project.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Source.Set;
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

   type Relation_Status is (Root, Imported, Aggregated);

   --  Data contains a project view data. We have all the attributes, variables
   --  and pakcages with the final values as parsed with the project's context
   --  in the given tree. Imports here are the project views corresponding to
   --  the imports in Trees.
   --
   --  Either a Data has a context or is referencing another with containing
   --  the context. This is used for aggregate project which can be used to
   --  refine the global context by setting some external values with the
   --  corresponding attribute. So both the root project and all aggregate
   --  projects have a context. All other projects are referencing a project
   --  which own a context.

   type Data (Has_Context : Boolean) is tagged record
      Trees             : Tree;
      Externals         : Containers.Name_List;
      --  List of externals directly or indirectly visible
      Signature         : Context.Binary_Signature;

      --  Actual values for the view
      Extended          : View.Object;
      Imports           : Project_View_Store.Map;
      Aggregated        : Project_View_Store.Map;
      Attrs             : Project.Attribute.Set.Object;
      Vars              : Project.Variable.Set.Object;
      Packs             : Project.Pack.Set.Object;

      Sources           : Project.Source.Set.Object;
      Sources_Signature : GNAT.MD5.Binary_Message_Digest;

      Units             : Unit.Set.Object;

      --  Some general information
      Context_View      : View.Object;
      --  The context view is the view that has context for this project. That
      --  is, Context_View will always point to a view with a context, either
      --  the Undefined view (means root project) which contains the context
      --  from the running environment or an aggregate project which has
      --  a context from the External attribute. Undefined is used for the
      --  root view to differenciate a root context from a root and aggregate
      --  project.

      Status            : Relation_Status;
      Kind              : Project_Kind;

      --  The project tree for this view
      Tree              : access Project.Tree.Object;

      case Has_Context is
         when True =>
            Context   : GPR2.Context.Object; -- root context
            A_Context : GPR2.Context.Object; -- aggregate context
         when False =>
            null;
      end case;
   end record
     with Dynamic_Predicate =>
            --  Only a root-aggregate project can have a context defined via
            --  the External attribute.
            not Data.Has_Context
            or else (Data.Context_View = View.Undefined
                     and then Data.Status = Root)
            or else Data.A_Context.Is_Empty;

   function Register (Def : Data) return View.Object
     with Pre  => Def.Trees.Project /= Parser.Project.Undefined
                    and then
                  (Def.Kind = K_Configuration or else Def.Tree /= null),
          Post => Get (Register'Result) = Def;
   --  Register a new project definition, returns the corresponding view

   procedure Unregister (View : Project.View.Object)
     with Pre  => Get (View).Trees.Project /= Parser.Project.Undefined,
          Post => Get (View).Tree = null;

   function Get (View : Project.View.Object) return Data
     with Post => Get'Result.Trees.Project /= Parser.Project.Undefined;
   --  Returns the project data definition for the given view

   function Get
     (Path_Name    : Path_Name_Type;
      Context_View : GPR2.Project.View.Object;
      Status       : Relation_Status;
      Tree         : GPR2.Project.Tree.Object) return Project.View.Object;
   --  Returns the project view corresponding to Path_Name, Status and
   --  Context_View in the given Tree or Undefined if this project is not
   --  yet registered.

   function Get
     (Name         : Name_Type;
      Context_View : Project.View.Object;
      Tree         : GPR2.Project.Tree.Object) return Project.View.Object;
   --  Returns the project view corresponding to Name and Context in the given
   --  Tree or Undefined if this project is not yet registered.

   function Get
     (View : Project.View.Object;
      Name : Name_Type) return Project.View.Object;
   --  Returns the project view corresponding to Name and found in the context
   --  of View (e.g. imported or extended).

   procedure Set (View : Project.View.Object; Def : Data)
     with Pre  => Def.Trees.Project /= Parser.Project.Undefined,
          Post => Get (View) = Def;
   --  Set the project data defintion for the given view

end GPR2.Project.Definition;
