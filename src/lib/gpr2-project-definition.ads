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

with Ada.Containers.Vectors;
with GNAT.MD5;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Parser.Project.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

limited with GPR2.Project.Tree;

private package GPR2.Project.Definition is

   use type View.Id;
   use type View.Object;
   use type Parser.Project.Object;
   use type Ada.Containers.Count_Type;

   --  Tree contains the Project parser object. This is shared by all projects
   --  view in all loaded tree. That is there is always a single instance of
   --  the project parser object.
   --  Imports contains the list of all imported projects for Project.

   type Tree is tagged record
      Project : Parser.Project.Object;
      Imports : Parser.Project.Set.Object;
   end record;

   package Project_View_Store is
     new Ada.Containers.Vectors (Positive, View.Object);

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
      Imports           : Project_View_Store.Vector;
      Aggregated        : Project_View_Store.Vector;
      Attrs             : Project.Attribute.Set.Object;
      Vars              : Project.Variable.Set.Object;
      Packs             : Project.Pack.Set.Object;
      Sources           : Project.Source.Set.Object;
      Sources_Signature : GNAT.MD5.Binary_Message_Digest;

      --  Some general information
      Context_View      : View.Object;
      Status            : Relation_Status;
      Kind              : Project_Kind;

      --  The project tree for this view
      Tree              : access Project.Tree.Object;

      case Has_Context is
         when True =>
            Context   : GPR2.Context.Object;
            A_Context : GPR2.Context.Object; -- Aggregate context
         when False =>
            null;
      end case;
   end record
     with Dynamic_Predicate =>
            --  Only a root-aggregate project can have a context defined via
            --  the External attribute.
            not Data.Has_Context
            or else Data.Context_View = View.Undefined
            or else Data.A_Context.Is_Empty;

   function Register (Def : Data) return View.Object
     with Pre  => Def.Trees.Project /= Parser.Project.Undefined,
          Post => Get (Register'Result) = Def;
   --  Register a new project definition, returns the corresponding view

   function Get (View : Project.View.Object) return Data
     with Post => Get'Result.Trees.Project /= Parser.Project.Undefined;
   --  Returns the project data definition for the given view

   procedure Set (View : Project.View.Object; Def : Data)
     with Pre  => Def.Trees.Project /= Parser.Project.Undefined,
          Post => Get (View) = Def;
   --  Set the project data defintion for the given view

end GPR2.Project.Definition;
