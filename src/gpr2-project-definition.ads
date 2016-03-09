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

with Ada.Containers.Vectors;

with GPR2.Context;
with GPR2.Parser.Project.Set;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Pack.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;

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

   --  Data contains a project view data. We have all the attributes, variables
   --  and pakcages with the final values as parsed with the project's context
   --  in the given tree. Imports here are the project views corresponding to
   --  the imports in Trees.

   type Data is tagged record
      Trees   : Tree;
      Sig     : Context.Binary_Signature;
      Imports : Project_View_Store.Vector;
      Attrs   : Project.Attribute.Set.Object;
      Vars    : Project.Variable.Set.Object;
      Packs   : Project.Pack.Set.Object;
   end record;

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
