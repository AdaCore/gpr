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

   use type View.Object;

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

   type Data (Has_Context : Boolean) is
     new Definition_Base (Has_Context)
   with record
      Trees             : Tree;

      --  Actual values for the view
      Extended          : View.Object;
      Aggregate         : View.Object;
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
            or else (not Data.Context_View.Is_Defined
                     and then Data.Status = Root)
            or else Data.A_Context.Is_Empty;

   type Ref is access all Data;

   type Const_Ref is access constant Data;

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Tree package --
   --------------------------------------------------------------

   Register : access function
     (Def : in out Definition.Data) return Project.View.Object;
   --  Register view definition in the project tree

   Set : access procedure
     (Ref : out View.Object; Def : Definition_Base'Class);
   --  Convert definition to view to register

   Get : access function (View : Project.View.Object) return Ref;
   --  Returns the project data definition reference for the given view.
   --  This routine should be used only when we need to change view definition
   --  for the read only object.

   Get_RO : access function (View : Project.View.Object) return Const_Ref;
   --  Returns the project data definition constant reference for the given
   --  view.

   Get_RW : access function (View : in out Project.View.Object) return Ref;
   --  Returns the project data definition reference to modify view

   -----------------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Configuration package --
   -----------------------------------------------------------------------

   Bind_Configuration_To_Tree : access procedure
     (Config : in out Configuration.Object;
      Tree   : not null access Project.Tree.Object);

   -------------------------------------------
   -- Helper routines for GPR2.Project.View --
   -------------------------------------------

   function Has_Packages
     (Def : Data; Name : Optional_Name_Type) return Boolean
   is
     (if Name = No_Name
      then not Def.Packs.Is_Empty
      else Def.Packs.Contains (Name_Type (Name)));
   --  Returns true if the project view definition has some packages defined

   function Has_Types
     (Def : Data; Name : Optional_Name_Type) return Boolean
   is
     (if Name = No_Name
      then not Def.Types.Is_Empty
      else Def.Types.Contains (Name));

   function Has_Attributes
     (Def   : Data;
      Name  : Optional_Name_Type;
      Index : Value_Type := No_Value) return Boolean
   is
     (if Name = No_Name and then Index = No_Value then not Def.Attrs.Is_Empty
      elsif Index = No_Value then Def.Attrs.Contains (Name)
      else not Def.Attrs.Filter (Name, Index).Is_Empty);

   function Languages (Def : Data) return Containers.Value_List;
   --  Returns the languages used on this project, this is not necessary the
   --  content of the Languages attribute as if not defined it returns the
   --  default language Ada.

   function Source_Directories (Def : Data) return Project.Attribute.Object;
   --  Returns the sources dirs for the project view. This is only defined for
   --  project having sources. If not defined in the project itself, the view
   --  does have the project directory has source dir.

   function Naming_Package (Def : Data) return Project.Pack.Object;
   --  Returns the Naming package for the current view. This is either
   --  the view Naming package, the project's tree Naming package from the
   --  loaded configuration project if any and finally the default Naming
   --  package.

   procedure Update_Sources (Def : in out Data; View : Project.View.Object);
   --  Ensure that the view definition sources are up-to-date. This is needed
   --  before computing the dependencies of a source in the project tree. This
   --  routine is called where needed and is there for internal use only.

end GPR2.Project.Definition;
