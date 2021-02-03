------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Configuration;
with GPR2.Project.Pack.Set;
with GPR2.Project.Source.Set;
with GPR2.Project.Typ.Set;
with GPR2.Project.Variable.Set;
with GPR2.Project.View;
with GPR2.Project.Unit_Info.Set;
with GPR2.Source_Info;
with GPR2.Unit;
with GPR2.View_Ids;

limited with GPR2.Project.Tree;

private package GPR2.Project.Definition is

   use type Attribute_Index.Object;
   use type View.Object;
   use type Path_Name.Object;
   use type Source_Info.Backend_Set;

   --  Tree contains the Project parser object. This is shared by all projects
   --  view in all loaded trees. That is there is always a single instance of
   --  the project parser object.
   --  Imports contains the list of all imported projects for Project.

   type Tree is record
      Project  : Parser.Project.Object;
      Imports  : Parser.Project.Set.Object;
      Extended : Parser.Project.Object;
   end record;

   package Simple_Name_Source is
     new Ada.Containers.Indefinite_Ordered_Maps
       (Simple_Name, Project.Source.Object, "=" => Project.Source."=");
   --  Map to find in which view a unit/source is defined

   package Project_View_Store is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, View.Object);

   --  Data contains a project view data. We have all the attributes, variables
   --  and packages with the final values as parsed with the project's context
   --  in the given tree. Imports here are the project views corresponding to
   --  the imports in Trees.
   --
   --  Either a Data has a context or is referencing another containing
   --  the context. This is used for aggregate project which can be used to
   --  refine the global context by setting some external values with the
   --  corresponding attribute. So both the root project and all aggregate
   --  projects have a context. All other projects are referencing a project
   --  which own a context.

   type Data is new Definition_Base with record
      Trees       : Tree;

      --  Actual values for the view

      Extending       : Weak_Reference;
      Extended        : View.Object;
      Aggregate       : Weak_Reference;
      Imports         : Project_View_Store.Map;
      Limited_Imports : Project_View_Store.Map;
      Aggregated      : Project_View_Store.Map;
      Attrs           : Project.Attribute.Set.Object;
      Vars            : Project.Variable.Set.Object;
      Packs           : Project.Pack.Set.Object;
      Types           : Project.Typ.Set.Object;
      Sources         : Project.Source.Set.Object;
      Sources_Map     : Simple_Name_Source.Map;
      Units           : Unit_Info.Set.Object;
      Unique_Id       : GPR2.View_Ids.View_Id;
      Instance_Of     : GPR2.View_Ids.View_Id;
      Root_View       : Weak_Reference;
      --  Either root aggregated project view, or just root view of the tree

      --  Some general information

      Context : GPR2.Context.Context_Kind := GPR2.Context.Root;
      --  Use the aggregate context including External attributes or only the
      --  root context.

      Tree : access Project.Tree.Object;
      --  The project tree for this view

   end record;

   type Ref is access all Data;

   type Const_Ref is access constant Data;

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.Tree package --
   --------------------------------------------------------------

   Register : access function
     (Def : in out Definition.Data) return Project.View.Object;
   --  Register view definition in the project tree

   Check_Source : access function
     (View   : Project.View.Object;
      Name   : Simple_Name;
      Result : in out Project.Source.Object) return Boolean;
   --  Get the source object by the simple filename from the same projects
   --  subtree where the View is.
   --  Return True on success and set Result.
   --  Return False if source not found and remain Result untouched.

   Check_Source_Unit : access function
     (View   : Project.View.Object;
      Unit   : GPR2.Unit.Object;
      Result : in out Source.Object) return Boolean;
   --  Get the source object by the unit from the same projects subtree where
   --  the View is.
   --  Return True on success and set Result.
   --  Return False if source not found and remain Result untouched.

   Has_Source : access function
     (View : Project.View.Object; Name : Simple_Name) return Boolean;
   --  Return True if source with such filename found in project namespace
   --  subtree.

   Set_Source : access procedure (Source : Project.Source.Object);
   --  Insert source into internal Tree container indexed by Root of subtree
   --  project name and simple source filename.

   Remove_Source : access procedure (Source : Project.Source.Object);
   --  Remove Source from internal Tree container indexed by Root of subtree
   --  project name and simple source filename.

   Get_Context : access function
     (View : Project.View.Object) return Context.Object;
   --  Returns context of the project view

   --------------------------------------------------------------
   -- Private routines exported from GPR2.Project.View package --
   --------------------------------------------------------------

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

   Refcount : access function (View : Project.View.Object) return Natural;
   --  Returns reference counter of the view

   Weak : access function (View : Project.View.Object) return Weak_Reference;
   --  Get weak reference from view. Need to avoid circular references

   Strong : access function (View : Weak_Reference) return Project.View.Object;
   --  Get view from weak reference

   Set_Pack_Default_Attributes : access procedure
     (Self : in out Pack.Object; VDD : Definition.Data);

   Set_Defaults : access procedure
     (Self : in out Attribute.Set.Object;
      VDD  : Data;
      Pack : Optional_Name_Type);

   Change_Actual_View : access function
     (Self : Source.Object; View : Project.View.Object) return Source.Object;

   Apply_Root_And_Subdirs : access function
     (Self : View.Object; Dir_Attr : Name_Type) return GPR2.Path_Name.Object;

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
     (Def  : Data;
      Name : Optional_Name_Type) return Boolean
   is
     (if Name = No_Name
      then not Def.Packs.Is_Empty
      else Def.Packs.Contains (Name_Type (Name)));
   --  Returns true if the project view definition has some packages defined

   function Has_Types
     (Def  : Data;
      Name : Optional_Name_Type) return Boolean
   is
     (if Name = No_Name
      then not Def.Types.Is_Empty
      else Def.Types.Contains (Name));

   function Has_Attributes
     (Def   : Data;
      Name  : Optional_Name_Type;
      Index : Attribute_Index.Object := Attribute_Index.Undefined)
      return Boolean
   is
     (if Name = No_Name and then Index = Attribute_Index.Undefined
      then not Def.Attrs.Is_Empty
      elsif Index = Attribute_Index.Undefined
      then Def.Attrs.Contains (Name)
      else not Def.Attrs.Filter (Name, Index).Is_Empty);

   function Languages (Def : Data) return Containers.Source_Value_List;
   --  Returns the languages used on this project, this is not necessary the
   --  content of the Languages attribute as if not defined it returns the
   --  default language Ada.

   function Naming_Package (Def : Data) return Project.Pack.Object;
   --  Returns the Naming package for the current view. This is either
   --  the view Naming package, the project's tree Naming package from the
   --  loaded configuration project if any and finally the default Naming
   --  package.

   procedure Update_Sources
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean;
      Backends      : Source_Info.Backend_Set)
     with Pre => View.Is_Defined and then Backends /= Source_Info.No_Backends;
   --  Ensure that the view definition sources are up-to-date. This is needed
   --  before computing the dependencies of a source in the project tree. This
   --  routine is called where needed and is there for internal use only.
   --  If Stop_On_Error is True and an error occurred on reading the sources,
   --  then the exception Project_Error raised. If Stop_On_Error is False then
   --  no exception is raised and errors can be discovered only from the
   --  Log.Object taken from the View.Tree.Log_Messages call.
   --  Backends parameter defines the set of parser that can be used to parse
   --  the source information.

   procedure Set_Default_Attributes (Def : in out Data);
   --  Set default and inherited attributes for the project view

   function Check_Circular_References
     (View : Project.View.Object) return Boolean;
   --  Check that references between View.Object does not create cycles.
   --  Either returns True or raises exception about found reference circle.
   --  Return value need to use this function in pragma Assert.

   procedure Sources_Map_Insert
     (Def : in out Data;
      Src : Project.Source.Object);
   --  Insert source into simple filename index if it is not yet inserted

   function Is_Sources_Loaded (View : Project.View.Object) return Boolean
     with Pre => View.Is_Defined;
   --  Return True if Sources already updated once

   procedure Check_Same_Name_Extended (View : Project.View.Object);
   --  Report "cannot extend a project with the same name" errors

   procedure Check_Aggregate_Library_Dirs (View : Project.View.Object);
   --  Report aggregate library (ALI)? directory cannot be shared with
   --  (object|library) directory of aggregated project errors

   procedure Check_Package_Naming (View : Project.View.Object);
   --  For all tree's views check Casing, Dot_Replacement, Spec_Suffix,
   --  Body_Suffix and Separate_Suffix naming package attributes value.

end GPR2.Project.Definition;
