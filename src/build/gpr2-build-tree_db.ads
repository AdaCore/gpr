--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Iterator_Interfaces;

with GPR2.Build.Actions;
with GPR2.Build.Artifacts;
with GPR2.Build.View_Db;
with GPR2.Log;
limited with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.View_Ids;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;

package GPR2.Build.Tree_Db is

   type Object is tagged limited private;
   type Object_Access is access all Object;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Tree (Self : Object) return access GPR2.Project.Tree.Object
     with Pre => Self.Is_Defined;

   --  CREATING/UNLOADING/REFRESHING THE TREE DATABASE:

   procedure Create
     (Self                 : in out Object;
      Tree                 : GPR2.Project.Tree.Object;
      With_Runtime_Sources : Boolean)
     with Pre => not Self.Is_Defined;
   --  Initializes the object.
   --  The artifacts are not loaded at this stage, Refresh needs to be called
   --  With_Runtime_Sources indicates whether the database should consider the
   --  sources of the Ada runtime attached to the tree or not.

   procedure Check_Tree (Self : in out Object)
     with Pre => Self.Is_Defined;
   --  Called when the project tree is updated to check that internal
   --  database structure is updated accordingly (proper view databases
   --  added or removed when appropriate).

   function Use_Runtime_Sources (Self : Object) return Boolean;
   --  Whether sources of the runtime are considered

   procedure Unload (Self : in out Object)
     with Post => not Self.Is_Defined;

   procedure Refresh
     (Self     : in out Object;
      Option   : Source_Info_Option;
      Messages : out GPR2.Log.Object)
     with Pre => Self.Is_Defined;

   function Source_Option (Self : Object) return Optional_Source_Info_Option;

   --  VIEW DATABASE LOOKUP:

   function View_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object
     with Pre => Self.Is_Defined
                   and then View.Is_Defined
                   and then View.Kind in With_Object_Dir_Kind,
          Inline;

   function View_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object
     with Pre => Self.Is_Defined,
          Inline;

   function Ref (Self : Object) return access Object
     with Pre => Self.Is_Defined;

   --  BUILD GRAPH SUPPORT

   procedure Add_Action
     (Self     : in out Object;
      Action   : Actions.Object'Class;
      Messages : in out GPR2.Log.Object)
     with Pre => Self.Is_Defined and then not Self.Has_Action (Action.UID);

   function Has_Action
     (Self : Object;
      Id   : Actions.Action_Id'Class) return Boolean
     with Pre => Self.Is_Defined;

   procedure Add_Artifact
     (Self : in out Object;
      Artifact : Artifacts.Object'Class)
     with Pre => Self.Is_Defined;

   function Has_Artifact
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Boolean;

   procedure Add_Input
     (Self     : in out Object;
      Action   : Actions.Action_Id'Class;
      Artifact : Artifacts.Object'Class;
      Explicit : Boolean)
     with Pre => Self.Is_Defined and then Self.Has_Action (Action);

   procedure Add_Output
     (Self     : in out Object;
      Action   : Actions.Action_Id'Class;
      Artifact : Artifacts.Object'Class;
      Messages : in out GPR2.Log.Object)
     with Pre => Self.Is_Defined and then Self.Has_Action (Action);

   ----------------------------
   -- Iteration on artifacts --
   ----------------------------

   type Artifact_Cursor is private;

   No_Artifact_Element : constant Artifact_Cursor;

   function Has_Element (Position : Artifact_Cursor) return Boolean;

   package Artifact_Iterators is new Ada.Iterator_Interfaces
     (Artifact_Cursor, Has_Element);

   type Artifacts_List (<>) is tagged private
     with Default_Iterator  => Artifact_Iterate,
          Iterator_Element  => Artifacts.Object'Class,
          Constant_Indexing => Constant_Artifact_Reference;

   function Artifact_Iterate
     (List : Artifacts_List) return Artifact_Iterators.Forward_Iterator'Class;

   type Constant_Artifact_Reference_Type
     (Element : not null access constant Artifacts.Object'Class) is private
     with Implicit_Dereference => Element;

   function Constant_Artifact_Reference
     (Iterator : aliased Artifacts_List;
      Pos      : Artifact_Cursor)
      return Constant_Artifact_Reference_Type;

   type Action_Cursor is private;
   No_Action_Element : constant Action_Cursor;

   function Has_Element (Position : Action_Cursor) return Boolean;

   package Action_Iterators is new Ada.Iterator_Interfaces
     (Action_Cursor, Has_Element);

   type Actions_List (<>) is tagged private
     with Default_Iterator  => Action_Iterate,
          Iterator_Element  => Actions.Object'Class,
          Constant_Indexing => Constant_Action_Reference,
          Variable_Indexing => Action_Reference;

   function Action_Iterate
     (List : Actions_List) return Action_Iterators.Forward_Iterator'Class;

   type Action_Reference_Type
     (Element : not null access Actions.Object'Class) is private
     with Implicit_Dereference => Element;

   function Action_Reference
     (Iterator : access Actions_List;
      Pos      : Action_Cursor) return Action_Reference_Type;

   type Constant_Action_Reference_Type
     (Element : not null access constant Actions.Object'Class) is private
     with Implicit_Dereference => Element;

   function Constant_Action_Reference
     (Iterator : aliased Actions_List;
      Pos      : Action_Cursor)
      return Constant_Action_Reference_Type;

   function All_Actions (Self : Object) return Actions_List'Class;

      function Inputs
        (Self : Object;
         Action : Actions.Action_Id'Class) return Artifacts_List'Class;

      function Outputs
        (Self : Object;
         Action : Actions.Action_Id'Class) return Artifacts_List'Class;

private

   function Hash (A : Artifacts.Object'Class) return Ada.Containers.Hash_Type
   is (A.Hash);

   package Build_DB_Maps is new Ada.Containers.Hashed_Maps
     (GPR2.View_Ids.View_Id, Build.View_Db.Object,
      GPR2.View_Ids.Hash, GPR2.View_Ids."=", Build.View_Db."=");

   package Action_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (GPR2.Build.Actions.Action_Id'Class, GPR2.Build.Actions.Object'Class,
      GPR2.Build.Actions.Less, GPR2.Build.Actions."=");

   package Artifact_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (GPR2.Build.Artifacts.Object'Class,
      GPR2.Build.Artifacts.Less, GPR2.Build.Artifacts."=");

   package Action_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (GPR2.Build.Actions.Action_Id'Class,
      GPR2.Build.Actions.Less, GPR2.Build.Actions."=");

   package Action_Artifacts_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (GPR2.Build.Actions.Action_Id'Class, Artifact_Sets.Set,
      GPR2.Build.Actions.Less, Artifact_Sets."=");

   package Artifact_Actions_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (GPR2.Build.Artifacts.Object'Class, Action_Sets.Set, Hash,
      GPR2.Build.Artifacts."=", Action_Sets."=");

   package Artifact_Action_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (GPR2.Build.Artifacts.Object'Class, Actions.Action_Id'Class, Hash,
      GPR2.Build.Artifacts."=", Actions."=");

   type Object is tagged limited record
   --  Options:
      Src_Option      : Optional_Source_Info_Option := No_Source;
      With_RTS        : Boolean := False;

      Self            : access Object;
      --  Handy self-reference

      Tree            : access GPR2.Project.Tree.Object;
      --  The project tree

      Build_Dbs       : Build_DB_Maps.Map;
      --  Distributed database objects sources from the views

      Actions         : Action_Maps.Map;
      Artifacts       : Artifact_Sets.Set;

      Inputs          : Action_Artifacts_Maps.Map;
      --  Explicit input(s) in the command line
      Implicit_Inputs : Action_Artifacts_Maps.Map;
      --  Implicit input(s): included by the explicit ones.
      Outputs         : Action_Artifacts_Maps.Map;
      --  Artifacts produced by a given action

      Successors      : Artifact_Actions_Maps.Map;
      Predecessor     : Artifact_Action_Maps.Map;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self.Self /= null);

   function Ref (Self : Object) return access Object is
     (Self.Self);

   function Use_Runtime_Sources (Self : Object) return Boolean is
     (Self.With_RTS);

   function Source_Option (Self : Object) return Optional_Source_Info_Option is
     (Self.Src_Option);

   function View_Database
     (Self : Object; View : GPR2.Project.View.Object)
      return Build.View_Db.Object
   is (Self.Build_Dbs.Element (View.Id));

   function View_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object
   is (Self.Build_Dbs.Element (View));

   function Has_Action
     (Self : Object;
      Id   : Actions.Action_Id'Class) return Boolean
   is (Self.Actions.Contains (Id));

   function Has_Artifact
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Boolean
   is (Self.Artifacts.Contains (Artifact));

   type Artifact_List_Kind is (Global_List,
                               Explicit_Inputs,
                               Implicit_Inputs,
                               Inputs,
                               Outputs);

   type Artifact_Cursor is record
      Pos     : Artifact_Sets.Cursor;
      --  Cursor to the artifact object
      Map_Pos : Action_Artifacts_Maps.Cursor;
      --  Cursor to the action->artifacts map element that contains Pos
      Current : Artifact_List_Kind := Global_List;
      --  If Kind is Inputs, this field is used to know if we're currently on
      --  the explicit or implicit list
   end record;

   No_Artifact_Element : constant Artifact_Cursor := (others => <>);

   function Has_Element (Position : Artifact_Cursor) return Boolean
   is (Artifact_Sets.Has_Element (Position.Pos));

   type Constant_Artifact_Reference_Type
     (Element : not null access constant Artifacts.Object'Class)
   is record
      Ref : Artifact_Sets.Constant_Reference_Type (Element);
   end record;

   type Artifacts_List (Kind : Artifact_List_Kind) is tagged record
      Db : access Object;

      case Kind is
         when Global_List =>
            null;
         when others =>
            Action : Action_Maps.Cursor;
      end case;
   end record;

   type Action_Cursor is record
      Pos     : Action_Maps.Cursor;
      Set_Pos : Action_Sets.Cursor;
   end record;

   No_Action_Element : constant Action_Cursor := (others => <>);

   function Has_Element (Position : Action_Cursor) return Boolean
   is (Action_Maps.Has_Element (Position.Pos));

   type Action_Reference_Type
     (Element : not null access Actions.Object'Class)
   is record
      Ref : Action_Maps.Reference_Type (Element);
   end record;

   type Constant_Action_Reference_Type
     (Element : not null access constant Actions.Object'Class)
   is record
      Ref : Action_Maps.Constant_Reference_Type (Element);
   end record;

   type Action_List_Kind is (Global_List,
                             Successors);

   type Actions_List (Kind : Action_List_Kind) is tagged record
      Db : access Object;

      case Kind is
         when Global_List =>
            null;
         when others =>
            Artifact : Artifact_Sets.Cursor;
      end case;
   end record;

   function All_Actions (Self : Object) return Actions_List'Class is
     (Actions_List'(Kind => Global_List, Db => Self.Self));

   function Inputs
     (Self : Object;
      Action : Actions.Action_Id'Class) return Artifacts_List'Class
   is (Artifacts_List'
         (Kind   => Inputs,
          Db     => Self.Self,
          Action => Self.Actions.Find (Action)));

   function Outputs
     (Self   : Object;
      Action : Actions.Action_Id'Class) return Artifacts_List'Class
   is (Artifacts_List'
         (Kind   => Outputs,
          Db     => Self.Self,
          Action => Self.Actions.Find (Action)));

end GPR2.Build.Tree_Db;
