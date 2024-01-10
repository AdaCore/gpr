--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Iterator_Interfaces;

with GPR2.Build.Actions;
with GPR2.Build.Artifacts;
with GPR2.Build.Artifact_Ids;
with GPR2.Build.View_Db;
with GPR2.Log;
limited with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.View_Ids;

private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Ordered_Sets;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Ordered_Maps;

package GPR2.Build.Tree_Db is

   type Object is tagged limited private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference;
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

   function Refreshing (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Changing artefacts in the Tree_Db is only allowed when the database
   --  is refreshing, so the state is set during call to `Refresh` above and
   --  reset before returning.

   function Source_Option (Self : Object) return Optional_Source_Info_Option;

   --  VIEW DATABASE LOOKUP:

   function View_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object
     with Pre => Self.Is_Defined
                   and then View.Is_Defined
     and then View.Kind in With_Object_Dir_Kind;

   --  BUILD ARTIFACTS HANDLING

   procedure Register_Action
     (Self   : access Object;
      Action : Actions.Object'Class);

   procedure Add_Artifact
     (Self     : access Object;
      Artifact : Artifacts.Object'Class)
     with Pre => Self.Refreshing
                   and then not Self.Has_Artifact (Artifact.Id);

   procedure Add_Implicit_Dependency
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id)
     with Pre => Has_Artifact (Self.all, Artifact)
                   and then Has_Artifact (Self.all, Predecessor)
                   and then Self.Refreshing;
   --  Add a simple dependency: one that is an implicit dependency for an
   --  action. This differs from the direct dependency where Predecessor is
   --  used as explicit input for an action (see below).

   procedure Add_Explicit_Dependency
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id)
     with Pre => Has_Artifact (Self.all, Artifact)
                   and then Has_Artifact (Self.all, Predecessor)
                   and then Self.Refreshing;
   --  Add an explicit dependency: action uses `Predecessor` as input to
   --  generate `Artifact`. Several inputs can be specified by calling
   --  Add_Explicit_Dependency several times

   procedure Remove_Artifact
     (Self     : access Object;
      Artifact : Artifact_Ids.Artifact_Id)
     with Pre => Self.Refreshing;

   function Has_Artifact
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id) return Boolean;

   function Artifact
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id) return Artifacts.Object'Class
     with Pre => Has_Artifact (Self, Artifact);

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return Artifacts.Object'Class;

   function Has_Element (Position : Cursor) return Boolean;

   package Artifact_Iterators is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
     (Element : not null access constant Artifacts.Object'Class) is private
     with Implicit_Dereference => Element;

   type Reference_Type
     (Element : not null access Artifacts.Object'Class) is private
     with Implicit_Dereference => Element;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type;

   function Reference
     (Self     : access Object;
      Position : Cursor) return Reference_Type;

   function Reference
     (Self : access Object;
      Id   : Artifact_Ids.Artifact_Id) return Reference_Type;

   function Is_Empty (Self : Object) return Boolean;

   -- Iterating the DAG --

   type Iterator (<>) is tagged private
     with Variable_Indexing => Iter_Reference,
          Constant_Indexing => Iter_Constant_Reference,
          Default_Iterator   => Iterate,
          Iterator_Element   => Build.Artifacts.Object'Class;

   function Iterate (Self  : Iterator)
      return Artifact_Iterators.Forward_Iterator'Class;

   function Iter_Constant_Reference
     (Self     : aliased Iterator;
      Position : Cursor) return Constant_Reference_Type;

   function Iter_Reference
     (Self     : access Iterator;
      Position : Cursor) return Reference_Type;

   function Artifacts
     (Self  : Object;
      Class : Artifact_Class := No_Artifact_Class)
      return Iterator'Class;

   function Predecessors
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id)
      return Iterator'Class;

   function Successors
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id)
      return Iterator'Class;

   function Ref (Self : Object) return access Object
     with Pre => Self.Is_Defined;

private

   use GPR2.Build.Artifact_Ids;

   package Build_DB_Maps is new Ada.Containers.Hashed_Maps
     (GPR2.View_Ids.View_Id, Build.View_Db.Object,
      GPR2.View_Ids.Hash, GPR2.View_Ids."=", Build.View_Db."=");

   package Artifact_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Artifact_Ids.Artifact_Id,
      Build.Artifacts.Object'Class,
      "=" => Build.Artifacts."=");

   package Artifact_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Artifact_Id);

   package Artifact_Class_Artifact_Set_Maps is new Ada.Containers.Ordered_Maps
     (Artifact_Class, Artifact_Sets.Set, "=" => Artifact_Sets."=");

   type Dependency_Kind is (Explicit, Implicit);
   --  An explicit dependency is an artifact used as direct input to an action
   --  while implicit dependencies are artifacts whose direct inputs depend
   --  on to generate the result (for example header files when compiling a
   --  C source).

   type Dependencies_List is array (Dependency_Kind) of Artifact_Sets.Set;

   type Dependencies is record
      Inputs : Dependencies_List;
      --  list of all the dependencies for an output artifact
      Action : Action_Class := No_Action_Class;
      --  refers to an action object in the Actions_List field: action used
      --  to generate the artifact from the explicit list of inputs and
      --  implicitly the Implicit inputs
   end record;

   package Artifact_Dependency_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => Artifact_Id,
        Element_Type    => Dependencies);
   --  Links an output with its inputs

   package Artifact_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Artifact_Id, "=");

   package Action_Class_Action_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Action_Class,
        Element_Type    => Actions.Object'Class,
        Hash            => Hash,
        Equivalent_Keys => "=",
        "="             => Actions."=");

   package Action_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Actions.Object'Class,
      Hash                => Actions.Hash,
      Equivalent_Elements => Actions."=",
      "="                 => Actions."=");

   package Artifact_Actions_Maps is new Ada.Containers.Ordered_Maps
     (Artifact_Class, Action_Sets.Set, "=" => Action_Sets."=");

   type Object is tagged limited record
      --  Options:
      Src_Option : Optional_Source_Info_Option := No_Source;
      With_RTS   : Boolean := False;

      Self       : access Object;
      --  Handy self-reference

      Tree       : access GPR2.Project.Tree.Object;
      --  The project tree

      Build_Dbs  : Build_DB_Maps.Map;
      --  Distributed database objects sources from the views

      Artifacts     : Artifact_Maps.Map;
      --  Stored artifacts

      Actions_List  : Action_Class_Action_Maps.Map;
      --  List of actions indexed by their action class id

      Todo_List     : Artifact_Actions_Maps.Map;
      --  Maps Artifact_Class => Actions that can take an artifact of such
      --  class as input, to easily fetch possible actions for a newly
      --  inserted artifact.

      New_Artifacts : Artifact_Class_Artifact_Set_Maps.Map;
      --  List of newly inserted artifacts, may be used as input to generate
      --  other artifacts to populate the DAG.
      To_Remove     : Artifact_Sets.Set;
      --  List of artifacts that don't have predecessors anymore and should
      --  be removed.
      Current_Action : Action_Class := No_Action_Class;

      --  DAG structure

      Predecessors  : Artifact_Dependency_Maps.Map;
      --  For each node, list its predecessors
      Successors    : Artifact_Dependency_Maps.Map;
      --  For each node, list its successors
      Sort          : Artifact_Vectors.Vector;
      --  Ordered list of the nodes
      Sort_Valid    : Boolean := True;
      --  Flag telling if the ordered list is valid or needs to be
      --  re-calculated
      On_Hold       : Boolean := False;
      --  Flag used to update the DAG in batch mode
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

   function Has_Artifact
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id) return Boolean
   is (Self.Artifacts.Contains (Artifact));

   function Artifact
     (Self : Object;
      Artifact : Artifact_Ids.Artifact_Id) return Build.Artifacts.Object'Class
   is (Self.Artifacts.Element (Artifact));

   function Refreshing (Self : Object) return Boolean
   is (Self.On_Hold);

   type Cursor_Kind is (Global_List, Predecessors, Successors);

   type Dependencies_Cursor is
     array (Dependency_Kind) of Artifact_Sets.Cursor;

   type Cursor (Kind : Cursor_Kind := Global_List) is record
      C   : Artifact_Maps.Cursor;

      case Kind is
         when Global_List =>
            null;
         when Predecessors | Successors =>
            C_Art   : Dependencies_Cursor;
            Current : Dependency_Kind := Dependency_Kind'First;
      end case;
   end record;

   No_Element : constant Cursor := (others => <>);

   type Constant_Reference_Type
     (Element : not null access constant Build.Artifacts.Object'Class)
   is record
      Ref : Artifact_Maps.Constant_Reference_Type (Element);
   end record;

   type Reference_Type
     (Element : not null access Build.Artifacts.Object'Class)
   is record
      Ref : Artifact_Maps.Reference_Type (Element);
   end record;

   function Element (Position : Cursor) return Build.Artifacts.Object'Class
   is (Artifact_Maps.Element (Position.C));

   function Has_Element (Position : Cursor) return Boolean
   is (Artifact_Maps.Has_Element (Position.C));

   function Is_Empty (Self : Object) return Boolean
   is (Self.Artifacts.Is_Empty);

   type Artifact_Iterator (Kind : Cursor_Kind) is
     new Artifact_Iterators.Forward_Iterator
   with record
      Graph : access Object;
      case Kind is
         when Global_List =>
            Class : Artifact_Class := No_Artifact_Class;
         when Predecessors | Successors =>
            Pos   : Artifact_Dependency_Maps.Cursor;
      end case;
   end record;

   overriding function First (Self : Artifact_Iterator) return Cursor;

   overriding function Next
     (Self     : Artifact_Iterator;
      Position : Cursor) return Cursor;

   type Iterator (Kind : Cursor_Kind) is tagged record
      Actual : Artifact_Iterator (Kind);
   end record;

   function Iterate
     (Self  : Iterator) return Artifact_Iterators.Forward_Iterator'Class
   is (Self.Actual);

end GPR2.Build.Tree_Db;
