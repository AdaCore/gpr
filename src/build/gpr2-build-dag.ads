--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Iterator_Interfaces;

with GPR2.Build.Actions;
with GPR2.Build.Artifacts;
with GPR2.Build.Artifact_Ids;

package GPR2.Build.DAG is

   type Object is tagged limited private
     with Constant_Indexing => Constant_Reference,
          Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Artifacts.Object'Class;
   --  DAG uses Artifact ids as node, but also stores the various actions
   --  and their input that produce a given node.

   procedure Hold (Self : access Object)
     with Pre => not Self.On_Hold;
   --  Prepares the DAG for artifact update sequence

   function On_Hold (Self : Object) return Boolean;
   --  Whether the DAG is on hold

   procedure Release (Self : access Object);
   --  Release the on_hold status and re-calculate the dag with updated changes

   procedure Register_Action
     (Self   : access Object;
      Action : Actions.Object'Class);

   procedure Add_Artifact
     (Self     : access Object;
      Artifact : Artifacts.Object'Class)
     with Pre => Self.On_Hold;

   procedure Update_Artifact
     (Self        : access Object;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id)
     with Pre => Has_Artifact (Self.all, Artifact)
                   and then Has_Artifact (Self.all, Predecessor)
                   and then Self.On_Hold;
   --  ??? Missing the context of the generation if any: need action parameter

   procedure Remove_Artifact
     (Self     : access Object;
      Artifact : Artifact_Ids.Artifact_Id)
     with Pre => Self.On_Hold;

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

   function Iterate
     (Self  : Object;
      Class : Artifact_Class := No_Artifact_Class)
      return Artifact_Iterators.Forward_Iterator'Class;

   function Predecessors
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id)
      return Artifact_Iterators.Forward_Iterator'Class;

   function Successors
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id)
      return Artifact_Iterators.Forward_Iterator'Class;

private

   use GPR2.Build.Artifact_Ids;

   package Artifact_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Artifact_Ids.Artifact_Id,
      Artifacts.Object'Class,
      "=" => Artifacts."=");

   package Artifact_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Artifact_Id);

   package Artifact_Class_Artifact_Set_Maps is new Ada.Containers.Ordered_Maps
     (Artifact_Class, Artifact_Sets.Set, "=" => Artifact_Sets."=");

   package Artifact_Artifact_Set_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type        => Artifact_Id,
        Element_Type    => Artifact_Sets.Set,
        "="             => Artifact_Sets."=");

   package Artifact_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Artifact_Id, "=");

   package Action_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Actions.Object'Class,
      Hash                => Actions.Hash,
      Equivalent_Elements => Actions."=",
      "="                 => Actions."=");

   package Artifact_Actions_Maps is new Ada.Containers.Ordered_Maps
     (Artifact_Class, Action_Sets.Set, "=" => Action_Sets."=");

   type Exec_Instance is record
      Action_Index : Positive;
      --  Index of the action in Actions_List field
      Inputs       : Artifact_Sets.Set;
      --  Inputs used to generate the output
   end record;

   package Exec_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Artifact_Id, Exec_Instance);

   type Object is tagged limited record
      Artifacts     : Artifact_Maps.Map;
      --  Stored artifacts

      Actions_List  : Artifact_Actions_Maps.Map;
      --  Maps Artifact_Class => Actions that can take an artifact of such
      --  class as input, to easily fetch possible actions for a newly
      --  inserted artifact.

      New_Artifacts : Artifact_Class_Artifact_Set_Maps.Map;
      --  List of newly inserted artifacts, may be used as input to generate
      --  other artifacts to populate the DAG.
      To_Remove     : Artifact_Sets.Set;
      --  List of artifacts that don't have predecessors anymore and should
      --  be removed.

      Executions    : Exec_Maps.Map;
      --  For each output, list the action that created it and it's input(s)

      --  DAG structure

      Predecessors  : Artifact_Artifact_Set_Maps.Map;
      --  For each node, list its predecessors
      Successors    : Artifact_Artifact_Set_Maps.Map;
      --  For each node, list its successors
      Sort          : Artifact_Vectors.Vector;
      --  Ordered list of the nodes
      Sort_Valid    : Boolean := True;
      --  Flag telling if the ordered list is valid or needs to be
      --  re-calculated
      On_Hold       : Boolean := False;
      --  Flag used to update the DAG in batch mode
   end record;

   function Has_Artifact
     (Self     : Object;
      Artifact : Artifact_Ids.Artifact_Id) return Boolean
   is (Self.Artifacts.Contains (Artifact));

   function Artifact
     (Self : Object;
      Artifact : Artifact_Ids.Artifact_Id) return Artifacts.Object'Class
   is (Self.Artifacts.Element (Artifact));

   function On_Hold (Self : Object) return Boolean
   is (Self.On_Hold);

   type Cursor_Kind is (Global_List, Pred_Succ);

   type Cursor (Kind : Cursor_Kind := Global_List) is record
      C   : Artifact_Maps.Cursor;

      case Kind is
         when Global_List =>
            null;
         when Pred_Succ =>
            C_A : Artifact_Sets.Cursor;
      end case;
   end record;

   No_Element : constant Cursor := (others => <>);

   type Constant_Reference_Type
     (Element : not null access constant Artifacts.Object'Class)
   is record
      Ref : Artifact_Maps.Constant_Reference_Type (Element);
   end record;

   type Reference_Type
     (Element : not null access Artifacts.Object'Class)
   is record
      Ref : Artifact_Maps.Reference_Type (Element);
   end record;

   function Element (Position : Cursor) return Artifacts.Object'Class
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
         when Pred_Succ =>
            Pos     : Artifact_Artifact_Set_Maps.Cursor;
      end case;
   end record;

   overriding function First (Self : Artifact_Iterator) return Cursor;

   overriding function Next
     (Self     : Artifact_Iterator;
      Position : Cursor) return Cursor;

end GPR2.Build.DAG;
