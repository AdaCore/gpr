--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps;

with GPR2.Build.Actions;
with GPR2.Build.Artifacts;
with GPR2.Build.Artifact_Ids;

package GPR2.Build.DAG is

   type DAG is private;
   --  DAG uses Artifact ids as node, but also stores the various actions
   --  and their input that produce a given node.

   procedure Register_Action
     (Self   : access DAG;
      Action : Actions.Object'Class);

   procedure Add_Artifact
     (Self     : access DAG;
      Artifact : Artifacts.Object'Class);

   procedure Update_Artifact
     (Self        : access DAG;
      Artifact    : Artifact_Ids.Artifact_Id;
      Predecessor : Artifact_Ids.Artifact_Id)
     with Pre => Has_Artifact (Self.all, Artifact)
                   and then Has_Artifact (Self.all, Predecessor);
   --  ??? Missing the context of the generation if any: need action parameter

   procedure Remove_Artifact
     (Self     : access DAG;
      Artifact : Artifact_Ids.Artifact_Id);

   function Has_Artifact
     (Self     : DAG;
      Artifact : Artifact_Ids.Artifact_Id) return Boolean;

   function Artifact
     (Self     : DAG;
      Artifact : Artifact_Ids.Artifact_Id) return Artifacts.Object'Class
     with Pre => Has_Artifact (Self, Artifact);

private

   use GPR2.Build.Artifact_Ids;

   package Artifact_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Artifact_Ids.Artifact_Id,
      Artifacts.Object'Class,
      Artifact_Ids.Hash,
      Artifact_Ids."=",
      Artifacts."=");

   package Artifact_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Artifact_Id, Hash, "=", "=");

   package Artifact_Artifact_Set_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Artifact_Id,
        Element_Type    => Artifact_Sets.Set,
        Hash            => Hash,
        Equivalent_Keys => "=",
        "="             => Artifact_Sets."=");

   package Artifact_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Artifact_Id, "=");

   package Action_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Actions.Object'Class,
      Hash                => Actions.Hash,
      Equivalent_Elements => Actions."=",
      "="                 => Actions."=");

   package Artifact_Actions_Maps is new Ada.Containers.Hashed_Maps
     (Artifact_Class, Action_Sets.Set, Hash, "=", Action_Sets."=");

   type Exec_Instance is record
      Action_Index : Positive;
      --  Index of the action in Actions_List field
      Inputs       : Artifact_Sets.Set;
      --  Inputs used to generate the output
   end record;

   package Exec_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Artifact_Id, Exec_Instance, Hash, "=");

   type DAG is record
      Artifacts    : Artifact_Maps.Map;
      --  Stored artifacts

      Actions_List : Artifact_Actions_Maps.Map;
      --  Maps Artifact_Class => Actions that can take an artifact of such
      --  class as input, to easily fetch possible actions for a newly
      --  inserted artifact.

      Executions   : Exec_Maps.Map;
      --  For each output, list the action that created it and it's input(s)

      --  DAG structure

      Predecessors : Artifact_Artifact_Set_Maps.Map;
      --  For each node, list its predecessors
      Successors   : Artifact_Artifact_Set_Maps.Map;
      --  For each node, list its successors
      Sort         : Artifact_Vectors.Vector;
      --  Ordered list of the nodes
      Sort_Valid   : Boolean := True;
      --  Flag telling if the ordered list is valid or needs to be
      --  re-calculated
   end record;

   function Has_Artifact
     (Self     : DAG;
      Artifact : Artifact_Ids.Artifact_Id) return Boolean
   is (Self.Artifacts.Contains (Artifact));

   function Artifact
     (Self : DAG;
      Artifact : Artifact_Ids.Artifact_Id) return Artifacts.Object'Class
   is (Self.Artifacts.Element (Artifact));

end GPR2.Build.DAG;
