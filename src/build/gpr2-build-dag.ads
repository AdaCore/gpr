--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

--  The Direct Acyclic Graph structure of the build database is at the heart
--  of the traceability engine that drives the builds (and more generally the
--  gpr tools actions) with regards to the artifacts detected via the GPR
--  project tree.
--
--  The nodes of this DAG are the artifacts, the links the actions. Each action
--  genernates a signature that is saved and then checked to see if it needs to
--  be replayed.

with Ada.Containers.Indefinite_Hashed_Sets;

with GPR2.Log;
with GPR2.Project.View;

package GPR2.Build.DAG is

   type Signature is interface;
   --  Represents the signature of an action: the summary of all external
   --  artifacts involved in an action to produce a new artifact.
   --  The signature is saved with the produced artifact to check if it needs
   --  to be re-created if its dependencies change.

   type Checksum_Type is interface;
   --  ??? would be nice to define a definite type here, like some hash format.

   function Checksum
     (Self : Signature) return Checksum_Type'Class is abstract;

   procedure Report_Diff
     (Self     : Signature;
      Other    : Signature;
      Messages : in out GPR2.Log.Object) is abstract;
   --  Report why the checksum between Self and Other differ

   type Artifact is interface;
   --  An artifact is a compilation element, be it concrete (such as sources
   --  or object files) or more abstract such as toolchain identifier (kind
   --  and version). For GPR's build database DAG, it is the node of the
   --  direct acyclic graph.

   function View (Self : Artifact) return GPR2.Project.View.Object is abstract;

   function Hash (Self : Artifact) return Ada.Containers.Hash_Type is abstract;
   --  To store in hashed containers

   function Class_Hash (Self : Artifact'Class) return Ada.Containers.Hash_Type;
   --  Hash of the artifact\s path name, to store the object in containers

   package Artifact_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Artifact'Class, Class_Hash, "=");

   type Action is interface;

   function Generates
     (Self  : Action;
      Input : Artifact'Class)
      return Artifact_Sets.Set is abstract;
   --  Return a set of artifacts that Self will generate from Input.

   function Dependencies
     (Self   : Action;
      Output : Artifact'Class)
      return Artifact_Sets.Set is abstract;
   --  Return a list of artifacts that were used to generate the Output.
   --  Important note: Output may not be present on the disk when
   --  this function is called, in this case the set of predictable input
   --  is given (for example foo.c for a compilation in C generating foo.o).
   --  Once the action is done (so output is generated), it is the
   --  responsibility of the action to store the actual dependencies, and the
   --  state of the various inputs.

   procedure Execute
     (Self    : Action;
      Input   : Artifact'Class;
      Stamp   : out Signature'Class;
      Success : out Boolean) is abstract;

   type Artifact_Id is private;
   No_Id : constant Artifact_Id;

   type Artifact_Class is private;
   No_Class : constant Artifact_Class;

   type Object is tagged limited private;

   procedure Register_Action
     (Self  : access Object;
      Input : Artifact_Class;
      Value : Action'Class);

   function Register_Artifact_Class
     (Self       : access Object;
      Class_Name : String) return Artifact_Class;

   function Register_Artifact
     (Self   : access Object;
      Class  : Artifact_Class;
      Value  : Artifact'Class) return Artifact_Id;

private

   function Class_Hash (Self : Artifact'Class) return Ada.Containers.Hash_Type
     is (Self.Hash);

   type Artifact_Id is new Natural with Default_Value => 0;
   No_Id : constant Artifact_Id := 0;

   type Artifact_Class is new Natural with Default_Value => 0;
   No_Class : constant Artifact_Class := 0;

   type Object is tagged limited record
      null;
   end record;

end GPR2.Build.DAG;
