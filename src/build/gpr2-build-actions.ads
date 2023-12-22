--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers;

with GPR2.Build.Artifact_Ids;
limited with GPR2.Build.Tree_Db;

package GPR2.Build.Actions is

   type Object is interface;
   --  Actions object represent the links in our build graph. They represent
   --  the tools involved in the build process to generate artifacts from
   --  other artifacts.
   --
   --  In this regard, this object is responsible for informing the graph
   --  of all the implicit dependencies involved during the generation of
   --  the output.
   --
   --  To do so, actions are reported as working on a class of artifacts
   --  (see the `Inputs` property). For each such artifact in the tree
   --  database, `Fill` will then be called to add the dependencies.
   --
   --  This object is meant to be a singleton: only one instance is
   --  expected for a given run, and needs to be registered via
   --  `GPR2.Build.Tree_Db.Register_Action`.

   function Class (Self : Object) return Action_Class is abstract;
   --  Identifier for this action. Used for fast storage/retrieval, and can
   --  be used in debug output, so should remain explicit and human
   --  understandable.

   function Inputs (Self : Object) return Artifact_Class is abstract;
   --  The category of artifacts handled by this action.

   procedure Fill
     (Self  : Object;
      Graph : access Tree_Db.Object;
      Input : Artifact_Ids.Artifact_Id) is abstract;
   --  Fill the Database with artifacts that will be derived from Input
   --  due to the execution of Self. known explicit and implicit dependencies
   --  should also be filled.

   function Hash (Action : Object'Class) return Ada.Containers.Hash_Type;
   --  Used to store action objects in hashed containers.

private

   function Hash (Action : Object'Class) return Ada.Containers.Hash_Type is
     (Hash (Action.Class));

end GPR2.Build.Actions;
