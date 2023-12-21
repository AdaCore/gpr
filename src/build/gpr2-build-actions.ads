--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers;
with Ada.Strings.Hash_Case_Insensitive;

with GPR2.Build.Artifact_Ids;
limited with GPR2.Build.DAG;

package GPR2.Build.Actions is

   type Action_Class is new Name_Type;

   type Object is interface;
   --  Meant to be a singleton

   function Class (Self : Object) return Action_Class is abstract;
   --  Human understandable identifier for a given action. For debug
   --  and display purpose.

   function Inputs (Self : Object) return Artifact_Class is abstract;
   --  The category of artifacts handled by this action.

   procedure Fill
     (Self  : Object;
      Graph : access DAG.Object;
      Input : Artifact_Ids.Artifact_Id) is abstract;
   --  Fill the DAG with Input. Action is responsible to tell the DAG the
   --  dependencies of the generated artifact as soon as it is known

   --  ??? TODO: the actual action execution

   function Hash (Action : Object'Class) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (String (Action.Class)));

end GPR2.Build.Actions;
