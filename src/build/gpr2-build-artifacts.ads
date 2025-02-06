--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.View;

private with Ada.Tags;

package GPR2.Build.Artifacts is

   Invalid_Checksum : constant String := "";

   type Object is interface;
   --  Artifacts are the nodes of the Tree DB graph, and represent the
   --  various ingredients used during a build process: sources, object files,
   --  dependency files, libraries, environment variables and so on.
   --
   --  Artifacts have identified classes that allow sorting the artifacts
   --  by category and generate related actions to generate the next stage
   --  artifacts (so for example source artifacts generate object file
   --  artifacts).

   function View (Self : Object) return GPR2.Project.View.Object is abstract;
   --  The view responsible for this artifact

   function Is_Defined (Self : Object) return Boolean is abstract;

   function Protocol (Self : Object) return String is abstract;
   --  Must be constant for a class of artifacts. Is used to
   --  serialize / unserialize artifacts.

   function Serialize (Self : Object) return String is abstract;
   --  JSON representaiton of Self

   procedure Unserialize
     (Val  : out Object;
      Repr : String;
      Chk  : String;
      Ctxt : GPR2.Project.View.Object) is abstract;
   --  Translates the JSON representation to an actual instance

   function Checksum (Self : Object) return String is abstract;
   --  The current checksum of the resource

   function "<" (L, R : Object) return Boolean is abstract;

   function Hash (Self : Object) return Ada.Containers.Hash_Type is abstract;

   function Less (L, R : Object'Class) return Boolean;
   --  Class wide comparison, compares object type's external tags if L and R
   --  are not of the same type.

   procedure Register_Artifact_Class
     (Artifact : Object'Class);
   --  Used to register a new artifact class. The artifacts handled by
   --  libgpr2 are all registered at elaboration time in the body of this
   --  package.

   function New_Instance (Protocol : String) return Object'Class;
   --  Used to get a new empty artifact object from a Protocol id

private

   use type Ada.Tags.Tag;

   function Less (L, R : Object'Class) return Boolean is
     (if L'Tag = R'Tag then L < R
      else Ada.Tags.External_Tag (L'Tag) < Ada.Tags.External_Tag (R'Tag));

end GPR2.Build.Artifacts;
