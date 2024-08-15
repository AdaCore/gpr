--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Source_Reference;
with GPR2.Utils.Hash;

private with Ada.Tags;

package GPR2.Build.Artifacts is
   use GPR2.Utils.Hash;

   type Object is interface;
   --  Artifacts are the nodes of the Tree DB graph, and represent the
   --  various ingredients used during a build process: sources, object files,
   --  dependency files, libraries, environment variables and so on.
   --
   --  Artifacts have identified classes that allow sorting the artifacts
   --  by category and generate related actions to generate the next stage
   --  artifacts (so for example source artifacts generate object file
   --  artifacts).

   function Create (Uri : String) return Object is abstract;
   --  Used to deserialize an artifact. The protocol part is removed from the
   --  uri before Create is called.

   function Image (Self : Object) return String is abstract;
   --  This value is used when unserializing the artifact (see Create above).
   --  It is also used to report artifact-related messages to the end user.

   function SLOC (Self : Object) return GPR2.Source_Reference.Object'Class
                  is abstract;
   --  A source reference object, to be used in error reporting to locate the
   --  artifact.

   function Checksum (Self : Object) return Hash_Digest is abstract;
   --  The current checksum of the resource

   function "<" (L, R : Object) return Boolean is abstract;

   function Hash (Self : Object) return Ada.Containers.Hash_Type is abstract;

   function Less (L, R : Object'Class) return Boolean;
   --  Class wide comparison, compares object type's external tags if L and R
   --  are not of the same type.

   function Protocol (Self : Object) return String is abstract;
   --  Must be constant for a class of artifacts. Is used to
   --  serialize / unserialize artifacts.

   procedure Register_Artifact_Class
     (Artifact : Object'Class);
   --  Used to register a new artifact class. The artifacts handled by
   --  libgpr2 are all registered at elaboration time in the body of this
   --  package.

   subtype Uri_Type is String;

   function Serialize (Artifact : Object'Class) return Uri_Type;

   function Unserialize (Uri : Uri_Type) return Object'Class;
   --  Translates an Uri into object. Raises Constraint_Error if the protocol
   --  was not registered before.


private

   use type Ada.Tags.Tag;

   function Less (L, R : Object'Class) return Boolean is
     (if L'Tag = R'Tag then L < R
      else Ada.Tags.External_Tag (L'Tag) < Ada.Tags.External_Tag (R'Tag));

end GPR2.Build.Artifacts;
