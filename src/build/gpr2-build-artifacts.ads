--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifact_Ids;

package GPR2.Build.Artifacts is

   type Object is interface;
   --  Artifacts are the nodes of the Tree DB graph, and represent the
   --  various ingredients used during a build process: sources, object files,
   --  dependency files, libraries, environment variables and so on.
   --
   --  Artifacts have identified classes that allow sorting the artifacts
   --  by category and generate related actions to generate the next stage
   --  artifacts (so for example source artifacts generate object file
   --  artifacts).

   function Id
     (Self : Object) return Artifact_Ids.Artifact_Id is abstract;

   function Class
     (Self : Object) return Artifact_Class is abstract;

end GPR2.Build.Artifacts;
