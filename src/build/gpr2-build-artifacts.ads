--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifact_Ids;

package GPR2.Build.Artifacts is

   type Object is interface;

   function Id
     (Self : Object) return Artifact_Ids.Artifact_Id is abstract;

   function Class
     (Self : Object) return Artifact_Class is abstract;

end GPR2.Build.Artifacts;
