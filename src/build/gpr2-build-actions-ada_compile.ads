--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.DAG;
private with GPR2.Build.Artifacts.Source.Ada;

package GPR2.Build.Actions.Ada_Compile is

   procedure Register (DAG : access Build.DAG.Object);

   type Object is new Actions.Object with private;

   overriding function Class (Self : Object) return Action_Class;

   overriding function Inputs (Self : Object) return Artifact_Class;

   overriding procedure Fill
     (Self  : Object;
      Graph : access DAG.Object;
      Input : Artifact_Ids.Artifact_Id);

private

   type Object is new Actions.Object with null record;

   overriding function Class (Self : Object) return Action_Class
   is ("Ada compile");

   overriding function Inputs (Self : Object) return Artifact_Class
   is (Artifacts.Source.Ada.A_Class);

end GPR2.Build.Actions.Ada_Compile;
