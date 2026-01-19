--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.Key_Value;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Artifacts.Object_File;
with GPR2.Build.Artifacts.Source_Files;

package body GPR2.Build.Artifacts is

   package Protocol_Artifacts_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Object'Class);

   Map : Protocol_Artifacts_Map.Map;

   ------------------
   -- New_Instance --
   ------------------

   function New_Instance (Protocol : String) return Object'Class
   is
   begin
      return Map (Protocol);
   end New_Instance;

   -----------------------------
   -- Register_Artifact_Class --
   -----------------------------

   procedure Register_Artifact_Class
     (Artifact : Object'Class) is
   begin
      Map.Insert (Artifact.Protocol, Artifact);
   end Register_Artifact_Class;

begin

   Register_Artifact_Class (Files.Undefined);
   Register_Artifact_Class (Key_Value.Undefined);
   Register_Artifact_Class (Library.Undefined);
   Register_Artifact_Class (Object_File.Undefined);
   Register_Artifact_Class (Source_Files.Undefined);
end GPR2.Build.Artifacts;
