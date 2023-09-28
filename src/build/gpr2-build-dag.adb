--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

package body GPR2.Build.DAG is

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Self : access Object; Input : Artifact_Class; Value : Action'Class)
   is
   begin
      null;
   end Register_Action;

   -----------------------
   -- Register_Artifact --
   -----------------------

   function Register_Artifact
     (Self : access Object; Class : Artifact_Class; Value : Artifact'Class)
      return Artifact_Id
   is
   begin
      return No_Id;
   end Register_Artifact;

   -----------------------------
   -- Register_Artifact_Class --
   -----------------------------

   function Register_Artifact_Class
     (Self : access Object; Class_Name : String) return Artifact_Class
   is
   begin
      return No_Class;
   end Register_Artifact_Class;

end GPR2.Build.DAG;
