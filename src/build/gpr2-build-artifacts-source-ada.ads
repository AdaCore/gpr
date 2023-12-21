--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.View;

package GPR2.Build.Artifacts.Source.Ada is

   type Object (<>) is new Artifacts.Source.Object with private;
   --  Represents an Ada source unit in the Build DAG.
   type Object_Access is access all Object;

   A_Class     : constant Artifact_Class := +"Source.Ada";

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Create (View     : GPR2.Project.View.Object;
                    Basename : Simple_Name;
                    Index    : Unit_Index) return Object;

   overriding function Id
     (Self : Object) return Artifact_Ids.Artifact_Id;

   function Id
     (View     : GPR2.Project.View.Object;
      Basename : Simple_Name;
      Index    : Unit_Index) return Artifact_Ids.Artifact_Id;

   overriding function Class
     (Self : Object) return Artifact_Class;

   function Index (Self : Object) return Unit_Index
     with Pre => Self.Is_Defined;

   procedure Set_Is_Main
     (Self  : in out Object;
      Value : Boolean)
     with Post => Self.Is_Main = Value;

   function Is_Main (Self : Object) return Boolean;

private

   type Object is new Artifacts.Source.Object
   with record
      Src_Index : Unit_Index;
      Is_Main   : Boolean := False;
   end record;

   overriding function Create (View     : GPR2.Project.View.Object;
                               Basename : Simple_Name) return Object
   is (Object'(Artifacts.Source.Create (View, Basename) with
       Src_Index => No_Index,
       Is_Main   => False));

   Undefined : constant Object :=
                 (Object'(Artifacts.Source.Undefined with
                          others           => <>));

   function Create (View     : GPR2.Project.View.Object;
                    Basename : Simple_Name;
                    Index    : Unit_Index) return Object
   is (Object'(Artifacts.Source.Create (View, Basename) with
       Src_Index => Index,
       Is_Main   => False));

   overriding function Is_Defined (Self : Object) return Boolean
   is (Self /= Undefined);

   overriding function Class
     (Self : Object) return Artifact_Class is (A_Class);

   function Index (Self : Object) return Unit_Index
   is (Self.Src_Index);

   function Is_Main (Self : Object) return Boolean
   is (Self.Is_Main);

end GPR2.Build.Artifacts.Source.Ada;
