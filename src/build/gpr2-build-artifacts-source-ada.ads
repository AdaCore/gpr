--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.View;

package GPR2.Build.Artifacts.Source.Ada is

   type Object (<>) is new Artifacts.Source.Object with private;
   --  Represents an Ada source unit in the Build DAG.

   Undefined : constant Object;

   overriding function Is_Defined (Self : Object) return Boolean;

   function Create (View     : GPR2.Project.View.Object;
                    Basename : Simple_Name;
                    Index    : Unit_Index) return Object;

   overriding function Id
     (Self : Object) return Artifact_Ids.Artifact_Id;

   overriding function Class
     (Self : Object) return Artifact_Class;

   function Index (Self : Object) return Unit_Index
     with Pre => Self.Is_Defined;

private

   type Object is new Artifacts.Source.Object
   with record
      Src_Index    : Unit_Index;
   end record;

   overriding function Create (View     : GPR2.Project.View.Object;
                                Basename : Simple_Name) return Object is
     (Object'(Artifacts.Source.Create (View, Basename) with
              Src_Index => No_Index));

   Undefined : constant Object :=
                 (Object'(Artifacts.Source.Undefined with
                          others           => <>));

   function Create (View     : GPR2.Project.View.Object;
                    Basename : Simple_Name;
                    Index    : Unit_Index) return Object is
     (Object'(Artifacts.Source.Create (View, Basename) with
              Src_Index => Index));

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   overriding function Class
     (Self : Object) return Artifact_Class is (+"Ada Source");

   function Index (Self : Object) return Unit_Index is
      (Self.Src_Index);

end GPR2.Build.Artifacts.Source.Ada;
