--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with GPR2.Project.View;
with GPR2.Build.Source;

package GPR2.Build.Artifacts.Source is

   type Object (<>) is new Artifacts.Object with private;
   --  Represents an Ada source unit in the Build DAG.

   A_Class     : constant Artifact_Class := +"Source";

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Create (View     : GPR2.Project.View.Object;
                    Basename : Simple_Name) return Object;

   overriding function Id
     (Self : Object) return Artifact_Ids.Artifact_Id;

   overriding function Class
     (Self : Object) return Artifact_Class;

   function Owning_View (Self : Object) return GPR2.Project.View.Object
     with Pre => Self.Is_Defined;

   function Source_Simple_Name (Self : Object) return Simple_Name
     with Pre => Self.Is_Defined;

   function Source (Self : Object) return GPR2.Build.Source.Object
     with Pre => Self.Is_Defined;

private

   type Object (Src_Basename_Len : Natural) is new Artifacts.Object with record
      Src_Owner    : Project.View.Object;
      Src_Basename : String (1 .. Src_Basename_Len);
   end record;

   Undefined : constant Object :=
                 (Src_Basename_Len => 0,
                  Src_Basename     => "",
                  others           => <>);

   function Create (View     : GPR2.Project.View.Object;
                    Basename : Simple_Name) return Object is
     ((Src_Basename_Len => Basename'Length,
       Src_Owner        => View,
       Src_Basename     => (if File_Names_Case_Sensitive
                            then String (Basename)
                            else Ada.Characters.Handling.To_Lower
                                   (String (Basename)))));

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   overriding function Id (Self : Object) return Artifact_Ids.Artifact_Id is
     (Artifact_Ids.Create
        (Class => Self.Class,
         View  => Self.Src_Owner.Id,
         Path  => Self.Src_Basename));

   overriding function Class
     (Self : Object) return Artifact_Class is (A_Class);

   function Owning_View (Self : Object) return GPR2.Project.View.Object is
      (Self.Src_Owner);

   function Source_Simple_Name (Self : Object) return Simple_Name is
      (Simple_Name (Self.Src_Basename));

   function Source (Self : Object) return GPR2.Build.Source.Object is
     (Self.Src_Owner.Source (Self.Source_Simple_Name));

end GPR2.Build.Artifacts.Source;
