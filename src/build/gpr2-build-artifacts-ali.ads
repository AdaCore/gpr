--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Path_Name;
with GPR2.Project.View;

package GPR2.Build.Artifacts.ALI is

   type Object (<>) is new Artifacts.Object with private;
   --  Represents an Ada source unit in the Build DAG.

   A_Class   : constant Artifact_Class := +"ALI";

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

   function Simple_Name (Self : Object) return Simple_Name
     with Pre => Self.Is_Defined;

   function Path (Self : Object) return GPR2.Path_Name.Object
     with Pre => Self.Is_Defined;

private

   type Object (Basename_Len : Natural) is new Artifacts.Object with record
      Owner    : Project.View.Object;
      Basename : String (1 .. Basename_Len);
   end record;

   Undefined : constant Object :=
                 (Basename_Len => 0,
                  Basename     => "",
                  others       => <>);

   function Create (View     : GPR2.Project.View.Object;
                    Basename : GPR2.Simple_Name) return Object is
     ((Basename_Len => Basename'Length,
       Owner        => View,
       Basename     => (if File_Names_Case_Sensitive
                        then String (Basename)
                        else Ada.Characters.Handling.To_Lower
                               (String (Basename)))));

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   overriding function Id (Self : Object) return Artifact_Ids.Artifact_Id is
     (Artifact_Ids.Create
        (Class => Self.Class,
         View  => Self.Owner.Id,
         Path  => Value_Type (Self.Path.Relative_Path (Self.Owner.Dir_Name))));

   overriding function Class
     (Self : Object) return Artifact_Class is (A_Class);

   function Owning_View (Self : Object) return GPR2.Project.View.Object is
      (Self.Owner);

   function Simple_Name (Self : Object) return GPR2.Simple_Name is
      (GPR2.Simple_Name (Self.Basename));

   function Path (Self : Object) return GPR2.Path_Name.Object is
     ((if Self.Owner.Is_Library
       then Self.Owner.Library_Directory.Compose
              (Filename_Type (Self.Basename))
       else Self.Owner.Object_Directory.Compose
              (Filename_Type (Self.Basename))));

end GPR2.Build.Artifacts.ALI;
