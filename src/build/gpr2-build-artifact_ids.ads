--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.View_Ids;
private with Ada.Strings.Hash;

package GPR2.Build.Artifact_Ids is

   type Artifact_Id (<>) is private;
   --  Artifact Ids are used to identify build artifacts without having to
   --  manipulate the objects themselves. Such representation is faster to
   --  use when reasoning at a graph level, and is resilient to artifacts
   --  changes.
   --  An artifact is identified by the class of the artifact, the view it
   --  belongs to, and its "path" whose significance depends on the
   --  artifact class.

   Undefined : constant Artifact_Id;

   function Is_Defined (Id : Artifact_Id) return Boolean;

   function Create
     (Class : Artifact_Class;
      View  : GPR2.View_Ids.View_Id;
      Path  : Value_Type) return Artifact_Id;

   function Hash (Id : Artifact_Id) return Ada.Containers.Hash_Type
     with Pre => Is_Defined (Id);

   function Image (Id : Artifact_Id) return Value_Type
     with Pre => Is_Defined (Id);
   --  Serialisation of the Artifact_Id

   function Import (Image : Value_Type) return Artifact_Id
     with Pre => Is_Valid_Image (Image);
   --  Deserialization of the Artifact_Id

   function Is_Valid_Image (Image : Value_Type) return Boolean;
   --  To be used before deserializing to check if the image is
   --  correct and can be decoded.

   function "<" (Id, Other : Artifact_Id) return Boolean;

   function Class (Id : Artifact_Id) return Artifact_Class;
   function Path (Id : Artifact_Id) return Value_Type;
   function View (Id : Artifact_Id) return GPR2.View_Ids.View_Id;

private

   type Artifact_Id (Path_Len : Natural) is record
      View_Id : GPR2.View_Ids.View_Id;
      Class   : Artifact_Class;
      Path    : Value_Type (1 .. Path_Len);
   end record;

   Undefined : constant Artifact_Id :=
                 (Path_Len => 0, others => <>);

   function Hash (Id : Artifact_Id) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Image (Id)));

   function Is_Defined (Id : Artifact_Id) return Boolean
   is (Id /= Undefined);

   function Class (Id : Artifact_Id) return Artifact_Class
   is (Id.Class);
   function Path (Id : Artifact_Id) return Value_Type
   is (Id.Path);
   function View (Id : Artifact_Id) return GPR2.View_Ids.View_Id
   is (Id.View_Id);

end GPR2.Build.Artifact_Ids;
