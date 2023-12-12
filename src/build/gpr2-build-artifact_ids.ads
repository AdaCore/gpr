--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.View_Ids;
private with Ada.Strings.Hash;

package GPR2.Build.Artifact_Ids is

   type Artifact_Id (<>) is private;

   Undefined : constant Artifact_Id;

   function Create
     (Class : Artifact_Class;
      View  : GPR2.View_Ids.View_Id;
      Path  : Value_Type) return Artifact_Id;

   function Hash (Id : Artifact_Id) return Ada.Containers.Hash_Type
     with Pre => Is_Defined (Id);

   function Image (Id : Artifact_Id) return Value_Type
     with Pre => Is_Defined (Id);

   function Import (Image : Value_Type) return Artifact_Id
     with Pre => Is_Valid_Image (Image);

   function Is_Defined (Id : Artifact_Id) return Boolean;

   function Is_Valid_Image (Image : Value_Type) return Boolean;

   function "<" (Id, Other : Artifact_Id) return Boolean;

private

   type Artifact_Id (Path_Len : Natural) is record
      View_Id : GPR2.View_Ids.View_Id;
      Class   : Artifact_Class;
      Path    : Value_Type (1 .. Path_Len);
   end record;

   Undefined : constant Artifact_Id :=
                 (Path_Len => 0, others => <>);

   function Hash (Id : Artifact_Id) return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash (Image (Id)));

   function Is_Defined (Id : Artifact_Id) return Boolean is
      (Id /= Undefined);

end GPR2.Build.Artifact_Ids;
