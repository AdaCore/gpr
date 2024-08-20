--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers;
with Ada.Strings.Unbounded;

with GNATCOLL;
with GNATCOLL.JSON;

with GPR2.Build.Artifacts;
with GPR2.Path_Name;
with GPR2.Utils.Hash;

private with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Build.Signature is
   use GNATCOLL.JSON;
   use Ada.Containers;
   use Utils.Hash;

   package UB renames Ada.Strings.Unbounded;

   type Object is tagged private;

   function Valid (Self : Object) return Boolean;
   --  Returns whether or not the signature is valid.
   --  This value is set by the Set_Valid_State which is representative of how
   --  each owner of a signature considers what a valid signature is.

   procedure Add_Artifact
     (Self : in out Object;
      Art  : Artifacts.Object'Class);
   --  Add or update an artifact in the signature

   procedure Clear (Self : in out Object);
   --  Clear all the signature artifacts and invalidate it

   function Load (Db_File  : Path_Name.Object) return Object;
   --  Loads the build DB file Db_File

   procedure Store (Self : in out Object; Db_File : Path_Name.Object);
   --  Store the signature into the build DB file Db_File

private

   package Artifact_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Artifacts.Object'Class, Hash_Digest, Artifacts."<");

   TEXT_SIGNATURE : constant UTF8_String := "signature";

   type Object is tagged record
      Artifacts : Artifact_Maps.Map := Artifact_Maps.Empty_Map;
   end record;

   function Artifacts_Signatures (Self : Object) return Artifact_Maps.Map is
     (Self.Artifacts);

end GPR2.Build.Signature;
