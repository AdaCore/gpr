--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;

with GNATCOLL;
with GNATCOLL.JSON;

with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Utils.Hash;

package GPR2.Build.Signature is
   use GNATCOLL.JSON;
   use Ada.Containers;
   use Utils.Hash;

   package UB renames Ada.Strings.Unbounded;

   type Object is tagged private;

   type Artifact_Signature is record
      Path        : UB.Unbounded_String;
      Checksum    : Hash_Digest := (others => 'X');
   end record;

   package Artifact_Maps is new Ada.Containers.Ordered_Maps
     (B3_Hash_Digest, Artifact_Signature);

   function Artifacts_Signatures (Self : Object) return Artifact_Maps.Map;
   --  Return the artifacts signatures stored in the previously loaded
   --  build DB.

   function Artifact_Checksum
     (Self : Object; Id : B3_Hash_Digest) return Hash_Digest;
   --  Returns the artifact checksum stored in the build DB

   function Coherent (Self : Object) return Boolean;
   --  Reflects the coherent state of the signature after loading it.
   --  True  : Parsing the build DB file went OK.
   --  False : Parsing the build DB file went wrong.

   function Valid (Self : Object) return Boolean;
   --  Returns whether or not the signature is valid.
   --  This value is set by the Set_Valid_State which is representative of how
   --  each owner of a signature considers what a valid signature is.

   procedure Set_Valid_State (Self : in out Object; Valid : Boolean);
   --  Sets the validity of the signature to Valid.
   --  This value is accessible with the Valid function which is representative
   --  of how each owner of a signature considers what a valid signature is.

   procedure Update_Artifact
     (Self         : in out Object;
      Id           : B3_Hash_Digest;
      Plain_Id     : UTF8_String;
      Checksum     : Hash_Digest);
   --  Add or update an artifact in the signature

   function Load
     (Db_File  : Path_Name.Object;
      Messages : in out GPR2.Log.Object) return Object;
   --  Loads the build DB file Db_File

   procedure Store (Self : Object; Db_File : Path_Name.Object);
   --  Store the signature into the build DB file Db_File

private

   TEXT_ARTIFACTS : constant UTF8_String := "artifacts";
   TEXT_ID        : constant UTF8_String := "id";
   TEXT_PLAIN_ID  : constant UTF8_String := "plain_id";
   TEXT_CHECKSUM  : constant UTF8_String := "checksum";

   type Object is tagged record
      Artifacts : Artifact_Maps.Map := Artifact_Maps.Empty_Map;
      Coherent  : Boolean := True;
      Valid     : Boolean := False;
   end record;

   function Artifacts_Signatures (Self : Object) return Artifact_Maps.Map is
     (Self.Artifacts);

   function Coherent (Self : Object) return Boolean is (Self.Coherent);

   function Valid (Self : Object) return Boolean is (Self.Valid);

end GPR2.Build.Signature;
