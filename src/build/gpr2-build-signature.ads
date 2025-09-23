--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GPR2.Build.Artifacts;
with GPR2.Path_Name;
with GPR2.Project.View;

--  The signature mechanism gather a list of input and output artifacts
--  and checks them against their expected value.
--
--  The format for saving an artifact is:
--
--  {"class": "",
--   "key":   "",
--   "value": ""}
--
--  The class allowing to find a proper instance, then the instance
--  unserializes itself using key and value.
--
--  The value is generic: for a file it will be a checksum but for
--  key-value it will be the actual value, where all is needed is
--  a simple string comparison.
--
--  When loading the signature, the actions will populate the
--  expected artifacts manually before run.
--  Each artifact is checked against the signature, and this process stops as
--  soon as there is a mismatch. The signature is automatically invalidated
--  in this case.
--
--  On the other hand, when saving the signature, the final list of
--  artifacts is fully populated and their checksums/values dumped.

package GPR2.Build.Signature is

   package UB renames Ada.Strings.Unbounded;

   type Object is tagged private;

   Undefined : constant Object;

   function Was_Saved (Self : Object) return Boolean;

   function Add_Output
     (Self           : in out Object;
      Art            : Artifacts.Object'Class;
      Checksum_Check : Boolean := True) return Boolean;
   --  Add a new output artifact to the signature. Returns the current
   --  valid status of the signature after addition of the artifact.
   --  If Checksum_Check is not set, then the checksum of the artifact is
   --  not saved or verified.

   function Add_Input
     (Self           : in out Object;
      Art            : Artifacts.Object'Class;
      Checksum_Check : Boolean := True) return Boolean;
   --  Add a new input artifact to the signature. Returns the current
   --  valid status of the signature after addition of the artifact.
   --  If Checksum_Check is not set, then the checksum of the artifact is
   --  not saved or verified

   procedure Add_Console_Output
     (Self   : in out Object;
      Stdout : UB.Unbounded_String;
      Stderr : UB.Unbounded_String);

   function Stdout (Self : Object) return UB.Unbounded_String;
   function Stderr (Self : Object) return UB.Unbounded_String;

   procedure Invalidate (Self : in out Object);
   --  Clear the checksums in Self.

   procedure Clear (Self : in out Object);
   --  Clear all the signature artifacts and invalidate it

   function Load (Db_File : Path_Name.Object;
                  Ctxt    : GPR2.Project.View.Object) return Object;
   --  Loads the build DB file Db_File

   procedure Store (Self : in out Object; Db_File : Path_Name.Object);
   --  Store the signature into the build DB file Db_File

   function Valid (Self : Object) return Boolean;

   function Valid (Self : Object; Art : Artifacts.Object'Class) return Boolean;
   --  Check if the artifact checksum is the same as the one stored in the
   --  signature.

   function Is_Empty (Self : Object) return Boolean;
   --  True if no input or output is in the signature

private

   TEXT_INPUTS    : constant String := "inputs";
   TEXT_OUTPUTS   : constant String := "outputs";
   TEXT_CLASS     : constant String := "class";
   TEXT_KEY       : constant String := "key";
   TEXT_VALUE     : constant String := "value";
   TEXT_STDOUT    : constant String := "stdout";
   TEXT_STDERR    : constant String := "stderr";

   package Checksum_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Artifacts.Object'Class, String, Artifacts.Less);

   function Hash
     (Item : Artifacts.Object'Class) return Ada.Containers.Hash_Type
   is (Item.Hash);

   package Artifact_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Artifacts.Object'Class, Hash, Artifacts."=", Artifacts."=");

   type IO_Type is (Input, Output);

   type Checksums_Type is array (IO_Type) of Checksum_Maps.Map;
   type Artifacts_Type is array (IO_Type) of Artifact_Sets.Set;

   type Object is tagged record
      Checksums : Checksums_Type;
      Artifacts : Artifacts_Type;
      Stdout    : Unbounded_String;
      Stderr    : Unbounded_String;
   end record;

   Undefined : constant Object := (others => <>);

   function Was_Saved (Self : Object) return Boolean is
     (not Self.Checksums (Input).Is_Empty
      or else not Self.Checksums (Output).Is_Empty);

   function Stdout (Self : Object) return Unbounded_String is
     (Self.Stdout);

   function Stderr (Self : Object) return Unbounded_String is
     (Self.Stderr);

   function Is_Empty (Self : Object) return Boolean is
     (Self.Artifacts (Input).Is_Empty
      and then Self.Artifacts (Output).Is_Empty);
end GPR2.Build.Signature;
