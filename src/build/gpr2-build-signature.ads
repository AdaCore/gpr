--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;

with GPR2.Build.Artifacts;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Command_Line;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Utils.Hash;

package GPR2.Build.Signature is
   use Ada.Containers;
   use Utils.Hash;

   package Artifact_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Artifacts.Object'Class, Hash_Digest, Artifacts.Less);

   package UB renames Ada.Strings.Unbounded;

   type Object is tagged private;

   function Valid (Self : Object) return Boolean;
   --  Returns whether or not the signature is valid.
   --  This value is set by the Set_Valid_State which is representative of how
   --  each owner of a signature considers what a valid signature is.

   function Has_Error (Self : Object) return Boolean;
   --  Whether one or several expected artifacts are missing

   function Missing_Artifact
     (Self : Object) return GPR2.Build.Artifacts.Files.Object'Class
     with Pre => Self.Has_Error;
   --  In case of erroneous artifacts, this returns the first missing artifact

   procedure Add_Artifact
     (Self : in out Object;
      Art  : Artifacts.Object'Class);
   --  Add or update an artifact in the signature

   function Has_Artifact
     (Self : in out Object;
      Art  : Artifacts.Object'Class) return Boolean;
   --  Check that the artifact is part of the closure of the signature

   procedure Add_Output
     (Self   : in out Object;
      Stdout : UB.Unbounded_String;
      Stderr : UB.Unbounded_String);

   procedure Update_Command_Line_Digest
     (Self : in out Object;
      Sig  : GPR2.Build.Command_Line.Object);

   procedure Clear (Self : in out Object);
   --  Clear all the signature artifacts and invalidate it

   procedure Invalidate (Self : in out Object);
   --  Removes the checcksum from Self, but keeps cmd line representation
   --  and standard output and error

   function Load (Db_File : Path_Name.Object;
                  Ctxt    : GPR2.Project.View.Object) return Object;
   --  Loads the build DB file Db_File

   procedure Store (Self : in out Object; Db_File : Path_Name.Object);
   --  Store the signature into the build DB file Db_File

   function Artifacts (Self : Object) return Artifact_Maps.Map with Inline;

   function Stdout (Self : Object) return UB.Unbounded_String;
   function Stderr (Self : Object) return UB.Unbounded_String;

private

   TEXT_SIGNATURE   : constant String := "signature";
   TEXT_URI         : constant String := "uri";
   TEXT_CHECKSUM    : constant String := "checksum";
   TEXT_STDOUT      : constant String := "stdout";
   TEXT_STDERR      : constant String := "stderr";
   TEXT_CMDLINE     : constant String := "cmdline";
   TEXT_CMDLINE_CHK : constant String := "cmdline_checksum";

   type Object is tagged record
      Artifacts         : Artifact_Maps.Map := Artifact_Maps.Empty_Map;
      Has_Error         : Boolean := False;
      Cmd_Line_Checksum : GPR2.Utils.Hash.Hash_Digest :=
                            GPR2.Utils.Hash.No_Digest;
      Cmd_Line_Repr     : Unbounded_String;
      Cmd_Line_Match    : Boolean := False;
      Stdout            : Unbounded_String;
      Stderr            : Unbounded_String;
   end record;

   function Has_Artifact
     (Self : in out Object;
      Art  : Build.Artifacts.Object'Class) return Boolean is
      (Self.Artifacts.Contains (Art));

   function Has_Error (Self : Object) return Boolean is
      (Self.Has_Error);

   function Artifacts (Self : Object) return Artifact_Maps.Map is
     (Self.Artifacts);

   function Stdout (Self : Object) return Unbounded_String is
     (Self.Stdout);

   function Stderr (Self : Object) return Unbounded_String is
     (Self.Stderr);

end GPR2.Build.Signature;
