--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Library;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;

--  Builds a Cargo project by running `cargo build`.
--
--  This action is created dynamically by the Cargo_Metadata action once the
--  Cargo target directory is known, and never while the tree is populated:
--  what a Cargo package builds, and under what name, is only known once
--  `cargo metadata` has run.

package GPR2.Build.Actions.Process.Cargo_Build is

   type Cargo_Mode is (Debug, Release);
   --  The Cargo profile to build with.
   --  @enum Debug Cargo's `dev` profile, written under `target/debug`
   --  @enum Release Cargo's `release` profile, written under `target/release`

   type Object is new Actions.Process.Object with private;
   --  A `cargo build` invocation for one Rust view

   procedure Initialize_Standard
     (Self             : in out Object;
      View             : GPR2.Project.View.Object;
      Cargo_Target_Dir : GPR2.Path_Name.Object;
      Binaries         : GPR2.Containers.Filename_Set;
      Mode             : Cargo_Mode := Release)
     with Pre => View.Is_Defined and then not View.Is_Library;
   --  Initialize the action for a standard view.
   --
   --  @param Self The action to initialize
   --  @param View The Rust view being built
   --  @param Cargo_Target_Dir The directory Cargo writes into, as reported by
   --     `cargo metadata`
   --  @param Binaries The binary targets Cargo builds, as reported by `cargo
   --     metadata`. When View declares Mains, only the related binaries should
   --     be specified
   --  @param Mode The Cargo profile to build with. Used to compute the final
   --     target directory

   procedure Initialize_Library
     (Self             : in out Object;
      View             : GPR2.Project.View.Object;
      Cargo_Target_Dir : GPR2.Path_Name.Object;
      Cargo_Lib_Name   : Filename_Optional;
      Cargo_Lib_Types  : GPR2.Containers.Value_List;
      Mode             : Cargo_Mode := Release)
     with Pre => View.Is_Defined and then View.Is_Library;
   --  Initialize the action for a library view.
   --
   --  Both cargo and project file should contain the same library name.
   --  The library kind should contain either staticlib or cdylib, but not
   --  both. Other values are not taken into account.
   --
   --  When incoherences are detected between the cargo information and the
   --  GPR project ones, errors are stored and reported later by
   --  On_Tree_Insertion.
   --
   --  @param Self The action to initialize
   --  @param View The Rust view being built
   --  @param Cargo_Target_Dir The directory Cargo writes into, as reported by
   --     `cargo metadata`
   --  @param Cargo_Lib_Name Library name specified in the Cargo manifest
   --  @param Cargo_Lib_Types The library types declared in the manifest.
   --     They should contain either staticlib or cdylib, but not both. Other
   --     values are not taken into account.
   --  @param Mode The Cargo profile to build with. Used to compute the final
   --     target directory

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;
   --  Identify this action by the view it builds.
   --  @param Self The action
   --  @return The action identifier

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;
   --  Declare this action's inputs and outputs, and reject a configuration
   --  Cargo cannot honour.
   --
   --  This is where the disagreements Initialize_Library recorded are
   --  reported, the action having a reporter to report them to only once it
   --  reaches the tree.
   --
   --  @param Self The action
   --  @param Db The action graph to declare into
   --  @return False when the configuration was rejected

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);
   --  Build the `cargo build` command line.
   --
   --  @param Self The action
   --  @param Slot The scheduler slot this will run in
   --  @param Cmd_Line The command line to fill in
   --  @param Signature_Only Whether the command is only needed to compute a
   --     signature

   overriding
   function Force_Execution (Self : Object) return Boolean;
   --  Whether to run even when the signature is up to date.
   --  @param Self The action
   --  @return Always True: Cargo decides for itself what needs rebuilding

   function Library_Artifact
     (Self : Object) return GPR2.Build.Artifacts.Library.Object;
   --  The library this action produces.
   --  @param Self The action
   --  @return The library artifact, Undefined for a standard view and for a
   --     library view whose configuration was rejected

private

   type Cargo_Build_Id is new Actions.Action_Id with record
      View : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Cargo_Build_Id) return Project.View.Object
   is (Self.View);

   overriding
   function Action_Class (Self : Cargo_Build_Id) return Value_Type
   is ("Cargo-Build");

   overriding
   function Language (Self : Cargo_Build_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Cargo_Build_Id) return Value_Type
   is (Value_Type (Self.View.Name));

   type Object is new Actions.Process.Object with record
      Mode : Cargo_Mode := Release;
      --  Whether to build in release or debug mode

      Library : GPR2.Build.Artifacts.Library.Object :=
        GPR2.Build.Artifacts.Library.Undefined;
      --  The library artifact produced by this action. Undefined for standard
      --  views.

      Manifest_Error : Unbounded_String := Null_Unbounded_String;
      --  Incoherence errors between the manifest file and the project file

      Rust_Triple : Unbounded_String := Null_Unbounded_String;
      --  Rust target triple passed to "cargo build --target". Null when the
      --  GPR target has no known Rust mapping and the action is therefore
      --  invalid.

      Binaries : GPR2.Containers.Filename_Set;
      --  Main binaries to build, is specified. Empty for a library view

      Executables : GPR2.Path_Name.Set.Object;
      --  The executables Cargo writes for this view, one per entry in
      --  Binaries. Empty for a library view.

      Cargo_Toml : GPR2.Path_Name.Object;
      --  Path to the Cargo.toml manifest, derived from the Cargo.Root
      --  project attribute.
   end record;

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object;

   overriding
   function Extended (Self : Object) return Object
   is (raise Internal_Error with "This action is not extending");

   overriding
   function Force_Execution (Self : Object) return Boolean
   is (True);

   Undefined : constant Object := (others => <>);

   function Library_Artifact
     (Self : Object) return GPR2.Build.Artifacts.Library.Object
   is (Self.Library);

end GPR2.Build.Actions.Process.Cargo_Build;
