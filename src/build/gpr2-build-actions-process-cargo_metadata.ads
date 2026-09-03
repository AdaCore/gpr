--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Process.Cargo_Build;
with GPR2.Build.Compilation_Unit;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

--  Retrieves Cargo project metadata by running `cargo metadata`.
--
--  Runs `cargo metadata --format-version 1 --no-deps` to obtain information
--  needed to create the Cargo_Build action.

package GPR2.Build.Actions.Process.Cargo_Metadata is

   type Object is new Actions.Process.Object with private;
   --  A `cargo metadata` invocation for one Rust view

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;
   --  Identify this action by the view it describes.
   --  @param Self The action
   --  @return The action identifier

   procedure Initialize
     (Self  : in out Object;
      View  : GPR2.Project.View.Object;
      Mode  : Cargo_Build.Cargo_Mode := Cargo_Build.Release;
      Mains : GPR2.Build.Compilation_Unit.Unit_Location_Vector :=
                GPR2.Build.Compilation_Unit.Empty_Vector);
   --  Initialize the action for a Rust view.
   --  @param Self The action to initialize
   --  @param View The Rust view whose manifest is to be read
   --  @param Mode The Cargo profile, carried through to the Cargo_Build
   --     action this one creates
   --  @param Mains The Rust mains to build. If empty, build all the mains.
   --     Is empty for library projects.

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;
   --  Declare the manifest as this action's input.
   --  @param Self The action
   --  @param Db The action graph to declare into
   --  @return False when the action cannot be inserted

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);
   --  Build the `cargo metadata` command line.
   --  @param Self The action
   --  @param Slot The scheduler slot this will run in
   --  @param Cmd_Line The command line to fill in
   --  @param Signature_Only Whether the command is only needed to compute a
   --     signature

   overriding
   function Post_Execution
     (Self   : in out Object;
      Status : Execution_Status;
      Stdout : Unbounded_String := Null_Unbounded_String;
      Stderr : Unbounded_String := Null_Unbounded_String) return Boolean;
   --  Create the Cargo_Build action from what Cargo reported.
   --
   --  @param Self The action
   --  @param Status The exit status of the `cargo metadata` run
   --  @param Stdout Its output, falling back on the saved output when empty
   --  @param Stderr Its error output, unused
   --  @return False when the metadata is missing, or describes incoherencies
   --     between the cargo manifest and the project file

   overriding
   function Force_Execution (Self : Object) return Boolean;
   --  Whether to run even when the signature is up to date.
   --  @param Self The action
   --  @return Always True: what the manifest describes has to be read back on
   --     every run, the Cargo_Build action being derived from it

private

   type Cargo_Metadata_Id is new Actions.Action_Id with record
      View : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Cargo_Metadata_Id) return Project.View.Object
   is (Self.View);

   overriding
   function Action_Class (Self : Cargo_Metadata_Id) return Value_Type
   is ("Cargo-Metadata");

   overriding
   function Language (Self : Cargo_Metadata_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Cargo_Metadata_Id) return Value_Type
   is (Value_Type (Self.View.Name));

   type Object is new Actions.Process.Object with record
      Mode : Cargo_Build.Cargo_Mode := Cargo_Build.Release;
      --  Whether to build in release or debug mode

      Cargo_Toml : GPR2.Path_Name.Object;
      --  Path to the Cargo.toml manifest

      Mains : GPR2.Build.Compilation_Unit.Unit_Location_Vector;
      --  Mains binaries to build explicitly, if specified
   end record;

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object;
   --  @param Self The action
   --  @return The Cargo root directory

   overriding
   function Extended (Self : Object) return Object
   is (raise Internal_Error with "This action is not extending");

   overriding
   function Display_Output (Action : Object) return Boolean
   is (False);

   overriding
   function Force_Execution (Self : Object) return Boolean
   is (True);

   Undefined : constant Object := (others => <>);

end GPR2.Build.Actions.Process.Cargo_Metadata;
