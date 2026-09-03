--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Process.Cargo_Build;
with GPR2.Build.Tree_Db;
with GPR2.Path_Name;

--  Removes what Cargo built for one package, by running `cargo clean -p`.

package GPR2.Build.Actions.Process.Cargo_Clean is

   type Object is new Actions.Process.Object with private;
   --  A `cargo clean` invocation for one Cargo package

   procedure Initialize
     (Self         : in out Object;
      View         : GPR2.Project.View.Object;
      Package_Name : Filename_Type;
      Mode         : Cargo_Build.Cargo_Mode := Cargo_Build.Release);
   --  Initialize the action.
   --  @param Self The action to initialize
   --  @param View The Rust view whose package is to be cleaned
   --  @param Package_Name The Cargo package to clean, as reported by
   --     `cargo metadata`
   --  @param Mode The Cargo profile to clean, which has to be the one the
   --     build uses

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class;
   --  Identify this action by the view it cleans for.
   --  @param Self The action
   --  @return The action identifier

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean;
   --  Nothing to declare: the libraries that make this action necessary are
   --  wired to it by whoever creates it, and it produces no artifact.
   --  @param Self The action
   --  @param Db The action graph
   --  @return Always True

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean);
   --  Build the `cargo clean` command line.
   --  @param Self The action
   --  @param Slot The scheduler slot this will run in
   --  @param Cmd_Line The command line to fill in
   --  @param Signature_Only Whether the command is only needed to compute a
   --     signature

private

   type Cargo_Clean_Id is new Actions.Action_Id with record
      View : GPR2.Project.View.Object;
   end record;

   overriding
   function View (Self : Cargo_Clean_Id) return Project.View.Object
   is (Self.View);

   overriding
   function Action_Class (Self : Cargo_Clean_Id) return Value_Type
   is ("Cargo-Clean");

   overriding
   function Language (Self : Cargo_Clean_Id) return Language_Id
   is (No_Language);

   overriding
   function Action_Parameter (Self : Cargo_Clean_Id) return Value_Type
   is (Value_Type (Self.View.Name));

   type Object is new Actions.Process.Object with record
      Mode : Cargo_Build.Cargo_Mode := Cargo_Build.Release;
      --  The profile to clean

      Package_Name : Unbounded_String;
      --  The Cargo package to clean

      Rust_Triple : Unbounded_String;
      --  Rust target triple the artifacts to remove were built for

      Cargo_Toml : GPR2.Path_Name.Object;
      --  Path to the Cargo.toml manifest
   end record;

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean);

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object;

   overriding
   function Extended (Self : Object) return Object
   is (raise Internal_Error with "This action is not extending");

   Undefined : constant Object := (others => <>);

end GPR2.Build.Actions.Process.Cargo_Clean;
