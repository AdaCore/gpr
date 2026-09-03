--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Process.Cargo_Support;
with GPR2.Build.Artifacts.Library;
with GPR2.Message;
with GPR2.Source_Reference;

package body GPR2.Build.Actions.Process.Cargo_Clean is

   ---------------------
   -- Compute_Command --
   ---------------------

   overriding
   procedure Compute_Command
     (Self           : in out Object;
      Slot           : Positive;
      Cmd_Line       : in out GPR2.Build.Command_Line.Object;
      Signature_Only : Boolean)
   is
      pragma Unreferenced (Slot, Signature_Only);
      use type Cargo_Build.Cargo_Mode;

      Driver : constant String := Cargo_Support.Driver (Self.Ctxt);
   begin
      if Driver = "" then
         Self.Tree.Reporter.Report
           (GPR2.Message.Create
              (GPR2.Message.Error,
               "no compiler driver defined for language Rust, "
               & "set Compiler.Driver (""Rust"") to the path of cargo",
               GPR2.Source_Reference.Create
                 (Self.Ctxt.Path_Name.Value, 0, 0)));

         return;
      end if;

      Cmd_Line.Set_Driver (Driver);

      Cmd_Line.Add_Argument ("clean");

      --  Only this package: cleaning the whole target directory would take
      --  the other members of a workspace with it.

      Cmd_Line.Add_Argument ("-p");
      Cmd_Line.Add_Argument (To_String (Self.Package_Name));

      --  The profile and triple have to be the ones the build uses, or the
      --  artifacts left behind are the ones that needed removing.

      Cmd_Line.Add_Argument ("--target=" & To_String (Self.Rust_Triple));

      Cmd_Line.Add_Argument ("--profile");
      Cmd_Line.Add_Argument
        (if Self.Mode = Cargo_Build.Release then "release" else "dev");

      Cmd_Line.Add_Argument
        ("--manifest-path=" & Self.Cargo_Toml.String_Value);

      Cmd_Line.Add_Argument ("-q");
   end Compute_Command;

   -----------------------
   -- Compute_Signature --
   -----------------------

   overriding
   procedure Compute_Signature
     (Self : in out Object; Check_Checksums : Boolean) is
   begin
      --  The libraries the Cargo build links against, and nothing else.
      --  Because Cargo is not aware of these libs, we ensure that the project
      --  is built from scratch when one of them changes.

      for Input of Self.Tree.Inputs (Object'Class (Self).UID) loop
         if Input in Artifacts.Library.Object'Class
           and then not Self.Signature.Add_Input (Input, Check_Checksums)
         then
            return;
         end if;
      end loop;
   end Compute_Signature;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : in out Object;
      View         : GPR2.Project.View.Object;
      Package_Name : Filename_Type;
      Mode         : Cargo_Build.Cargo_Mode := Cargo_Build.Release)
   is
      Triple : constant String := Cargo_Support.Rust_Triple (View);
   begin
      Self := Undefined;
      Self.Ctxt := View;
      Self.Mode := Mode;
      Self.Package_Name := To_Unbounded_String (String (Package_Name));

      Self.Cargo_Toml := Cargo_Support.Manifest (View);

      if Triple /= "" then
         Self.Rust_Triple := To_Unbounded_String (Triple);
      end if;
   end Initialize;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding
   function On_Tree_Insertion
     (Self : Object; Db : in out GPR2.Build.Tree_Db.Object) return Boolean
   is
      pragma Unreferenced (Self, Db);
   begin
      return True;
   end On_Tree_Insertion;

   ---------
   -- UID --
   ---------

   overriding
   function UID (Self : Object) return Actions.Action_Id'Class is
      Result : constant Cargo_Clean_Id := (View => Self.Ctxt);
   begin
      return Result;
   end UID;

   -----------------------
   -- Working_Directory --
   -----------------------

   overriding
   function Working_Directory (Self : Object) return Path_Name.Object
   is (Cargo_Support.Root_Directory (Self.Ctxt));

end GPR2.Build.Actions.Process.Cargo_Clean;
