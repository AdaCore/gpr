--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2.Build.Actions.Process.Cargo_Clean;
with GPR2.Options;
with GPR2.Project.Tree;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;

use GPR2;

function Test return Integer is
   package Cargo_Clean renames GPR2.Build.Actions.Process.Cargo_Clean;
   package A renames Test_Assert;

   Tree : GPR2.Project.Tree.Object;
   Opts : GPR2.Options.Object;
   CC   : Cargo_Clean.Object;

begin
   Opts.Add_Switch (GPR2.Options.P, "tree/hello_from_rust.gpr");

   A.Assert (Tree.Load (Opts, With_Runtime => False), "Load the tree");
   A.Assert
     (Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts),
      "Update sources");

   --  Mode is left out: its default is the one a build uses

   CC.Initialize
     (View         => Tree.Root_Project,
      Package_Name => "hello_from_rust");

   A.Assert
     (Tree.Artifacts_Database.Add_Action (CC), "Insert Cargo_Clean action");

   CC.Update_Command_Line (1);

   declare
      use Ada.Strings.Fixed;

      Args : constant Argument_List := CC.Command_Line.Argument_List;

      Has_Clean    : Boolean := False;
      Has_Package  : Boolean := False;
      Named        : Boolean := False;
      Has_Target   : Boolean := False;
      Has_Profile  : Boolean := False;
      Released     : Boolean := False;
      Has_Manifest : Boolean := False;

   begin
      for Arg of Args loop
         if Arg = "clean" then
            Has_Clean := True;
         elsif Arg = "-p" then
            Has_Package := True;
         elsif Has_Package and then Arg = "hello_from_rust" then
            --  Naming the package is what keeps the other members of a
            --  workspace out of the clean

            Named := True;
         elsif Index (Arg, "--target=") = 1 then
            Has_Target := True;
         elsif Arg = "--profile" then
            Has_Profile := True;
         elsif Has_Profile and then Arg = "release" then
            --  The profile has to be the one the build used, and naming none
            --  would empty every profile the package has

            Released := True;
         elsif Index (Arg, "--manifest-path=") = 1 then
            Has_Manifest := True;
         end if;
      end loop;

      A.Assert (Has_Clean,    "command line has clean");
      A.Assert (Has_Package,  "command line has -p");
      A.Assert (Named,        "the package is named after -p");
      A.Assert (Has_Target,   "command line has --target=");
      A.Assert (Has_Profile,  "command line has --profile");
      A.Assert (Released,     "the release profile is named after --profile");
      A.Assert (Has_Manifest, "command line has --manifest-path=");
   end;

   --  Run it. Every Rust build that consumes a library gets this action put
   --  in front of the Cargo build, so a switch this Cargo will not take stops
   --  the build before it starts.

   declare
      Output : Unbounded_String;
      Status : Integer;
   begin
      Output := Run
        (Args   => CC.Command_Line.Argument_List,
         Stdin  => FS.Null_FD,
         Stderr => FS.To_Stdout,
         Status => Status);

      A.Assert
        (Status = 0,
         "cargo took the clean command line: " & To_String (Output));
   end;

   return A.Report;
end Test;
