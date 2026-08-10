with Ada.Strings.Fixed;

with GPR2.Build.Actions.Process.Cargo_Build;
with GPR2.Build.Artifacts.Library;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;

use GPR2;

function Test return Integer is
   package Cargo_Build renames GPR2.Build.Actions.Process.Cargo_Build;
   package A renames Test_Assert;

   Binaries : GPR2.Containers.Filename_Set;
   --  The binary targets "cargo metadata" would have reported
begin
   Binaries.Include ("hello_from_rust");

   --  Check command line construction: --target=, --release, --manifest-path=
   --  and RUSTFLAGS population when an Ada shared library is wired as input.
   declare
      Tree : GPR2.Project.Tree.Object;
      Opts : GPR2.Options.Object;
      CB   : Cargo_Build.Object;
   begin
      Opts.Add_Switch (GPR2.Options.P,      "tree/hello_from_rust.gpr");

      A.Assert (Tree.Load (Opts, With_Runtime => False), "Load the tree");
      A.Assert
        (Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts),
         "Update sources");

      CB.Initialize_Standard
        (View             => Tree.Root_Project,
         Cargo_Target_Dir => GPR2.Path_Name.Create_Directory ("target"),
         Binaries         => Binaries,
         Mode             => Cargo_Build.Release);

      A.Assert
        (Tree.Artifacts_Database.Add_Action (CB),
         "Insert Cargo_Build action");

      Tree.Artifacts_Database.Add_Input
        (CB.UID,
         GPR2.Build.Artifacts.Library.Create_Shared
           (GPR2.Path_Name.Create_File ("lib/libmathlib.so"),
            Link_Name => "mathlib"));

      --  A directory holding a space: the flags reach Cargo through
      --  CARGO_ENCODED_RUSTFLAGS precisely so that this stays one flag.

      Tree.Artifacts_Database.Add_Input
        (CB.UID,
         GPR2.Build.Artifacts.Library.Create_Shared
           (GPR2.Path_Name.Create_File ("lib dir/libspaced.so"),
            Link_Name => "spaced"));

      CB.Update_Command_Line (1);

      declare
         use Ada.Strings.Fixed;
         Args         : constant Argument_List :=
                          CB.Command_Line.Argument_List;
         Has_Target   : Boolean := False;
         Has_Release  : Boolean := False;
         Has_Manifest : Boolean := False;
         Has_Bin      : Boolean := False;
         Bin_Named    : Boolean := False;
      begin
         for Arg of Args loop
            if Index (Arg, "--target=") = 1 then
               Has_Target := True;
            elsif Arg = "--release" then
               Has_Release := True;
            elsif Index (Arg, "--manifest-path=") = 1 then
               Has_Manifest := True;
            elsif Arg = "--bin" then
               Has_Bin := True;
            elsif Has_Bin and then Arg = "hello_from_rust" then
               --  Cargo must be told which binaries to build, or it builds
               --  every one the package declares while GPR2 tracks only the
               --  ones asked for.

               Bin_Named := True;
            end if;
         end loop;
         A.Assert (Has_Target,   "command line has --target=");
         A.Assert (Has_Release,  "command line has --release");
         A.Assert (Has_Manifest, "command line has --manifest-path=");
         A.Assert (Has_Bin,      "command line has --bin");
         A.Assert (Bin_Named,    "the selected binary is named after --bin");
      end;

      declare
         use Ada.Strings.Fixed;

         Env : constant Environment_Dict :=
                 CB.Command_Line.Environment_Variables;
      begin
         A.Assert
           (Env.Contains ("CARGO_ENCODED_RUSTFLAGS"),
            "CARGO_ENCODED_RUSTFLAGS is set");
         A.Assert
           (not Env.Contains ("RUSTFLAGS"),
            "the whitespace-split RUSTFLAGS is not used");

         declare
            Flags : constant GPR2.Containers.Value_List :=
                      GPR2.Containers.Create
                        (Value_Type (Env.Element ("CARGO_ENCODED_RUSTFLAGS")),
                         (1 => ASCII.US));

            Run_Path : constant GPR2.Project.Attribute.Object :=
                         Tree.Root_Project.Attribute
                           (GPR2.Project.Registry.Attribute.Run_Path_Option);
            --  How this target spells a runtime search path. Undefined where
            --  there is none to spell, Windows being the case at hand.

            Rpath_Flag : constant String :=
                           (if Run_Path.Is_Defined
                              and then not Run_Path.Values.Is_Empty
                            then "link-arg="
                                 & Run_Path.Values.Last_Element.Text
                            else "");

            Has_L      : Boolean := False;
            Has_Rpath  : Boolean := False;
            Any_Rpath  : Boolean := False;
            Has_Spaced : Boolean := False;
            Split      : Boolean := False;

            function Shown return String;
            --  The encoded value with its separators made visible, so that a
            --  failure below says what was actually emitted

            -----------
            -- Shown --
            -----------

            function Shown return String is
               Value : String := Env.Element ("CARGO_ENCODED_RUSTFLAGS");
            begin
               for C of Value loop
                  if C = ASCII.US then
                     C := '|';
                  end if;
               end loop;

               return Value;
            end Shown;

         begin
            for Flag of Flags loop
               --  Compared against Flag'First, not against 1: Create builds
               --  each element from a slice of the value it was given, so
               --  the elements keep that value's indices rather than
               --  starting at one.

               if Flag = "-lmathlib" then
                  Has_L := True;

               elsif Rpath_Flag /= ""
                 and then Index (Flag, Rpath_Flag) = Flag'First
               then
                  Has_Rpath := True;
               end if;

               if Index (Flag, "rpath") > 0 then
                  Any_Rpath := True;
               end if;

               --  The directory holding a space must sit whole inside one
               --  flag, never as a flag that stops at the space and another
               --  that starts after it.

               if Index (Flag, "-L") = Flag'First
                 and then Index (Flag, "lib dir") > 0
               then
                  Has_Spaced := True;

               elsif Flag = "-L"
                 or else Index (Flag, "dir") = Flag'First
               then
                  Split := True;
               end if;
            end loop;

            A.Assert (Has_L, "encoded flags hold -lmathlib: " & Shown);
            if Rpath_Flag = "" then
               A.Assert
                 (not Any_Rpath,
                  "this target has no runtime search path, so no flag "
                  & "carries one: " & Shown);
            else
               A.Assert
                 (Has_Rpath,
                  "encoded flags hold the runtime search path: " & Shown);
            end if;
            A.Assert
              (Has_Spaced,
               "the directory with a space is one flag: " & Shown);
            A.Assert
              (not Split, "no flag was split on the space: " & Shown);
         end;
      end;
   end;

   --  Check that cargo build executes successfully
   declare
      Tree    : GPR2.Project.Tree.Object;
      Opts    : GPR2.Options.Object;
      CB      : Cargo_Build.Object;
      Process : Process_Handle;
      Ret     : Integer;
   begin
      Opts.Add_Switch (GPR2.Options.P,      "tree/hello_from_rust.gpr");

      A.Assert (Tree.Load (Opts, With_Runtime => False), "Load the tree");
      A.Assert
        (Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts),
         "Update sources");

      CB.Initialize_Standard
        (View             => Tree.Root_Project,
         Cargo_Target_Dir => GPR2.Path_Name.Create_Directory ("target"),
         Binaries         => Binaries,
         Mode             => Cargo_Build.Release);

      A.Assert
        (Tree.Artifacts_Database.Add_Action (CB),
         "Insert Cargo_Build action");

      CB.Update_Command_Line (1);
      Process := Start
        (Args        => CB.Command_Line.Argument_List,
         Env         => CB.Command_Line.Environment_Variables,
         Cwd         => CB.Working_Directory.String_Value,
         Stdout      => FS.Standout,
         Stderr      => FS.Standerr,
         Inherit_Env => True);
      Ret := Wait (Process);
      A.Assert (Ret = 0, "cargo build returns 0");
   end;

   return A.Report;
end Test;
