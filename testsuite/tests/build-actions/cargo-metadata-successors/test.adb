with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2.Build.Actions.Process.Cargo_Build;
with GPR2.Build.Actions.Process.Cargo_Metadata;
with GPR2.Build.Actions;
with GPR2.Build.Artifacts.Library;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;

use GPR2;

--  The library produced by cargo is only known once 'cargo metadata' has run,
--  so the actions depending on a Rust library can until then only reference
--  it through the Cargo_Metadata UID artifact, which carries no path.
--  Post_Execution is what hands them the library artifact itself, and it must
--  do so for every action depending on it: a Rust executable is built by a
--  Cargo_Build action, not by a GPR2 link action, and would otherwise be
--  built without the library it calls into.

function Test return Integer is
   package Cargo_Build renames GPR2.Build.Actions.Process.Cargo_Build;
   package Cargo_Metadata renames GPR2.Build.Actions.Process.Cargo_Metadata;
   package A renames Test_Assert;

   Tree     : GPR2.Project.Tree.Object;
   Opts     : GPR2.Options.Object;
   Lib_View : GPR2.Project.View.Object;
   Exe_View : GPR2.Project.View.Object;
   CM       : Cargo_Metadata.Object;
   CB       : Cargo_Build.Object;
   Binaries : GPR2.Containers.Filename_Set;
   --  The binary targets "cargo metadata" would have reported

   function Has_Library_Input return Boolean is
   begin
      for Input of Tree.Artifacts_Database.Inputs (CB.UID) loop
         if Input in GPR2.Build.Artifacts.Library.Object'Class then
            return True;
         end if;
      end loop;

      return False;
   end Has_Library_Input;

begin
   Binaries.Include ("main_from_rust");

   Opts.Add_Switch (GPR2.Options.P, "tree/main.gpr");

   A.Assert (Tree.Load (Opts, With_Runtime => False), "Load the tree");
   A.Assert
     (Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts),
      "Update sources");

   for V of Tree loop
      if V.Language_Ids.Contains (Rust_Language) then
         if V.Is_Library then
            Lib_View := V;
         else
            Exe_View := V;
         end if;
      end if;
   end loop;

   A.Assert (Lib_View.Is_Defined, "the Rust library view was found");
   A.Assert (Exe_View.Is_Defined, "the Rust executable view was found");

   --  Create the two actions the population would create for such a tree

   CM.Initialize (View => Lib_View, Mode => Cargo_Build.Release);

   A.Assert
     (Tree.Artifacts_Database.Add_Action (CM),
      "Insert Cargo_Metadata action");

   CB.Initialize_Standard
     (View             => Exe_View,
      Cargo_Target_Dir => GPR2.Path_Name.Create_Directory ("target"),
      Binaries         => Binaries,
      Mode             => Cargo_Build.Release);

   A.Assert
     (Tree.Artifacts_Database.Add_Action (CB),
      "Insert Cargo_Build action");

   --  The executable depends on the library: before 'cargo metadata' has run
   --  this can only be expressed through the UID artifact

   Tree.Artifacts_Database.Add_Input (CB.UID, CM.UID_Artifact);

   A.Assert
     (not Has_Library_Input,
      "no library is known before cargo metadata has run");

   declare
      Status : Integer;
      Stdout : Unbounded_String;
   begin
      CM.Update_Command_Line (1);
      Stdout := Run
        (Args        => CM.Command_Line.Argument_List,
         Env         => CM.Command_Line.Environment_Variables,
         Cwd         => CM.Working_Directory.String_Value,
         Inherit_Env => True,
         Status      => Status);
      A.Assert (Status = 0, "cargo metadata returns 0");

      A.Assert
        (CM.Post_Execution
           (Status => GPR2.Build.Actions.Success,
            Stdout => Stdout),
         "Post_Execution succeeds");
   end;

   A.Assert
     (Has_Library_Input,
      "the cargo build of the executable received the library artifact");

   return A.Report;
end Test;
