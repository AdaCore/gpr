with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2.Build.Actions.Process.Cargo_Build;
with GPR2.Build.Actions.Process.Cargo_Metadata;
with GPR2.Build.Actions;
with GPR2.Options;
with GPR2.Project.Tree;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with Test_Assert;

use GPR2;

function Test return Integer is
   package Cargo_Metadata renames GPR2.Build.Actions.Process.Cargo_Metadata;
   package A renames Test_Assert;

   Tree : GPR2.Project.Tree.Object;
   Opts : GPR2.Options.Object;
   CM   : Cargo_Metadata.Object;

   function Has_Action_Class (Class : String) return Boolean is
   begin
      for Act of Tree.Artifacts_Database.All_Actions loop
         if String (Act.UID.Action_Class) = Class then
            return True;
         end if;
      end loop;
      return False;
   end Has_Action_Class;

begin
   Opts.Add_Switch (GPR2.Options.P,      "tree/hello_from_ada.gpr");

   A.Assert (Tree.Load (Opts, With_Runtime => False), "Load the tree");
   A.Assert
     (Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts),
      "Update sources");

   for V of Tree loop
      if V.Is_Library and then V.Language_Ids.Contains (Rust_Language) then
         CM.Initialize
           (View => V,
            Mode => GPR2.Build.Actions.Process.Cargo_Build.Release);
         exit;
      end if;
   end loop;

   A.Assert
     (Tree.Artifacts_Database.Add_Action (CM),
      "Insert Cargo_Metadata action");

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
     (Has_Action_Class ("Cargo-Build"),
      "Post_Execution created a Cargo-Build action");

   return A.Report;
end Test;
