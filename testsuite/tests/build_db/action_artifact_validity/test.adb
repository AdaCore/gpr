--  Copyright (C) 2026, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception

with Ada.Containers;
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with File_Action;
with GPR2.Build.Signature;
with GPR2.Options;
with GPR2.Project.Tree;
with Test_Assert;

function Test return Integer is
   use Ada.Strings.Unbounded;
   use Test_Assert;
   use type Ada.Containers.Count_Type;

   Tree   : GPR2.Project.Tree.Object;
   Opts   : GPR2.Options.Object;
   Action : File_Action.Object;
   Saved  : GPR2.Build.Signature.Object;
   Stdout : Unbounded_String;
   Stderr : Unbounded_String;

   procedure Write_File (Name, Content : String) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Name);
      Ada.Text_IO.Put_Line (File, Content);
      Ada.Text_IO.Close (File);
   end Write_File;

begin
   Opts.Add_Switch (GPR2.Options.P, "tree/prj.gpr");
   Assert (Tree.Load (Opts, With_Runtime => True), "load project");
   Ada.Directories.Create_Path
     (Tree.Root_Project.Object_Directory.String_Value);

   Write_File (File_Action.Input.Path.String_Value, "original input");
   Action.Initialize (Tree.Root_Project);
   Assert (Tree.Artifacts_Database.Add_Action (Action), "add action");
   Assert (Action.Execute (Stdout, Stderr) = 0, "execute action");
   declare
      Copy_File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open
        (Copy_File, Ada.Text_IO.In_File, File_Action.Copy.Path.String_Value);
      Assert (Ada.Text_IO.Get_Line (Copy_File) = "original input",
              "action did not copy its input");
      Ada.Text_IO.Close (Copy_File);
   end;
   Assert (Action.Write_Signature (Stdout, Stderr), "save signature");

   Action.Load_Signature (Check_Checksums => False);
   Saved := Action.Signature;
   Assert (Saved.Was_Saved, "signature was not loaded");
   declare
      Artifacts : constant GPR2.Build.Signature.Artifact_Sets.Set :=
        Saved.Get_Artifacts;
   begin
      Assert (Artifacts.Length = 3, "wrong artifact count");
      Assert (Artifacts.Contains (File_Action.Input), "input missing");
      Assert (Artifacts.Contains (File_Action.Copy), "copy missing");
      Assert (Artifacts.Contains (File_Action.Marker), "marker missing");
   end;
   Assert (Saved.Valid (File_Action.Input), "input initially invalid");
   Assert (Saved.Valid (File_Action.Copy), "copy initially invalid");
   Assert (Saved.Valid (File_Action.Marker), "marker initially invalid");

   Write_File (File_Action.Input.Path.String_Value, "modified input data");
   Tree.Artifacts_Database.File_Indexer.Start_Generation;
   Assert (not Saved.Valid (File_Action.Input), "changed input is valid");
   Assert (Saved.Valid (File_Action.Copy), "copy changed with input");
   Assert (Saved.Valid (File_Action.Marker), "marker changed with input");

   Write_File (File_Action.Copy.Path.String_Value, "modified copy data");
   Tree.Artifacts_Database.File_Indexer.Start_Generation;
   Assert (not Saved.Valid (File_Action.Input), "input became valid");
   Assert (not Saved.Valid (File_Action.Copy), "changed copy is valid");
   Assert (Saved.Valid (File_Action.Marker), "marker changed with copy");

   Write_File (File_Action.Marker.Path.String_Value, "modified marker data");
   Tree.Artifacts_Database.File_Indexer.Start_Generation;
   Assert (not Saved.Valid (File_Action.Input), "input became valid");
   Assert (not Saved.Valid (File_Action.Copy), "copy became valid");
   Assert (not Saved.Valid (File_Action.Marker), "changed marker is valid");
   Assert (Saved.Was_Saved, "checks cleared after artifact checks");

   return Report;
end Test;
