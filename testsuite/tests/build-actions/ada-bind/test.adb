with Ada.Directories;
with Ada.Containers;

with GNAT.OS_Lib;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;

with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with Test_Assert;         use Test_Assert;

use GPR2, GPR2.Build;

function Test return Integer is
   use type GPR2.Language_Id;
   use type Ada.Containers.Count_Type;
   package GBA renames GPR2.Build.Actions;

   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Project     : constant String := "tree/main.gpr";


   Action : GBA.Ada_Bind.Object := GBA.Ada_Bind.Undefined;

   procedure Create_Ali_File (Base_Name : String) is
      Args    : Argument_List;
      P_Wo    : FS.File_Descriptor;
      P_Ro    : FS.File_Descriptor;
      Ret     : Integer;
      Process : Process_Handle;

      DS : constant Character := GNAT.OS_Lib.Directory_Separator;
   begin
      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append ("tree" & DS & "src" & DS & Base_Name & ".adb");
      Args.Append ("-o");
      Args.Append ("tree" & DS & "obj" & DS & Base_Name & ".o");

      FS.Open_Pipe (P_Ro, P_Wo);
      Process := Start (Args => Args, Stdout => P_Wo, Stderr => FS.Standerr);
      FS.Close (P_Wo);
      Ret := Wait (Process);
      Assert (Ret = 0, "Check ali file's creation process return code");
   end Create_Ali_File;

   procedure Init_Action
   is
      Source : GPR2.Build.Source.Object;
   begin

      for Root of Tree.Namespace_Root_Projects loop
         for Main of Root.Mains loop
            Action.Initialize
              (Main_Ali => Artifacts.Files.Create
                 (Root.Object_Directory.Compose
                      (Filename_Type
                           (String (Main.Source.Base_Name) & ".ali"))),
               Context   => Root);
            Assert
              (not Tree.Artifacts_Database.Has_Action (Action.UID),
               "Check that action is not already in the Tree DB");
            Assert
              (Tree.Artifacts_Database.Add_Action (Action),
               "Insert action to the tree database");
         end loop;
      end loop;
   end Init_Action;

   Obj_Dir      : Virtual_File;
   Count        : Natural;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   Assert
     (Tree.Load (Opts, With_Runtime => True), "Load the tree");

   Assert (Tree.Update_Sources
     (Option   => GPR2.Sources_Units_Artifacts), "Update sources");

   Obj_Dir := GNATCOLL.VFS.Create
                (Filesystem_String
                   (Tree.Root_Project.Object_Directory.Value));
   Make_Dir (Obj_Dir);
   Assert (Is_Directory (Obj_Dir));

   --  We need to create the ALI files dynamically because the
   --  compiler date used to create the ALI is stored at its first line.
   --  If the runtime ALIs date differs, then an error is raised.

   Create_Ali_File ("main");
   Create_Ali_File ("pkg");
   Create_Ali_File ("dep_two");

   Init_Action;

   declare
      Args    : Argument_List;
      Env     : Environment_Dict;
      P_Wo    : FS.File_Descriptor;
      P_Ro    : FS.File_Descriptor;
      Ret     : Integer;
      Process : Process_Handle;
   begin
      Action.Compute_Command (Args, Env);
      FS.Open_Pipe (P_Ro, P_Wo);
      Process := Start
        (Args        => Args,
         Env         => Env,
         Cwd         => Action.Working_Directory.String_Value,
         Stdin       => P_Wo,
         Stderr      => FS.Standerr,
         Inherit_Env => True);
      FS.Close (P_Wo);

      Ret := Wait (Process);
      Assert (Ret = 0, "Check action return code");
   end;

   Assert (Action.Generated_Spec.Path.Extension = ".ads",
           "Check output spec has .ads extension");
   Assert (Action.Generated_Body.Path.Extension = ".adb",
           "Check output body has .adb extension");
   Assert (Action.Generated_Body.Path.Base_Name = Action.Generated_Spec.Path.Base_Name,
           "Check output base name coherence");
   Assert (Action.Generated_Body.Path.Base_Name = "b__main",
           "Check output base name");

   Count := 0;
   for Input of Tree.Artifacts_Database.Inputs (Action.UID) loop
      Count := Count + 1;
      Assert (Artifacts.Files.Object (Input).Path.Simple_Name = "main.ali",
              "Check that the initial ALI file is main.ali");
   end loop;

   Assert (Count = 1, "Check length of input ALIs");

   declare
      PB  : GPR2.Build.Actions.Post_Bind.Object := Action.Post_Bind;
   begin
      Assert (PB.Is_Defined,
              "Check that post-binding action is defined");
   end;

   return Report;
end Test;
