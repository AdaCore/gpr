with Ada.Directories;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;

with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Set;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with Test_Assert;         use Test_Assert;

use GPR2;

function Test return Integer is
   use type GPR2.Language_Id;
   package GBA renames GPR2.Build.Actions;
   package A renames Test_Assert;

   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Messages    : GPR2.Log.Object;
   Project     : constant String := "tree/main.gpr";
   Ali_Path    : GPR2.Path_Name.Object;

   Bind_Action : GBA.Ada_Bind.Object;
   Action      : GBA.Post_Bind.Object;

   function Init_Action return Boolean
   is
      Source  : GPR2.Build.Source.Object;
      Log     : GPR2.Log.Object;
      Context : GPR2.Project.View.Object :=
                  Tree.Namespace_Root_Projects.First_Element;
      Success : Boolean := False;

   begin
      Ali_Path := Context.Object_Directory.Compose ("main.ali");
      Bind_Action.Initialize (Build.Artifacts.Files.Create (Ali_Path),
                              Context);
      Tree.Artifacts_Database.Add_Action (Bind_Action, Log);
      Action := Bind_Action.Post_Bind;

      Assert
        (Tree.Artifacts_Database.Has_Action (Action.UID),
         "Check that action is added by the binder action in the Tree DB");

      if Log.Has_Error then
         Log.Output_Messages (Warning => False);
         Assert
            (False, "Failed to insert action to the tree database");

         return False;
      end if;

      return True;
   end Init_Action;

   Obj_Dir : Virtual_File;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   Assert
     (Tree.Load (Opts, With_Runtime => True), "Load the tree");

   Tree.Update_Sources
     (Option   => GPR2.Sources_Units_Artifacts,
      Messages => Messages);

   if Messages.Has_Error then
      Messages.Output_Messages;
      Assert (False, "Update sources");
   end if;

   Messages.Clear;

   Obj_Dir := GNATCOLL.VFS.Create
                (Filesystem_String
                   (Tree.Root_Project.Object_Directory.Value));
   Make_Dir (Obj_Dir);
   Assert (Is_Directory (Obj_Dir));

   Assert (Init_Action, "Initialize the Ada post-bind action");

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

   declare
      Expected_Obj : Filesystem_String := Obj_Dir.Join ("b__main.o").Full_Name;
   begin
      Assert
        (Action.Object_File.Path.String_Value = String (Expected_Obj),
         "Check object file path");
      Assert
        (Action.Object_File.Path.Exists,
         "Check that object file has been correctly created");
   end;

   return A.Report;
end Test;
