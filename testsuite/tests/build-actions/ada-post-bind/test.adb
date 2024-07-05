with Ada.Directories;

with GPR2.Build.Actions.Ada_Compile.Post_Bind;
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

   Action : GBA.Ada_Compile.Post_Bind.Object :=
                GBA.Ada_Compile.Post_Bind.Undefined;

   function Init_Action return Boolean
   is
      Source  : GPR2.Build.Source.Object;
      Log     : GPR2.Log.Object;
      Unit    : GPR2.Build.Compilation_Unit.Object;
      Context : GPR2.Project.View.Object :=
                  Tree.Namespace_Root_Projects.First_Element;
      Success : Boolean := False;

   begin
      Unit := GPR2.Build.Compilation_Unit.Create
                       (Name    => "ada_main",
                        Context => Context);

      GPR2.Build.Compilation_Unit.Add
        (Self     => Unit,
         Kind     => S_Body,
         View     => Context,
         Path     => Context.Object_Directory.Compose ("b__main.adb"),
         Success  => Success);

      GPR2.Build.Compilation_Unit.Add
        (Self     => Unit,
         Kind     => S_Spec,
         View     => Context,
         Path     => Context.Object_Directory.Compose ("b__main.ads"),
         Success  => Success);

      Action.Initialize (Unit);
      Assert
        (not Tree.Artifacts_Database.Has_Action (Action.UID),
         "Check that action is not already in the Tree DB");
      Tree.Artifacts_Database.Add_Action (Action, Log);

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
      Args    : constant Argument_List := Action.Command;
      P_Wo    : FS.File_Descriptor;
      P_Ro    : FS.File_Descriptor;
      Ret     : Integer;
      Process : Process_Handle;
   begin
      FS.Open_Pipe (P_Ro, P_Wo);
      Process := Start (Args => Args, Stdout => P_Wo, Stderr => FS.Standerr);
      FS.Close (P_Wo);

      Ret := Wait (Process);
      Assert (Ret = 0, "Check action return code");
   end;

   declare
      Unit : GPR2.Build.Compilation_Unit.Object := Action.Input_Unit;
   begin
      Assert (Unit.Is_Defined, "Check that input unit is defined");
      Assert
        (Unit.Main_Part.Source.Base_Name = "b__main", "Check unit basename");
      Assert (Unit.Name = "ADA_MAIN", "Check unit name");
   end;

   declare
      Expected_Obj : Filesystem_String := Obj_Dir.Join ("b__main.o").Full_Name;
   begin
      Assert
        (Action.Object_File.String_Value = String (Expected_Obj),
         "Check object file path");
      Assert
        (Action.Object_File.Exists,
         "Check that object file has been correctly created");
   end;

   declare
      Expected_Ali : Filesystem_String :=
                       Obj_Dir.Join ("b__main.ali").Full_Name;
   begin
      Assert
        (Action.Ali_File.String_Value = String (Expected_Ali),
         "Check ALI file path");
      Assert
        (Action.Ali_File.Exists,
         "Check that ALI file has been correctly created");
   end;

   return A.Report;
end Test;
