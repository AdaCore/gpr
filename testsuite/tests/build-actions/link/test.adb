with Ada.Directories;
with Ada.Containers;

with GPR2.Build.Actions.Link;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNAT.OS_Lib;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;
with GNATCOLL.VFS;        use GNATCOLL.VFS;
with Test_Assert;         use Test_Assert;
use GPR2;

function Test return Integer is
   use type GPR2.Language_Id;
   use type Ada.Containers.Count_Type;
   package GBA renames GPR2.Build.Actions;

   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Messages    : GPR2.Log.Object;
   Project     : constant String := "tree/main.gpr";


   Action : GBA.Link.Object := GBA.Link.Undefined;

   function Init_Action return Boolean
   is
      Source : GPR2.Build.Source.Object;
      Log    : GPR2.Log.Object;
   begin

      for Root of Tree.Namespace_Root_Projects loop
         for Main of Root.Mains loop
            Action.Initialize
               (Executable  => Root.Executable_Directory.Compose
                                 (Filename_Type
                                   (String (Main.Source.Base_Name))),
                Main_Object_File => Root.Object_Directory.Compose
                                 (Filename_Type
                                   (String (Main.Source.Base_Name) & ".o")),
                Context     => Root);
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
         end loop;
      end loop;

      return False;
   end Init_Action;

   procedure Execute_Command (Cmd : Argument_List; Cwd : String := "") is
      Ret     : Integer;
      Process : Process_Handle;
   begin
      Process := Start (Args => Cmd, Cwd => Cwd, Stdout => FS.Standout, Stderr => FS.Standerr);
      Ret := Wait (Process);
      Assert (Ret = 0, "Check action return code");
   end Execute_Command;

   Obj_Dir      : Virtual_File;
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

   declare
      Args : Argument_List;
   begin
      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append (".." & GNAT.OS_Lib.Directory_Separator & "src" & GNAT.OS_Lib.Directory_Separator & "main.adb");
      Execute_Command (Args, Obj_Dir.Display_Full_Name);
   end;

   declare
      Args : Argument_List;
   begin
      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append (".." & GNAT.OS_Lib.Directory_Separator & "src" & GNAT.OS_Lib.Directory_Separator & "pkg.adb");
      Execute_Command (Args, Obj_Dir.Display_Full_Name);
   end;
      declare
      Args : Argument_List;
   begin
      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append (".." & GNAT.OS_Lib.Directory_Separator & "src" & GNAT.OS_Lib.Directory_Separator & "dep_two.adb");
      Execute_Command (Args, Obj_Dir.Display_Full_Name);
   end;

   declare
      Args : Argument_List;
   begin
      Args.Append ("gnatbind");
      Args.Append ("main.ali");
      Args.Append ("-o");
      Args.Append ("b__main.adb");
      Execute_Command (Args, Obj_Dir.Display_Full_Name);
   end;

   declare
      Args : Argument_List;
   begin
      Args.Append ("gcc");
      Args.Append ("-c");
      Args.Append ("b__main.adb");
      Execute_Command (Args, Obj_Dir.Display_Full_Name);
   end;

   Assert (Init_Action, "Initialize the Ada compile action");

   Action.Add_Object_File
     (GPR2.Path_Name.Create_File
        ("pkg.o", Filename_Optional (Obj_Dir.Dir_Name)));

   Action.Add_Object_File
     (GPR2.Path_Name.Create_File
        ("dep_two.o", Filename_Optional (Obj_Dir.Dir_Name)));

   Action.Add_Object_File
     (GPR2.Path_Name.Create_File
        ("b__main.o", Filename_Optional (Obj_Dir.Dir_Name)));

   Assert (Action.Input_Object_Files.Length = 4);

   Assert
     ((for all Obj of Action.Input_Object_Files => Obj.Exists),
      "Check that all input object files exist");

   Execute_Command (Action.Command);

   Assert
     (GPR2.Path_Name.Create_File
        ("main", Filename_Optional (Obj_Dir.Dir_Name)).Exists);

   return Report;
end Test;
