with Ada.Directories;

with GPR2.Build.Actions.Ada_Compile;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;

with GPR2.Project.Tree;
with GPR2.Project.View;

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


   Ada_Comp : GBA.Ada_Compile.Object := GBA.Ada_Compile.Undefined;

   function Init_Action
     (Action : in out GBA.Ada_Compile.Object; Tree : GPR2.Project.Tree.Object)
   return Boolean;

   function Init_Action
     (Action : in out GBA.Ada_Compile.Object; Tree : GPR2.Project.Tree.Object)
   return Boolean is
      Source : GPR2.Build.Source.Object;
      Log    : GPR2.Log.Object;
   begin

      for Root of Tree.Namespace_Root_Projects loop
         for Main of Root.Mains loop
            Source := Main.View.Source (Main.Source.Simple_Name);

            Assert (Source.Has_Units, "Main must have a unit");
            for U of Source.Units loop
               Action.Initialize (Root.Unit (U.Name));
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
      end loop;

      return False;
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

   Assert (Init_Action (Ada_Comp, Tree), "Initialize the Ada compile action");

   declare
      Args    : constant Argument_List := Ada_Comp.Command;
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
      Unit : GPR2.Build.Compilation_Unit.Object := Ada_Comp.Input_Unit;
   begin
      Assert (Unit.Is_Defined, "Check that input unit is defined");
      Assert (Unit.Main_Part.Source.Base_Name = "main", "Check unit");
      Assert (Unit.Name = "main", "Check unit name");
   end;

   declare
      Expected_Obj : Filesystem_String := Obj_Dir.Join ("main.o").Full_Name;
   begin
      Assert
        (Ada_Comp.Object_File.String_Value = String (Expected_Obj),
         "Check object file path");
      Assert
        (Ada_Comp.Object_File.Exists,
         "Check that object file has been correctly created");
   end;

   declare
      Expected_Ali : Filesystem_String :=
                        Obj_Dir.Join ("main.ali").Full_Name;
   begin
      Assert
        (Ada_Comp.Ali_File.String_Value = String (Expected_Ali),
         "Check ALI file path");
      Assert
        (Ada_Comp.Ali_File.Exists,
         "Check that ALI file has been correctly created");
   end;

   Assert ((for all Dep of Ada_Comp.Dependencies =>
             (Dep.Simple_Name = "pkg.ads" or else
              Dep.Simple_Name = "main.adb" or else
              Dep.Simple_Name = "system.ads")),
           "Check dependencies");

   return A.Report;
end Test;
