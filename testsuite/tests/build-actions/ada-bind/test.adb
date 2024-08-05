with Ada.Directories;
with Ada.Containers;

with GNAT.OS_Lib;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Source;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;

with GPR2.Project.Tree;
with GPR2.Project.View;

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

   function Init_Action return Boolean
   is
      Source : GPR2.Build.Source.Object;
      Log    : GPR2.Log.Object;
   begin

      for Root of Tree.Namespace_Root_Projects loop
         for Main of Root.Mains loop
            Action.Initialize
               (Main_Ali => Root.Object_Directory.Compose
                              (Filename_Type
                                 (String (Main.Source.Base_Name) & ".ali")),
                Context   => Root);
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

   --  We need to create the ALI files dynamically because the
   --  compiler date used to create the ALI is stored at its first line.
   --  If the runtime ALIs date differs, then an error is raised.

   Create_Ali_File ("main");
   Create_Ali_File ("pkg");
   Create_Ali_File ("dep_two");

   Assert (Init_Action, "Initialize the Ada compile action");

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
      Unit : GPR2.Build.Compilation_Unit.Object := Action.Output_Unit;
   begin
      Assert (Unit.Is_Defined, "Check that input unit is defined");
      Assert (Unit.Main_Part.Source.Base_Name = "b__main", "Check unit");
      Assert (Unit.Name = "ADA_MAIN", "Check unit name");
   end;

   Assert (Action.Input_Alis.Length = 1);
   Assert
     ((for all Ali of Action.Input_Alis => Ali.Simple_Name = "main.ali"),
      "Check that the initial ALI file is main.ali");

   Action.Parse_Ali
     (GPR2.Path_Name.Create_File
        ("main.ali", Filename_Optional (Obj_Dir.Dir_Name)));

   Assert (Action.Input_Alis.Length = 2);

   Assert
     ((for all Ali of Action.Input_Alis =>
         Ali.Simple_Name = "main.ali" or else
         Ali.Simple_Name = "pkg.ali"),
      "Check after main.ali parsing");

   Action.Parse_Ali
     (GPR2.Path_Name.Create_File
        ("pkg.ali", Filename_Optional (Obj_Dir.Dir_Name)));

   Assert (Action.Input_Alis.Length = 5);
   Assert
     ((for all Ali of Action.Input_Alis =>
       Ali.Simple_Name = "main.ali" or else
       Ali.Simple_Name = "pkg.ali" or else
       Ali.Simple_Name = "ada.ali" or else
       Ali.Simple_Name = "a-textio.ali" or else
       Ali.Simple_Name = "dep_two.ali"),
    "Check after pkg.ali parsing");

   Assert
     ((for all Ali of Action.Input_Alis => Ali.Exists),
      "Check that all ALI files exist");

   return Report;
end Test;
