--  with Test_Assert;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Actions.Ada_Compile.Post_Bind;
with GPR2.Build.Actions.Ada_Compile.Pre_Bind;
with GPR2.Build.Actions.Compile;
with GPR2.Build.Actions.Link;
with GPR2.Build.Artifacts.Files;
with GPR2.Build.Artifacts.File_Part;
with GPR2.Build.Compilation_Unit; use GPR2.Build.Compilation_Unit;
with GPR2.Build.Process_Manager.JSON;
with GPR2.Build.Source;
with GPR2.Build.Source.Sets;
with GPR2.Build.Unit_Info.List;

with GPR2.Log;
with GPR2.Options;
with GPR2.Path_Name;

with GPR2.Project.Tree;
with GPR2.Project.View;

with GNATCOLL.VFS; use GNATCOLL.VFS;

use GPR2;

function Test return Integer is
   use type GPR2.Language_Id;
   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Log         : GPR2.Log.Object;
   Project     : constant String := "tree/main.gpr";
   Process_M   : GPR2.Build.Process_Manager.JSON.Object;
   Source      : GPR2.Build.Source.Object;
   Main_Found  : Boolean := False;

   package GBA renames GPR2.Build.Actions;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => True)
   then
      return 1;
   end if;

   Make_Dir
     (GNATCOLL.VFS.Create
       (Filesystem_String (Tree.Root_Project.Object_Directory.Value)));


   Tree.Update_Sources
     (Option   => GPR2.Sources_Units_Artifacts,
      Messages => Log);

   if Log.Has_Error then
      Log.Output_Messages;
      return 1;
   end if;

   Log.Clear;

   for Root of Tree.Namespace_Root_Projects loop
      for Main of Root.Mains loop
         Main_Found := True;
         Source := Main.View.Source (Main.Source.Simple_Name);

         declare
            A : GBA.Ada_Compile.Pre_Bind.Object;
         begin
            A.Initialize
              (Root.Unit
                 (GPR2.Build.Unit_Info.List.Element
                   (Source.Units.Iterate.First).Name));

            if not Tree.Artifacts_Database.Has_Action (A.UID) then
               Tree.Artifacts_Database.Add_Action (A, Log);

               if Log.Has_Error then
                  Log.Output_Messages (Warning => False);

                  return 1;
               end if;
            end if;

            declare
               Bind_Action : GBA.Ada_Bind.Object;
               C : GBA.Ada_Compile.Post_Bind.Object;
            begin
               Bind_Action.Initialize (A.Ali_File, Root);
               C.Initialize (Bind_Action.Output_Unit);

               declare
                  L : GBA.Link.Object;
               begin
                  L.Initialize
                     (Root.Executable
                        (Main.Source.Simple_Name, Main.Index),
                     A.Object_File, Root);

                  L.Add_Object_File (C.Object_File);

                  if not Tree.Artifacts_Database.Has_Action (Bind_Action.UID)
                  then
                     Tree.Artifacts_Database.Add_Action (Bind_Action, Log);

                     if Log.Has_Error then
                        Log.Output_Messages (Warning => False);
                        return 2;
                     end if;
                  end if;

                  if not Tree.Artifacts_Database.Has_Action (C.UID) then
                     Tree.Artifacts_Database.Add_Action (C, Log);

                     if Log.Has_Error then
                        Log.Output_Messages (Warning => False);
                        return 3;
                     end if;
                  end if;

                  if not Tree.Artifacts_Database.Has_Action (L.UID) then
                     Tree.Artifacts_Database.Add_Action (L, Log);

                  if Log.Has_Error then
                        Log.Output_Messages (Warning => False);
                        return 4;
                     end if;
                  end if;
               end;
            end;

            exit;
         end;
      end loop;

      for Src of Root.Sources (Compilable_Only => True) loop
         if Src.Language /= Ada_Language and then Src.Kind = S_Body then
            declare
               A : GBA.Compile.Object;
            begin
               A.Initialize (Src);
               if not Tree.Artifacts_Database.Has_Action (A.UID) then
                  Tree.Artifacts_Database.Add_Action (A, Log);
                  if Log.Has_Error then
                     Log.Output_Messages (Warning => False);
                     return 5;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end loop;

   if not Main_Found then
      Ada.Text_IO.Put_Line ("[ERROR] No main unit found");
      return 6;
   end if;

   --  Bind action expects to be executed in the object directory

   Change_Dir
     (GNATCOLL.VFS.Create
       (Filesystem_String (Tree.Root_Project.Object_Directory.Value)));

   Process_M.Execute (Tree.Artifacts_Database, 1);

   return 0;
end Test;
