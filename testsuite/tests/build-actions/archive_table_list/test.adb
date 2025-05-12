with Ada.Text_IO;
with GPR2.Build.Artifacts;
with GPR2.Build.Artifacts.Library;
with GPR2.Build.Actions.Archive_Table_List;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

use GPR2;

procedure Test is
   package GBA renames GPR2.Build.Actions;

   Tree        : GPR2.Project.Tree.Object;
   Opts        : GPR2.Options.Object;
   Project     : constant String := "tree/lib.gpr";

   Action : GBA.Archive_Table_List.Object := GBA.Archive_Table_List.Undefined;

   -----------------
   -- Init_Action --
   -----------------

   procedure Init_Action
   is
      Archive : GPR2.Build.Artifacts.Library.Object :=
      GPR2.Build.Artifacts.Library.Create (GPR2.Path_Name.Create_File ("tree/lib/libmylib.a"));
   begin
      for Root of Tree.Namespace_Root_Projects loop
         Action.Initialize (Archive, Root);

         if not (Tree.Artifacts_Database.Add_Action (Action)) then
            Ada.Text_IO.Put_Line ("Failed to insert the action to the DB");
         end if;

         return;
      end loop;
   end Init_Action;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command (Cmd : Argument_List; Cwd : String := "") is
      Ret     : Integer;
      Process : Process_Handle;
      use FS;
   begin
      Process := Start (Args => Cmd, Cwd => Cwd, Stdout => FS.Standout, Stderr => FS.Standerr);
      Ret := Wait (Process);
      if Ret /= 0 then
         Ada.Text_IO.Put_Line ("Action return code is different from 0");
      end if;
   end Execute_Command;

begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => True) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
   end if;

   if not Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
   end if;

   Init_Action;

   Action.Update_Command_Line (1);
   Execute_Command (Action.Command_Line.Argument_List,
                     Action.Working_Directory.String_Value);
end Test;
