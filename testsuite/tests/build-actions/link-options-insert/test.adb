with Ada.Text_IO;
with GPR2.Containers;
with GPR2.Build.Actions.Link_Options_Insert;
with GPR2.Build.Artifacts.Object_File;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

use GPR2;

procedure Test is
   package GBA renames GPR2.Build.Actions;

   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Project : constant String := "tree/lib.gpr";

   Action : GBA.Link_Options_Insert.Object :=
     GBA.Link_Options_Insert.Undefined;

   -----------------
   -- Init_Action --
   -----------------

   procedure Init_Action is
      Object_File : GPR2.Build.Artifacts.Object_File.Object :=
        GPR2.Build.Artifacts.Object_File.Create
          (GPR2.Path_Name.Create_File ("tree/obj/pkg.o"));
      Options     : GPR2.Containers.Value_List;
   begin
      Options.Append ("Option1");
      Options.Append ("--Option2");
      Options.Append ("-o");
      Options.Append ("--Option1=hello");
      Options.Append ("option with whitespaces");

      for Root of Tree.Namespace_Root_Projects loop
         Action.Initialize (Object_File, Options, Root);

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
   begin
      Process :=
        Start
          (Args   => Cmd,
           Cwd    => Cwd,
           Stdout => FS.Standout,
           Stderr => FS.Standerr);
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

   Ada.Text_IO.Put_Line (Action.Command_Line.Signature);
   Action.Update_Command_Line (1);
   Execute_Command
     (Action.Command_Line.Argument_List,
      Action.Working_Directory.String_Value);

end Test;
