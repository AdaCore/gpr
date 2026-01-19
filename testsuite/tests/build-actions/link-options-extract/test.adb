with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;
use Ada;
use Ada.Containers;
with GPR2.Build.Actions;    use GPR2.Build.Actions;
with GPR2.Build.Actions.Link_Options_Extract;
with GPR2.Build.Actions.Link;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GNATCOLL.OS.Process;   use GNATCOLL.OS.Process;

use GPR2;

procedure Test is
   package GBA renames GPR2.Build.Actions;

   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Project : constant String := "tree/lib.gpr";

   Link_Extract : GBA.Link_Options_Extract.Object :=
     GBA.Link_Options_Extract.Undefined;
   Link         : GBA.Link.Object := GBA.Link.Undefined;
   -----------------
   -- Init_Action --
   -----------------

   procedure Init_Actions is
   begin
      for Root of Tree.Namespace_Root_Projects loop
         Link_Extract.Initialize ("o__mylib.o", Root);

         if not (Tree.Artifacts_Database.Add_Action (Link_Extract)) then
            Ada.Text_IO.Put_Line
              ("Failed to insert the link extract action to the DB");
         end if;

         Link.Initialize
           (Kind    => GBA.Link.Library,
            Context => Root);

         if not (Tree.Artifacts_Database.Add_Action (Link)) then
            Ada.Text_IO.Put_Line
              ("Failed to insert the link action to the DB");
         end if;

         Tree.Artifacts_Database.Add_Input
           (Link.UID, Link_Extract.UID_Artifact, True);

         return;
      end loop;
   end Init_Actions;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Cmd    : Argument_List;
      Stdout : in out Unbounded_String;
      Cwd    : String := "")
   is
      Ret     : Integer;
      Process : Process_Handle;
      P_Wo    : FS.File_Descriptor;
      P_Ro    : FS.File_Descriptor;
      use FS;

   begin
      FS.Open_Pipe (P_Ro, P_Wo);
      Process :=
        Start (Args => Cmd, Cwd => Cwd, Stdout => P_Wo, Stderr => FS.Standerr);
      Ret := Wait (Process);
      FS.Close (P_Wo);

      if Ret /= 0 then
         Stdout := Null_Unbounded_String;
         Ada.Text_IO.Put_Line ("Action return code is different from 0");
      end if;

      Stdout := FS.Read (P_Ro);
   end Execute_Command;

   Stdout : Unbounded_String;
begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => True) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
   end if;

   if not Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
   end if;

   Init_Actions;

   Link_Extract.Update_Command_Line (1);
   Execute_Command
     (Link_Extract.Command_Line.Argument_List,
      Stdout,
      Link_Extract.Working_Directory.String_Value);

   if Stdout = Null_Unbounded_String then
      Ada.Text_IO.Put_Line ("Command stdout is empty");
   else
      declare
         Number_Of_Options : constant Count_Type :=
           Link.Options_From_Binder.Length;
      begin
         if not Link_Extract.Post_Command (Success, Stdout) then
            Ada.Text_IO.Put_Line ("Failed action post-command");
         end if;

         if GPR2.Build.Actions.Link.Object'Class
              (Tree.Artifacts_Database.Action_Id_To_Reference (Link.UID)
                 .Element.all)
              .Options_From_Binder
              .Length
           > Number_Of_Options
         then
            Ada.Text_IO.Put_Line ("Options extracted with success");
         else
            Ada.Text_IO.Put_Line
              ("Options coming from the linker options extractor have not been added to the linker options");
         end if;
      end;
   end if;

end Test;
