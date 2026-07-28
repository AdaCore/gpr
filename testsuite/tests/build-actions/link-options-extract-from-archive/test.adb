with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GPR2.Build.Actions; use GPR2.Build.Actions;
with GPR2.Build.Actions.Process.Archive_Table_List;
with GPR2.Build.Actions.Process.Link;
with GPR2.Build.Actions.Process.Link_Options_Extract;
with GPR2.Build.Artifacts.Library;
with GPR2.Options;
with GPR2.Project.Tree;

with GNATCOLL.OS.Process; use GNATCOLL.OS.Process;

use GPR2;

--  Regression test for the "no o__<lib>.o member" fallback in the
--  Archive_Table_List action.
--
--  test.py tampers the library archive so that the ".GPR.linker_options"
--  section lives in a regular member (pkg1.o) and the dedicated "o__mylib.o"
--  member is removed, mimicking a library built by gprbuild1.
--
--  We then drive Archive_Table_List end to end and check that, seeing no
--  "o__" member, it falls back to creating an archive-scanning
--  Link_Options_Extract action, and that the fake linker option carried by
--  the archive is forwarded to the dependent linker.

procedure Test is
   package GBA renames GPR2.Build.Actions;

   Tree    : GPR2.Project.Tree.Object;
   Opts    : GPR2.Options.Object;
   Project : constant String := "tree/lib.gpr";

   Table_List : GBA.Process.Archive_Table_List.Object :=
     GBA.Process.Archive_Table_List.Undefined;
   Link       : GBA.Process.Link.Object := GBA.Process.Link.Undefined;

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

   -------------------------
   -- Link_Has_Fake_Option --
   -------------------------

   function Link_Has_Fake_Option return Boolean is
   begin
      for Opt of
        GBA.Process.Link.Object'Class
          (Tree.Artifacts_Database.Action_Id_To_Reference (Link.UID)
             .Element.all)
          .Options_From_Binder
      loop
         if Opt = "-lgpr2_fake_marker" then
            return True;
         end if;
      end loop;

      return False;
   end Link_Has_Fake_Option;

   Stdout : Unbounded_String;

begin
   Opts.Add_Switch (GPR2.Options.P, Project);

   if not Tree.Load (Opts, With_Runtime => True) then
      Ada.Text_IO.Put_Line ("Failed to load the tree");
   end if;

   if not Tree.Update_Sources (Option => GPR2.Sources_Units_Artifacts) then
      Ada.Text_IO.Put_Line ("Failed to update sources");
   end if;

   for Root of Tree.Namespace_Root_Projects loop
      declare
         Archive : constant GPR2.Build.Artifacts.Library.Object :=
           GPR2.Build.Artifacts.Library.Create_Static
             (Root.Library_Filename);

         Extract_Id : constant Action_Id'Class :=
           GBA.Process.Link_Options_Extract.Create
             (Root.Library_Filename.Simple_Name, Root);
      begin
         Table_List.Initialize (Archive, Root, Root);

         if not Tree.Artifacts_Database.Add_Action (Table_List) then
            Ada.Text_IO.Put_Line
              ("Failed to add the archive-table-list action");
         end if;

         Link.Initialize (Kind => GBA.Process.Link.Library, Context => Root);

         if not Tree.Artifacts_Database.Add_Action (Link) then
            Ada.Text_IO.Put_Line ("Failed to add the link action");
         end if;

         --  Make the link depend on the archive-table-list action so that its
         --  Post_Execution finds it as a dependent linker.

         Tree.Artifacts_Database.Add_Input (Link.UID, Table_List.UID_Artifact);

         Table_List.Update_Command_Line (1);
         Execute_Command
           (Table_List.Command_Line.Argument_List,
            Stdout,
            Table_List.Working_Directory.String_Value);

         if not Table_List.Post_Execution (Success, Stdout) then
            Ada.Text_IO.Put_Line ("archive-table-list Post_Execution failed");
         end if;

         if not Tree.Artifacts_Database.Has_Action (Extract_Id) then
            Ada.Text_IO.Put_Line
              ("ERROR: no archive-scanning fallback action was created");
         else
            Ada.Text_IO.Put_Line ("Fallback extraction action created");

            GBA.Process.Object'Class
              (Tree.Artifacts_Database.Action_Id_To_Reference (Extract_Id)
                 .Element.all)
              .Update_Command_Line (1);

            Execute_Command
              (GBA.Process.Object'Class
                 (Tree.Artifacts_Database.Action_Id_To_Reference (Extract_Id)
                    .Element.all)
                 .Command_Line
                 .Argument_List,
               Stdout,
               GBA.Process.Object'Class
                 (Tree.Artifacts_Database.Action_Id_To_Reference (Extract_Id)
                    .Element.all)
                 .Working_Directory
                 .String_Value);

            if not Tree.Artifacts_Database.Action_Id_To_Reference (Extract_Id)
                     .Element.all
                     .Post_Execution (Success, Stdout)
            then
               Ada.Text_IO.Put_Line ("extraction Post_Execution failed");
            end if;

            if Link_Has_Fake_Option then
               Ada.Text_IO.Put_Line
                 ("Fake linker option forwarded to the linker");
            else
               Ada.Text_IO.Put_Line
                 ("ERROR: fake linker option not forwarded to the linker");
            end if;
         end if;

         exit;
      end;
   end loop;
end Test;
