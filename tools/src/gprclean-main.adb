------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.Traces;

with GPR2.Build.Actions_Population;
with GPR2.Build.Actions;
with GPR2.Build.Actions.Compile;
with GPR2.Build.Actions.Post_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Log;
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Configuration;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Reporter;
with GPR2.Source_Reference;

with GPRtools.Options;
with GPRtools.Program_Termination;
with GPRtools.Util;

with GPRclean.Options;

function GPRclean.Main return Ada.Command_Line.Exit_Status is

   use Ada;
   use Ada.Exceptions;

   use GPR2;
   use GPRtools;
   use GPRtools.Program_Termination;
   use GPR2.Path_Name;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PAI renames GPR2.Project.Attribute_Index;

   procedure Delete_File
     (Name : String; Opts : GPRclean.Options.Object);
   --  Remove file if exists.
   --  Opts parameter need because the command line options is used inside of
   --  this routine and could be different for different projects because of
   --  Switches attribute in project package Clean.

   procedure Remove_Artifacts_Dirs
     (View : GPR2.Project.View.Object; Opts : GPRclean.Options.Object);
   --  Removes the empty obj/lib/exec dirs of View

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File
     (Name : String; Opts : GPRclean.Options.Object)
   is
      use GNAT.OS_Lib;
      use GPR2.Reporter;
      Success : Boolean := False;
   begin
      if Is_Regular_File (Name) then
         if Opts.Dry_Run then
            Text_IO.Put_Line (Name);

         else
            if GNAT.OS_Lib.Is_Owner_Writable_File (Name) then
               Delete_File (Name, Success);

            elsif Opts.Force_Deletions then
               GNAT.OS_Lib.Set_Writable (Name);
               Delete_File (Name, Success);
            end if;

            if Success then
               Opts.Tree.Reporter.Report
                 ('"' & Name & """ has been deleted",
                  Level => GPR2.Message.Optional);
            elsif Opts.Tree.Reporter.Verbosity > No_Warnings then
               Opts.Tree.Reporter.Report
                 ("Warning: """ & Name & """ could not be deleted");
            end if;
         end if;
      end if;
   end Delete_File;

   procedure Remove_Artifacts_Dirs
     (View : GPR2.Project.View.Object; Opts : GPRclean.Options.Object)
   is
      procedure Remove_Dir (Path : GPR2.Path_Name.Object);

      procedure Remove_Dir (Path : GPR2.Path_Name.Object) is
         use Ada.Directories;
      begin
         if Kind (Path.String_Value) = Directory then
            Delete_Directory (Path.String_Value);
         end if;

      exception
         when Use_Error =>
            declare
               Search : Search_Type;
            begin
               Start_Search (Search, Path.String_Value, "");
               Opts.Tree.Reporter.Report
                 ("warning: Directory """ & Path.String_Value
                  & """ could not be removed"
                  & (if More_Entries (Search)
                    then " because it is not empty"
                    else "") & '.');
            end;
      end Remove_Dir;
   begin
      if View.Kind in GPR2.With_Object_Dir_Kind then
         Remove_Dir (View.Object_Directory);

         if View.Is_Namespace_Root
           and then View.Has_Mains
           and then View.Executable_Directory /= View.Object_Directory
         then
            Remove_Dir (View.Executable_Directory);
         end if;
      end if;

      if View.Is_Library then
         Remove_Dir (View.Library_Directory);

         if View.Library_Ali_Directory /= View.Library_Directory then
            Remove_Dir (View.Library_Ali_Directory);
         end if;
      end if;
   end Remove_Artifacts_Dirs;

   Project_Tree  : Project.Tree.Object;
   Opt           : GPRclean.Options.Object;
   Parser        : GPRtools.Options.Command_Line_Parser;
   Lang          : GPR2.Language_Id;
   Artifact_Path : Path_Name.Object;
   Conf          : GPR2.Project.View.Object;

   use type GPR2.Project.View.Object;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gprclean");
   GPRclean.Options.Setup (Parser);
   GPRclean.Options.Parse_Command_Line (Parser, Opt);

   if not GPRtools.Options.Load_Project (Opt, GPR2.No_Error) then
      Handle_Program_Termination (Message => "");
   end if;

   Project_Tree := Opt.Tree;

   --  Check gprclean's Switch attribute from loaded project

   declare
      Attr : constant GPR2.Project.Attribute.Object :=
               Project_Tree.Root_Project.Attribute (PRA.Clean.Switches);
   begin
      if Attr.Is_Defined then
         Opt := (GPRtools.Options.Empty_Options
                     with others => <>);
         GPRclean.Options.Parse_Attribute_Switches
           (Parser, Opt, Attr.Values);

         --  re-parse the command line to allow it to overwrite project
         --  defined Switches attribute.

         GPRclean.Options.Parse_Command_Line (Parser, Opt);

         --  Note that we never need to reload the tree, as we ensured that
         --  no switch modifying the configuration of the project or the
         --  way we load the project tree is allowed in the Switches
         --  attribute.
      end if;
   end;

   if Project_Tree.Has_Configuration
     and then Project_Tree.Configuration.Log_Messages.Has_Element
       (Warning  => True,
        Hint     => False,
        Error    => False)
   then
      Project_Tree.Log_Messages.Append
        (GPR2.Message.Create
           (GPR2.Message.Warning,
            "Cleaning may be incomplete, as there were problems during"
            & " auto-configuration",
            Source_Reference.Create
              (Project_Tree.Root_Project.Path_Name.Value, 0, 0)));
   end if;

   if Project_Tree.Has_Configuration then
      Conf := Project_Tree.Configuration.Corresponding_View;
   end if;

   Project_Tree.Update_Sources;

   --  Create actions that will be used to iterate and obtain artifacts
   --  for removal.

   if not GPR2.Build.Actions_Population.Populate_Actions
     (Project_Tree, Opt.Build_Options)
   then
      return To_Exit_Status (E_Abort);
   end if;

   --  Iterate on all actions, and clean their output artifacts

   for Action of Project_Tree.Artifacts_Database.All_Actions loop
      if not Action.View.Is_Externally_Built
        and then (Opt.All_Projects
                  or else Action.View = Project_Tree.Root_Project)
        and then (not Opt.Compil_Only
                  or else Action in GPR2.Build.Actions.Compile.Object'Class
                  or else Action in GPR2.Build.Actions.Post_Bind.Object'Class)
      then
         for Artifact of
           Project_Tree.Artifacts_Database.Outputs (Action.UID)
         loop
            if Artifact in GPR2.Build.Artifacts.Files.Object'Class then
               Artifact_Path :=
                 GPR2.Build.Artifacts.Files.Object'Class (Artifact).Path;

               Delete_File (Artifact_Path.String_Value, Opt);

               if Action in GPR2.Build.Actions.Compile.Object'Class then
                  Lang :=
                    GPR2.Build.Actions.Compile.Object'Class (Action).Language;
                  declare
                     Src_Exts : constant GPR2.Project.Attribute.Object :=
                                  Conf.Attribute
                                    (PRA.Clean.Source_Artifact_Extensions,
                                     PAI.Create (Lang));
                     Obj_Exts : constant GPR2.Project.Attribute.Object :=
                                  Conf.Attribute
                                    (PRA.Clean.Object_Artifact_Extensions,
                                     PAI.Create (Lang));
                     Obj_BN   : constant Filename_Type :=
                                  Artifact_Path.Base_Filename;
                     Obj_Dir  : constant Path_Name.Object :=
                                  Artifact_Path.Containing_Directory;
                     Path     : Path_Name.Object;
                  begin
                     if Obj_Exts.Is_Defined then
                        for Val of Obj_Exts.Values loop
                           Path := Obj_Dir.Compose
                             (Obj_BN &
                              Filename_Type
                                (if Val.Text (Val.Text'First) = '.'
                                 then Val.Text
                                 else "." & Val.Text));
                           Delete_File (Path.String_Value, Opt);
                        end loop;
                     end if;

                     if Src_Exts.Is_Defined then
                        for Val of Src_Exts.Values loop
                           Path := Obj_Dir.Compose
                             (Obj_BN &
                              Filename_Type
                                (if Val.Text (Val.Text'First) = '.'
                                 then Val.Text
                                 else "." & Val.Text));
                           Delete_File (Path.String_Value, Opt);
                        end loop;
                     end if;
                  end;
               end if;
            end if;
         end loop;

         Delete_File
           (Action.View.Object_Directory.Compose
              (GPR2.Build.Actions.Db_Filename (Action.UID)).String_Value,
            Opt);
      end if;
   end loop;

   if Opt.Remove_Config then
      Delete_File (Opt.Config_Project.String_Value, Opt);
   end if;

   if Opt.Remove_Empty_Dirs then
      if Opt.All_Projects then
         for V of Project_Tree.Ordered_Views loop
            if not V.Is_Externally_Built then
               Remove_Artifacts_Dirs (Project_Tree.Root_Project, Opt);
            end if;
         end loop;

      else
         Remove_Artifacts_Dirs (Project_Tree.Root_Project, Opt);
      end if;
   end if;

   return To_Exit_Status (E_Success);

exception
   when E : GPR2.Options.Usage_Error =>
      Handle_Program_Termination
        (Display_Command_Line_Help => True,
         Force_Exit                => False,
         Message                   => Exception_Message (E));
      return To_Exit_Status (E_Fatal);

   when Project_Error  =>
   Handle_Program_Termination
     (Force_Exit => False,
      Message    =>
        '"' & String (Opt.Project_File.Simple_Name) &
        """ processing failed");
      return To_Exit_Status (E_Fatal);

   when E_Program_Termination =>
      return To_Exit_Status (E_Fatal);

   when E : others =>
      Handle_Program_Termination
        (Force_Exit => False,
         Exit_Cause => E_Generic,
         Message    => Exception_Message (E));
      return To_Exit_Status (E_Fatal);
end GPRclean.Main;
