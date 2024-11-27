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
with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.Traces;

with GPR2.Build.Actions_Population;
with GPR2.Log;
with GPR2.Message;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
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

   procedure Delete_File
     (Name : String; Opts : GPRclean.Options.Object);
   --  Remove file if exists.
   --  Opts parameter need because the command line options is used inside of
   --  this routine and could be different for different projects because of
   --  Switches attribute in project package Clean.

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
                    ('"' & Name & """ has been deleted");
            elsif Opts.Tree.Reporter.Verbosity > No_Warnings then
               Opts.Tree.Reporter.Report
                 ("Warning: """ & Name & """ could not be deleted");
            end if;
         end if;
      end if;
   end Delete_File;

   Project_Tree : Project.Tree.Object;
   Opt          : GPRclean.Options.Object;
   Parser       : GPRtools.Options.Command_Line_Parser;
   Build_Opt    : GPR2.Build.Actions_Population.Build_Options;

begin
   GNATCOLL.Traces.Parse_Config_File;
   GPRtools.Util.Set_Program_Name ("gpr2clean");
   GPRclean.Options.Setup (Parser);
   GPRclean.Options.Parse_Command_Line (Parser, Opt);

   if not GPRtools.Options.Load_Project (Opt, GPR2.No_Error) then
      Handle_Program_Termination (Message => "");
   end if;

   Project_Tree := Opt.Tree;

   if Project_Tree.Is_Defined
     and then Project_Tree.Root_Project.Has_Archive_Builder
     and then Project_Tree.Root_Project.Archive_Builder.Empty_Values
   then
      Handle_Program_Termination
        (Exit_Code => E_Success,
         Message   => "empty Archive_builder is not supported yet.");
   end if;

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

   if Project_Tree.Root_Project.Is_Library and then Opt.Arg_Mains then
      Project_Tree.Log_Messages.Append
        (GPR2.Message.Create
           (GPR2.Message.Error,
            "main cannot be a source of a library project: """
            & String (Opt.Mains.First_Element) & '"',
            Source_Reference.Create
              (Project_Tree.Root_Project.Path_Name.Value, 0, 0)));

      Handle_Program_Termination
        (Message => "problems with main sources");
   end if;

   Project_Tree.Update_Sources;

   --  Create actions that will be used to iterate and obtain artifacts
   --  for removal.

   if Project_Tree.Root_Project.Is_Library then

      --  Create actions to build a lib

      null;
   else
      if not GPR2.Build.Actions_Population.Populate_Actions
               (Project_Tree, Build_Opt)
      then
         return To_Exit_Status (E_Abort);
      end if;
   end if;

   --  Iterate on all actions, and clean their output artifacts

   for Action of Project_Tree.Artifacts_Database.All_Actions loop
      for Artifact of Project_Tree.Artifacts_Database.Outputs (Action.UID)
      loop
         Delete_File (String (Artifact.SLOC.Filename), Opt);
      end loop;
   end loop;

   if Opt.Arg_Mains and then not Opt.Mains.Is_Empty then
      Handle_Program_Termination
        (Message => '"' & String (Opt.Mains.First_Element)
         & """ was not found in " & "the sources of any project");
   end if;

   if Opt.Remove_Config then
      Delete_File (Opt.Config_Project.String_Value, Opt);
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
