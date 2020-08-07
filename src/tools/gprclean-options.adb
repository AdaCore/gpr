------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Text_IO;

with GPR2.Compilation.Registry;
with GPR2.Version;

package body GPRclean.Options is

   Dummy : aliased Boolean;
   --  Just to support obsolete options

   ------------
   -- Append --
   ------------

   overriding procedure Append (Self : in out Object; Next : Object) is
      package GTO renames GPRtools.Options;

      procedure Add (Left : in out Boolean; Right : Boolean) with Inline;
      --  Logically add Right to Left

      ---------
      -- Add --
      ---------

      procedure Add (Left : in out Boolean; Right : Boolean) is
      begin
         Left := Left or else Right;
      end Add;

   begin
      GTO.Append (GTO.Object (Self), GTO.Object (Next));

      Add (Self.Dry_Run,                  Next.Dry_Run);
      Add (Self.All_Projects,             Next.All_Projects);
      Add (Self.Remain_Useful,            Next.Remain_Useful);
      Add (Self.No_Project,               Next.No_Project);
      Add (Self.Debug_Mode,               Next.Debug_Mode);
      Add (Self.Full_Path_Name_For_Brief, Next.Full_Path_Name_For_Brief);
      Add (Self.Remove_Empty_Dirs,        Next.Remove_Empty_Dirs);
      Add (Self.Unchecked_Shared_Lib,     Next.Unchecked_Shared_Lib);

      Self.Args.Union (Next.Mains);
      Self.Arg_Mains := not Self.Mains.Is_Empty;
   end Append;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line
     (Options      : in out Object;
      Project_Tree : in out Project.Tree.Object;
      Parser       : Opt_Parser := Command_Line_Parser)
   is
      use Ada;

      Config : Command_Line_Configuration renames Options.Config;

      Additional : constant Boolean := Parser /= Command_Line_Parser;
      --  Parsed from package Clean attribute Switches

      procedure Value_Callback (Switch, Value : String);
      --  Accept string swithces

      --------------------
      -- Value_Callback --
      --------------------

      procedure Value_Callback (Switch, Value : String) is

         function Normalize_Value return String is
           (if Value /= "" and then Value (Value'First) = '='
            then Value (Value'First + 1 .. Value'Last) else Value);
         --  Remove leading '=' symbol from value for options like
         --  --config=file.cgrp

      begin
         if Switch = "--config" then
            Options.Config_File :=
              Path_Name.Create_File (Name_Type (Normalize_Value));

         elsif Switch = "--autoconf" then
            --  --autoconf option for gprbuild mean that the file have to be
            --  generated if absent. The gprclean have to remove all gprbuild
            --  generated files.

            Options.Remove_Config := True;

            Options.Config_File :=
              Path_Name.Create_File (Name_Type (Normalize_Value));

         elsif Switch = "--subdirs" then
            Options.Subdirs := To_Unbounded_String (Normalize_Value);
         end if;
      end Value_Callback;

   begin
      Options.Tree := Project_Tree.Reference;
      GPRtools.Options.Setup
        (GPRtools.Options.Object (Options), GPRtools.Clean);

      Define_Switch
        (Config, Options.All_Projects'Access, "-r",
         Help => "Clean all projects recursively");

      Define_Switch
        (Options.Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--subdirs:",
         Help        => "Real obj/lib/exec dirs are subdirs",
         Argument    => "<dir>");

      Define_Switch
        (Config, Options.Dry_Run'Access, "-n",
         Help => "Nothing to do: only list files to delete");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--config:",
         Help => "Specify the configuration project file name");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--autoconf:",
         Help => "Specify generated config project file name");

      Define_Switch
        (Config, Options.Remain_Useful'Access, "-c",
         Help => "Only delete compiler generated files");

      Define_Switch
        (Config, Dummy'Access, "-eL",
         Help => "For backwards compatibility, has no effect");

      Define_Switch
        (Config, Options.Remove_Empty_Dirs'Access,
         Switch => "-p",
         Help   => "Remove empty build directories");

      Define_Switch
        (Config, Options.Force_Deletions'Access,
         Switch => "-f",
         Help => "Force deletions of unwritable files");

      Getopt
        (Config, Parser => Parser, Quiet => Additional, Concatenate => False);

      GPR2.Set_Debug (Options.Debug_Mode);

      if Additional then
         --  Next options should be parsed only from command line
         return;
      end if;

      if Options.Version or else Options.Verbose then
         GPR2.Version.Display
           ("GPRCLEAN", "2018", Version_String => GPR2.Version.Long_Value);

         if Options.Version then
            GPR2.Version.Display_Free_Software;
            return;
         end if;
      end if;

      --  Now read arguments

      Options.Read_Remaining_Arguments (GPRtools.Clean);

      Options.Arg_Mains := not Options.Mains.Is_Empty;

      if Options.Slave_Env = Null_Unbounded_String
        and then Options.Distributed_Mode
      then
         Options.Slave_Env := To_Unbounded_String
           (GPR2.Compilation.Registry.Compute_Env
              (Project_Tree, Options.Slave_Env_Auto));

         if Options.Slave_Env_Auto and then Options.Verbose then
            Text_IO.Put_Line
              ("slave environment is " & To_String (Options.Slave_Env));
         end if;
      end if;
   end Parse_Command_Line;

end GPRclean.Options;
