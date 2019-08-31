------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GPR2.Compilation.Registry;
with GPR2.Version;
with GPRtools.Util;

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

      Add (Self.Dry_Run,                     Next.Dry_Run);
      Add (Self.All_Projects,                Next.All_Projects);
      Add (Self.Remain_Useful,               Next.Remain_Useful);
      Add (Self.No_Project,                  Next.No_Project);
      Add (Self.Debug_Mode,                  Next.Debug_Mode);
      Add (Self.Full_Path_Name_For_Brief,    Next.Full_Path_Name_For_Brief);
      Add (Self.Remove_Empty_Dirs,           Next.Remove_Empty_Dirs);
      Add (Self.Unchecked_Shared_Lib_Import, Next.Unchecked_Shared_Lib_Import);

      Self.Mains.Union (Next.Mains);
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

      procedure Set_Project (Path : String);
      --  Set project pathname, raise exception if already done

      -----------------
      -- Set_Project --
      -----------------

      procedure Set_Project (Path : String) is
      begin
         if not Options.Project_Path.Is_Defined then
            Options.Project_Path := Project.Create (Optional_Name_Type (Path));

         else
            raise GPRtools.Usage_Error with
              '"' & Path & """, project already """
              & Options.Project_Path.Value & '"';
         end if;
      end Set_Project;

      --------------------
      -- Value_Callback --
      --------------------

      procedure Value_Callback (Switch, Value : String) is

         function Normalize_Value return String is
           (if Value /= "" and then Value (Value'First) = '='
            then Value (Value'First + 1 .. Value'Last) else Value);
         --  Remove leading '=' symbol from value for options like
         --  --config=file.cgrp

         Idx : Natural := 0;

      begin
         if Switch = "-P" then
            Set_Project (Value);

         elsif Switch = "-X" then
            Idx := Ada.Strings.Fixed.Index (Value, "=");

            if Idx = 0 then
               raise GPRtools.Usage_Error with
                 "Can't split '" & Value & "' to name and value";
            end if;

            Options.Context.Insert
              (Name_Type (Value (Value'First .. Idx - 1)),
               Value (Idx + 1 .. Value'Last));

         elsif Switch = "--config" then
            Options.Config_File :=
              Path_Name.Create_File (Name_Type (Normalize_Value));

         elsif Switch = "--autoconf" then
            --  --autoconf option for gprbuild mean that the file have to be
            --  generated if absent. The gprclean have to remove all gprbuild
            --  generated files.

            Options.Remove_Config := True;

            Options.Config_File :=
              Path_Name.Create_File (Name_Type (Normalize_Value));

         elsif Switch = "-aP" then
            Project_Tree.Register_Project_Search_Path
              (Path_Name.Create_Directory (Name_Type (Value)));

         elsif Switch = "--subdirs" then
            Options.Subdirs := To_Unbounded_String (Normalize_Value);

         elsif Switch = "--src-subdirs" then
            Options.Src_Subdirs := To_Unbounded_String (Normalize_Value);
         end if;
      end Value_Callback;

   begin
      GPRtools.Options.Setup
        (GPRtools.Options.Object (Options), GPRtools.Clean);

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access, "-P:",
         Help => "Project file");

      Define_Switch
        (Config, Options.No_Project'Access,
         Long_Switch => "--no-project",
         Help        => "Do not use project file");

      Define_Switch
        (Config, Options.All_Projects'Access, "-r",
         Help => "Clean all projects recursively");

      Define_Switch
        (Options.Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--subdirs:",
         Help        => "Real obj/lib/exec dirs are subdirs",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Value_Callback'Unrestricted_Access,
         Long_Switch => "--src-subdirs:",
         Help        => "Prepend <obj>/dir to the list of source dirs for each"
                        & " project",
         Argument    => "<dir>");

      Define_Switch
        (Config, Options.Dry_Run'Access, "-n",
         Help => "Nothing to do: only list files to delete");

      Define_Switch
        (Config, Value_Callback'Unrestricted_Access, "-X:",
         Help => "Specify an external reference for Project Files");

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
        (Config, Value_Callback'Unrestricted_Access,
         "-aP:",
         Help => "Add directory ARG to project search path");

      Define_Switch
        (Config, Dummy'Access, "-eL",
         Help => "For backwards compatibility, has no effect");

      Define_Switch
        (Config, Options.Unchecked_Shared_Lib_Import'Access,
         Long_Switch => "--unchecked-shared-lib-imports",
         Help => "Shared lib projects may import any project");

      Define_Switch
        (Config, Options.Remove_Empty_Dirs'Access,
         Switch => "-p",
         Help   => "Remove empty build directories");

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

      GPRtools.Options.Read_Remaining_Arguments
        (Options.Project_Path, Options.Mains);

      Options.Arg_Mains := not Options.Mains.Is_Empty;

      if not Options.Project_Path.Is_Defined then
         Options.Project_Path :=
           GPRtools.Util.Look_For_Default_Project
             (Quiet         => Options.Quiet,
              Implicit_Only => Options.No_Project);

         Options.Implicit_Proj := Options.Project_Path.Is_Defined
           and then Options.Project_Path.Dir_Name
             /= Ada.Directories.Current_Directory;

         if not Options.Project_Path.Is_Defined then
            Display_Help (Config);
            raise GPRtools.Usage_Error with
              "Can't determine project file to work with";
         end if;

      elsif Options.No_Project  then
         raise GPRtools.Usage_Error with
           "cannot specify --no-project with a project file";
      end if;

      Options.Clean_Build_Path
        (if Options.Implicit_Proj
         then Path_Name.Create_Directory
                (Name_Type (Ada.Directories.Current_Directory))
         else Options.Project_Path);

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
