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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;

with GPR.Util;

with GPR2.Context;
with GPR2.Interrupt_Handler;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Configuration;
with GPR2.Project.Tree;
with GPR2.Source_Reference;
with GPR2.Version;

with GPRtools.Options;
with GPRtools.Sigint;
with GPRtools.Util;

with GPRinstall.DB;
with GPRinstall.Install;
with GPRinstall.Options;
with GPRinstall.Uninstall;

procedure GPRinstall.Main is

   use Ada;
   use Ada.Exceptions;

   use GNAT.OS_Lib;

   use GPR2;
   use GPRtools;

   Tree    : GPR2.Project.Tree.Object;

   Dummy   : aliased Boolean;
   --  A dummy boolean for supporting default switch like -a

   procedure Parse_Command_Line (Options : in out GPRinstall.Options.Object);
   --  Process one gprinstall command line arguments

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line (Options : in out GPRinstall.Options.Object) is

      Prefix          : aliased String_Access;
      Exec_Subdir     : aliased String_Access;
      ALI_Subdir      : aliased String_Access;
      Lib_Subdir      : aliased String_Access;
      Link_Lib_Subdir : aliased String_Access;
      Sources_Subdir  : aliased String_Access;
      Project_Subdir  : aliased String_Access;

      use GNAT.Command_Line;

      procedure Set_Param
        (P     : in out GPRinstall.Options.Param;
         Value : String);
      --  Set P with value for option Name

      procedure Set_Build_Var (Swicth, Value : String);
      --  Call for each --build-var options Value being the parameter value

      -------------------
      -- Set_Build_Var --
      -------------------

      procedure Set_Build_Var (Swicth, Value : String) is
         pragma Unreferenced (Swicth);
      begin
         if Options.Build_Vars = null then
            --  Not yet set
            Options.Build_Vars := new String'(Value);

         else
            --  Append after previous value
            Options.Build_Vars :=
              new String'(Value & ',' & Options.Build_Vars.all);
         end if;
      end Set_Build_Var;

      ---------------
      -- Set_Param --
      ---------------

      procedure Set_Param
        (P     : in out GPRinstall.Options.Param;
         Value : String) is
      begin
         P := (new String'(Value), False);
      end Set_Param;

   begin
      --  Call parent/generic command line setup

      Options.Tree := Tree.Reference;
      GPRtools.Options.Setup (Options, GPRtools.Install);

      Define_Switch
        (Options.Config, Options.Config_Project'Access,
         Long_Switch => "--config=",
         Help        => "Specify the main config project file name",
         Argument    =>  "file.cgpr");

      Define_Switch
        (Options.Config, Options.Auto_Config_Project'Access,
         Long_Switch => "--autoconf=",
         Help        => "Specify/create the main config project file name",
         Argument    =>  "file.cgpr");

      Define_Switch
        (Options.Config, Options.Create_Dest_Dir'Access,
         "-p", "--create-missing-dirs",
         Help => "Use runtime <runtime> for language Ada");

      Define_Switch
        (Options.Config, Dummy'Access,
         Switch => "-a",
         Help   => "Copy all source files (default)");
      --  This option is kept for upward compatibility but does nothing, the
      --  default is True and passing -a is actually to select the default
      --  (install all sources). See option below which actually change the
      --  way sources are installed.

      Define_Switch
        (Options.Config, Options.All_Sources'Access,
         Switch => "-m",
         Help   => "Minimal copy of sources (only those needed)",
         Value  => False);

      Define_Switch
        (Options.Config, Options.Recursive'Access,
         Switch => "-r",
         Help   => "Recursive");

      Define_Switch
        (Options.Config, Options.Force_Installations'Access,
         "-f", "--force",
         Help => "Force installation, overwrite files");

      Define_Switch
        (Options.Config, Options.Dry_Run'Access,
         "-d", "--dry-run",
         Help => "Execute nothing, display commands");

      Define_Switch
        (Options.Config, Options.Output_Stats'Access,
         Long_Switch => "--stat",
         Help        => "Display stats about installed projects,"
                        & " must be used with --list");

      Define_Switch
        (Options.Config, Options.No_Build_Var'Access,
         Long_Switch => "--no-build-var",
         Help        => "Do not generate external build variable");

      Define_Switch
        (Options.Config, Options.No_Lib_Link'Access,
         Long_Switch => "--no-lib-link",
         Help        => "Do not copy shared lib in exec/lib directory");

      Define_Switch
        (Options.Config, Options.Side_Debug'Access,
         Long_Switch => "--side-debug",
         Help        => "Write debug information into a separate file");

      Define_Switch
        (Options.Config, Options.List_Mode'Access,
         Long_Switch => "--list",
         Help        => "List all installed projects");

      Define_Switch
        (Options.Config, Options.Uninstall_Mode'Access,
         Long_Switch => "--uninstall",
         Help        => "Remove all previously installed files");

      Define_Switch
        (Options.Config, Options.Sources_Only'Access,
         Long_Switch => "--sources-only",
         Help        => "Copy project sources only");

      Define_Switch
        (Options.Config, Options.No_Project'Access,
         Long_Switch => "--no-project",
         Help        => "Do not install project file");

      Define_Switch
        (Options.Config, Prefix'Access,
         Long_Switch => "--prefix=",
         Help        => "Install destination directory");

      Define_Switch
        (Options.Config, Exec_Subdir'Access,
         Long_Switch => "--exec-subdir=",
         Help        => "The executable directory/sub-directory",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Lib_Subdir'Access,
         Long_Switch => "--lib-subdir=",
         Help        => "The library directory/sub-directory",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Link_Lib_Subdir'Access,
         Long_Switch => "--link-lib-subdir=",
         Help        => "The symlib directory/sub-directory to libraries",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, ALI_Subdir'Access,
         Long_Switch => "--ali-subdir=",
         Help        => "The ALI directory/sub-directory",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Sources_Subdir'Access,
         Long_Switch => "--sources-subdir=",
         Help        => "The sources directory/sub-directory",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Project_Subdir'Access,
         Long_Switch => "--project-subdir=",
         Help        => "The project directory/sub-directory",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Options.Build_Name'Access,
         Long_Switch => "--build-name=",
         Help        => "Build name value (default is ""Default"")",
         Argument    => "<name>");

      Define_Switch
        (Options.Config, Options.Install_Name'Access,
         Long_Switch => "--install-name=",
         Help        => "The name of the installation (manifest)",
         Argument    => "<name>");

      Define_Switch
        (Options.Config, Options.Subdirs'Access,
         Long_Switch => "--subdirs=",
         Help        => "Real obj/lib/exec dirs are subdirs",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Options.Mode'Access,
         Long_Switch => "--mode=",
         Help        => "Kind of installation (default is ""dev"")",
         Argument    => "dev|usage");

      Define_Switch
        (Options.Config, Set_Build_Var'Unrestricted_Access,
         Long_Switch => "--build-var=",
         Help        => "Name of the variable which identify a build)",
         Argument    => "<name>");

      Define_Switch
        (Options.Config, Options.Install_Manifest'Unrestricted_Access,
         Long_Switch => "--no-manifest",
         Help        => "Do not generate the manifest file",
         Value       => False);

      Getopt (Options.Config);

      GPR2.Set_Debug (Options.Debug_Mode);

      --  If preceding switch was -P, a project file name need to be
      --  specified, not a switch.

      if Options.Config_Project.all /= ""
        and then Options.Auto_Config_Project.all /= ""
        and then Options.Config_Project.all /= Options.Auto_Config_Project.all
      then
         raise Usage_Error with
           "several different configuration switches cannot be specified";
      end if;

      if Options.Mode.all /= "" then
         Options.Mode.all := Characters.Handling.To_Lower (Options.Mode.all);

         if Options.Mode.all in "dev" | "usage" then
            Set_Param (Options.Global_Install_Mode, Options.Mode.all);

         else
            raise Usage_Error with "mode value must be dev or usage";
         end if;
      end if;

      if Prefix.all /= "" then
         Set_Param (Options.Global_Prefix_Dir, Prefix.all);
      end if;

      if Exec_Subdir.all /= "" then
         Set_Param (Options.Global_Exec_Subdir, Exec_Subdir.all);
      end if;

      if Sources_Subdir.all /= "" then
         Set_Param (Options.Global_Sources_Subdir, Sources_Subdir.all);
      end if;

      if ALI_Subdir.all /= "" then
         Set_Param (Options.Global_ALI_Subdir, ALI_Subdir.all);
      end if;

      if Lib_Subdir.all /= "" then
         Set_Param (Options.Global_Lib_Subdir, Lib_Subdir.all);
      end if;

      if Link_Lib_Subdir.all /= "" then
         Set_Param (Options.Global_Link_Lib_Subdir, Link_Lib_Subdir.all);
      end if;

      if Project_Subdir.all /= "" then
         Set_Param (Options.Global_Project_Subdir, Project_Subdir.all);
      end if;

      if Options.Install_Name.all /= "" then
         Options.Global_Install_Name := (Options.Install_Name, False);
      end if;

      --  Now read arguments

      Options.Read_Remaining_Arguments (GPRtools.Install);

      if Options.Uninstall_Mode then
         if Options.Project_File.Is_Defined then
            Options.Args.Include
              (String (Options.Project_File.Name (Extension => False)));
         end if;

         case Options.Args.Length is
            when 0 =>
               raise Usage_Error with "A project file or an install name is"
                 & " required with --uninstall";
            when 1 =>
               null;
            when others =>
               raise Usage_Error with "Can have only one uninstall name";
         end case;

      elsif not Options.Args.Is_Empty then
         raise Usage_Error with
           "Parameter " & Options.Args.First_Element & " unrecognized";
      end if;

      --  check -a & -m
      --  check -v & -q
      --  check --list & --uninstall

      --  If --lib-subdir set and not --ali-subdir then makes the later with
      --  --lib-subdir.

      if not Options.Global_Lib_Subdir.Default
        and then Options.Global_ALI_Subdir.Default
      then
         Options.Global_ALI_Subdir :=
           GPRinstall.Options.Dup (Options.Global_Lib_Subdir);
      end if;

      if Options.Version or else Options.Verbose then
         Version.Display
           ("GPRINSTALL", "2018", Version_String => Version.Long_Value);

         if Options.Version then
            Version.Display_Free_Software;
            return;
         end if;
      end if;

      if Options.Build_Name.all /= "default"
        and then Options.Uninstall_Mode
      then
         raise Usage_Error with
           "cannot specify --build-name in uninstall mode";
      end if;

      if Options.Build_Vars /= null and then Options.Uninstall_Mode then
         raise Usage_Error with
           "cannot specify --build-var in uninstall mode";
      end if;

      if Options.Build_Vars /= null and then Options.No_Build_Var then
         raise Usage_Error with
           "cannot specify --build-var and --no-build-var";
      end if;

      if Options.Output_Stats and then not Options.List_Mode then
         raise Usage_Error with
           "cannot specify --stat in install/uninstall mode";
      end if;

      if Options.No_Project
        and then not Options.Global_Project_Subdir.Default
      then
         raise Usage_Error with
           "cannot specify --no-project and --project-subdir";
      end if;

      --  If no project file was specified, this is an error

      if not Options.Project_File.Is_Defined
        and then not Options.List_Mode
        and then not Options.Uninstall_Mode
      then
         raise Usage_Error with "no project file specified";
      end if;

      --  Check prefix, if not specified set to default toolchain

      if Options.Global_Prefix_Dir.V = null then
         --  Set to default for current toolchain
         Options.Global_Prefix_Dir :=
           (new String'(GPR.Util.Executable_Prefix_Path), True);

      elsif Options.Global_Prefix_Dir.V.all = "" then
         raise Usage_Error with "--prefix argument cannot be empty";
      end if;

      if Options.Project_File.Is_Defined then
         Options.Clean_Build_Path (Options.Project_File);
      end if;
   end Parse_Command_Line;

   Config  : Project.Configuration.Object;
   Options : GPRinstall.Options.Object;

begin
   GPRtools.Util.Set_Program_Name ("gprinstall");

   --  Initialize and read the command line arguments

   Parse_Command_Line (Options);

   if not Options.Version then
      --  And install Ctrl-C handler

      Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

      --  Check command line arguments. These will be overridden when looking
      --  for the configuration file.
      --
      --  If configuration project is specified then load it, otherwise we will
      --  conduct an autoconf setup.

      if Options.Config_Project.all /= "" then
         Config := Project.Configuration.Load
           (Path_Name.Create_File (Name_Type (Options.Config_Project.all)),
            Target => Name_Type
              (Strings.Unbounded.To_String (Options.Target)));
      end if;

      --  Then, parse the user's project and the configuration file. Apply the
      --  configuration file to the project so that its settings are
      --  automatically inherited by the project.

      if Options.Uninstall_Mode then
         if Options.Global_Install_Name.Default then
            Uninstall.Process (Options.Args.First_Element, Options);
         else
            Uninstall.Process (Options.Global_Install_Name.V.all, Options);
         end if;

      elsif Options.List_Mode then
         DB.List (Options);

      else
         if Config.Is_Defined then
            Tree.Load
              (Options.Project_File, Options.Context, Config,
               Options.Build_Path,
               (if Options.Subdirs = null
                then ""
                else Optional_Name_Type (Options.Subdirs.all)));
         else
            --  No configuration, go with auto-configuration

            Tree.Load_Autoconf
              (Options.Project_File, Options.Context, Options.Build_Path,
               (if Options.Subdirs = null
                then ""
                else Optional_Name_Type (Options.Subdirs.all)));
         end if;

         if Options.Verbose then
            for M of Tree.Log_Messages.all loop
               Text_IO.Put_Line (M.Format);
            end loop;
         end if;

         Install.Process (Tree, Options);
      end if;
   end if;

exception
   when GNAT.Command_Line.Exit_From_Command_Line
      | GNAT.Command_Line.Invalid_Switch
      | GNAT.Command_Line.Invalid_Parameter
      =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : Usage_Error =>
      Text_IO.Put_Line ("gprinstall: " & Exception_Message (E));
      GNAT.Command_Line.Try_Help;

   when Project_Error | Processing_Error =>
      if Options.Verbose then
         --  Display all messagges
         for M of Tree.Log_Messages.all loop
            Text_IO.Put_Line (M.Format);
         end loop;

      else
         --  Display only errors
         for C in Tree.Log_Messages.Iterate
           (False, False, True, True, True)
         loop
            Text_IO.Put_Line (Log.Element (C).Format);
         end loop;
      end if;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : Constraint_Error =>
      Text_IO.Put_Line
        ("gprinstall: "
         & (if Options.Verbose
            then Exception_Information (E)
            else Exception_Message (E)));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Text_IO.Put_Line ("error: " & Exception_Information (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end GPRinstall.Main;
