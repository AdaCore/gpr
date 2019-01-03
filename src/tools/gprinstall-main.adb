------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
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

with GPRinstall.DB;
with GPRinstall.Install;
with GPRinstall.Options;
with GPRinstall.Uninstall;

procedure GPRinstall.Main is

   use Ada;
   use Ada.Directories;
   use Ada.Exceptions;

   use GNAT.OS_Lib;

   use GPR2;
   use GPRtools;

   Tree    : GPR2.Project.Tree.Object;
   Context : GPR2.Context.Object;

   Dummy   : aliased Boolean;
   --  A dummy boolean for supporting default switch like -a

   procedure Parse_Command_Line (Options : in out GPRinstall.Options.Object);
   --  Process one gprinstall command line arguments

   procedure Copyright;
   --  Output the Copyright notice

   ---------------
   -- Copyright --
   ---------------

   procedure Copyright is
   begin
      Version.Display
        ("GPRINSTALL", "2018", Version_String => Version.Long_Value);
   end Copyright;

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
        (P      : in out GPRinstall.Options.Param;
         Value  : String;
         Is_Dir : Boolean := True);
      --  Set P with value for option Name

      procedure Set_Build_Var (Swicth, Value : String);
      --  Call for each --build-var options Value being the parameter value

      procedure Add_Search_Path (Swicth, Value : String);
      --  Add Value to project search path (-aP option)

      procedure Add_Scenario_Variable (Swicth, Value : String);
      --  Add a scenario variable (-X option)

      procedure Set_Project (Switch, Value : String);
      --  Set the project file

      ---------------------------
      -- Add_Scenario_Variable --
      ---------------------------

      procedure Add_Scenario_Variable (Swicth, Value : String) is
         pragma Unreferenced (Swicth);
         I : constant Natural := Strings.Fixed.Index (Value, "=");
      begin
         if I = 0 then
            Context.Include (Name_Type (Value), "");
         else
            Context.Include
              (Name_Type (Value (Value'First .. I - 1)),
               Value (I + 1 .. Value'Last));
         end if;
      end Add_Scenario_Variable;

      ---------------------
      -- Add_Search_Path --
      ---------------------

      procedure Add_Search_Path (Swicth, Value : String) is
         pragma Unreferenced (Swicth);
      begin
         Tree.Register_Project_Search_Path
           (Path_Name.Create_Directory (Name_Type (Value)));
      end Add_Search_Path;

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
        (P      : in out GPRinstall.Options.Param;
         Value  : String;
         Is_Dir : Boolean := True)
      is
         Path : Path_Name.Object;
      begin
         if Is_Dir then
            Path := Path_Name.Create_Directory (Name_Type (Value));
         else
            Path := Path_Name.Create_File (Name_Type (Value));
         end if;

         P := (new String'(Path.Value), False);
      end Set_Param;

      -----------------
      -- Set_Project --
      -----------------

      procedure Set_Project (Switch, Value : String) is
         pragma Unreferenced (Switch);
      begin
         Options.Project_File := Path_Name.Create_File
           (Name_Type (Value),
            Optional_Name_Type (Directories.Current_Directory));
      end Set_Project;

      Argument_Count : Natural := 0;

   begin
      --  Call parent/generic command line setup

      GPRtools.Options.Setup (GPRtools.Options.Object (Options));

      Define_Switch
        (Options.Config, Set_Project'Unrestricted_Access,
         "-P:",
         Help => "Project file to install");

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
        (Options.Config, Options.RTS'Access,
         "--RTS=",
         Help     => "Use runtime <runtime> for language Ada",
         Argument => "<runtime>");

      Define_Switch
        (Options.Config, Options.Create_Dest_Dir'Access,
         "-p", "--create-missing-dirs",
         Help => "Use runtime <runtime> for language Ada");

      Define_Switch
        (Options.Config, Add_Search_Path'Unrestricted_Access,
         "-aP:",
         Help     => "Add directory dir to project search path",
         Argument => "<dir>");

      Define_Switch
        (Options.Config, Add_Scenario_Variable'Unrestricted_Access,
         "-X!",
         Help     => "Add scenario variable",
         Argument => "<NAME>=<VALUE>");

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
        (Options.Config, Options.Target_Name'Access,
         Long_Switch => "--target=",
         Help        => "Specify a target for cross platforms",
         Argument    => "<name>");

      Define_Switch
        (Options.Config, Options.Root_Dir'Access,
         Long_Switch => "--root-dir=",
         Help        => "Root directory of obj/lib/exec to relocate",
         Argument    => "<dir>");

      Define_Switch
        (Options.Config, Options.Build_Tree_Dir'Access,
         Long_Switch => "--relocate-build-tree:",
         Help        => "Root obj/lib/exec dirs are current-directory or dir",
         Argument    => "<dir>");

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

      Getopt (Options.Config);

      --  If preceding switch was -P, a project file name need to be
      --  specified, not a switch.

      if Options.Config_Project.all /= ""
        and then Options.Auto_Config_Project.all /= ""
        and then Options.Config_Project.all /= Options.Auto_Config_Project.all
      then
         raise Usage_Error with
           "several different configuration switches cannot be specified";
      end if;

      if Options.Root_Dir.all /= "" then
         declare
            Path : constant Path_Name.Object :=
                     Path_Name.Create_Directory
                       (Name_Type  (Options.Root_Dir.all));
         begin
            Options.Root_Dir :=
              new String'(Normalize_Pathname (Path.Dir_Name));
         end;
      end if;

      if Options.Mode.all /= "" then
         Options.Mode.all := Characters.Handling.To_Lower (Options.Mode.all);

         if Options.Mode.all in "dev" | "usage" then
            Set_Param
              (Options.Global_Install_Mode, Options.Mode.all, Is_Dir => False);

         else
            raise Usage_Error with "mode value must be dev or usage";
         end if;
      end if;

      if Options.Build_Tree_Dir.all /= "" then
         if Options.Build_Tree_Dir'Length = 0 then
            Options.Build_Tree_Dir := new String'(Current_Directory);

         else
            declare
               Path : constant Path_Name.Object :=
                        Path_Name.Create_Directory
                          (Name_Type (Options.Build_Tree_Dir.all));
            begin
               Options.Build_Tree_Dir := new String'(Path.Dir_Name);
            end;
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

      Read_Arguments : loop
         declare
            Arg : constant String := Get_Argument;
         begin
            exit Read_Arguments when Arg = "";

            if not Options.Project_File.Is_Defined then
               Options.Project_File := Path_Name.Create_File
                 (Name_Type (Arg),
                  Optional_Name_Type (Directories.Current_Directory));

               Argument_Count := Argument_Count + 1;

            elsif Argument_Count = 0 then
               raise Usage_Error with
                 "cannot have -P<proj> and <proj> on the same command line";
            else
               raise Usage_Error with
                 "cannot have multiple <proj> on the same command line";
            end if;
         end;
      end loop Read_Arguments;

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

      if Options.Verbose then
         Copyright;
      end if;

      if Options.Version then
         Copyright;
         return;
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

      --  Check consistency of out-of-tree build options

      if Options.Root_Dir.all /= ""
        and then Options.Build_Tree_Dir.all = ""
      then
         raise Usage_Error with
           "cannot use --root-dir without --relocate-build-tree option";
      end if;

      --  Set default Root_Dir

      if Options.Build_Tree_Dir.all /= ""
        and then Options.Root_Dir.all = ""
      then
         Options.Root_Dir :=
           new String'(Normalize_Pathname (Options.Project_File.Dir_Name));
      end if;

      --  Set default target

      if Options.Target_Name.all = "" then
         Options.Target_Name := new String'("all");
      end if;
   end Parse_Command_Line;

   Config  : Project.Configuration.Object;
   Options : GPRinstall.Options.Object;

begin
   --  First initialize and read the command line arguments

   Parse_Command_Line (Options);

   if not Options.Version then
      --  And install Ctrl-C handler

      Interrupt_Handler.Install_Sigint (GPRtools.Sigint.Handler'Access);

      --  Check command line arguments. These will be overridden when looking
      --  for the configuration file.

      --  ??? we need to handle --autoconf

      if Options.Config_Project.all /= "" then
         Config := Project.Configuration.Load
           (Path_Name.Create_File (Name_Type (Options.Config_Project.all)),
            Target => Name_Type (Options.Target_Name.all));

      else
         Config := Project.Configuration.Create
           (Project.Configuration.Default_Description,
            Target  => Name_Type (Options.Target_Name.all),
            Project => Options.Project_File);
      end if;

      --  Then, parse the user's project and the configuration file. Apply the
      --  configuration file to the project so that its settings are
      --  automatically inherited by the project.

      if Options.Uninstall_Mode then
         if Options.Global_Install_Name.Default then
            Uninstall.Process
              (Directories.Base_Name (String (Options.Project_File.Name)),
               Options);
         else
            Uninstall.Process (Options.Global_Install_Name.V.all, Options);
         end if;

      elsif Options.List_Mode then
         DB.List (Options);

      else
         Tree.Load
           (Options.Project_File, Context, Config,
            (if Options.Subdirs = null
             then ""
             else Optional_Name_Type (Options.Subdirs.all)));

         if Options.Verbose then
            for M of Tree.Log_Messages.all loop
               Text_IO.Put_Line (M.Format);
            end loop;
         end if;

         --  ??? handle configuration after loading project to get languages

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

   when Project_Error =>
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

   when E : others =>
      Text_IO.Put_Line ("error: " & Exception_Information (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end GPRinstall.Main;
