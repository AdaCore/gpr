------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with GPR2.Path_Name;

with GPRtools.Command_Line;
with GPRtools.Util;

package body GPRinstall.Options is

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   procedure Set_Param
     (P     : in out GPRinstall.Options.Param;
      Value : String);
   --  Set P with value for option Name

   function "+" (US : Unbounded_String) return String
     renames To_String;
   function "+" (S : String) return Unbounded_String
     renames To_Unbounded_String;

   ---------------
   -- On_Switch --
   ---------------

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String)
   is
      pragma Unreferenced (Parser, Index);
      use type GPRtools.Command_Line.Switch_Type;
      use GPRtools;

      Result   : constant access Object := Object (Res.all)'Access;
   begin
      if Arg = "-p" then
         Result.Create_Dest_Dir := True;

      elsif Arg = "-a" then
         null;

      elsif Arg = "-m" then
         Result.All_Sources := False;

      elsif Arg = "-r" then
         Result.Recursive := True;

      elsif Arg = "-f" then
         Result.Force_Installations := True;

      elsif Arg = "-d" then
         Result.Dry_Run := True;

      elsif Arg = "--stat" then
         Result.Output_Stats := True;

      elsif Arg = "--no-build-var" then
         Result.No_Build_Var := True;

      elsif Arg = "--no-lib-link" then
         Result.No_Lib_Link := True;

      elsif Arg = "--side-debug" then
         Result.Side_Debug := True;

      elsif Arg = "--list" then
         Result.List_Mode := True;

      elsif Arg = "--uninstall" then
         Result.Uninstall_Mode := True;

      elsif Arg = "--sources-only" then
         Result.Sources_Only := True;

      elsif Arg = "--no-project" then
         Result.No_GPR_Install := True;

      elsif Arg = "--prefix" then
         if Param /= "" then
            Set_Param (Result.Global_Prefix_Dir, Param);
         end if;

      elsif Arg = "--exec-subdir" then
         if Param /= "" then
            Set_Param (Result.Global_Exec_Subdir, Param);
         end if;

      elsif Arg = "--lib-subdir" then
         if Param /= "" then
            Set_Param (Result.Global_Lib_Subdir, Param);
         end if;

      elsif Arg = "--link-lib-subdir" then
         if Param /= "" then
            Set_Param (Result.Global_Link_Lib_Subdir, Param);
         end if;

      elsif Arg = "--ali-subdir" then
         if Param /= "" then
            Set_Param (Result.Global_ALI_Subdir, Param);
         end if;

      elsif Arg = "--sources-subdir" then
         if Param /= "" then
            Set_Param (Result.Global_Sources_Subdir, Param);
         end if;

      elsif Arg = "--project-subdir" then
         if Param /= "" then
            Set_Param (Result.Global_Project_Subdir, Param);
         end if;

      elsif Arg = "--build-name" then
         Result.Build_Name := To_Unbounded_String (Param);

      elsif Arg = "--install-name" then
         if Param /= "" then
            Set_Param (Result.Global_Install_Name, Param);
         end if;

      elsif Arg = "--mode" then
         declare
            Low : constant String := Ada.Characters.Handling.To_Lower (Param);
         begin
            if Low in "dev" | "usage" then
               Set_Param (Result.Global_Install_Mode, Low);
            else
               raise Usage_Error with "mode value must be dev or usage";
            end if;
         end;

      elsif Arg = "--build-var" then
         if Length (Result.Build_Vars) = 0 then
            Result.Build_Vars := To_Unbounded_String (Param);
         else
            Result.Build_Vars :=
              To_Unbounded_String (Param & ",") & Result.Build_Vars;
         end if;

      elsif Arg = "--no-manifest" then
         Result.Install_Manifest := False;

      end if;
   end On_Switch;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line
     (Options : in out GPRinstall.Options.Object;
      Tree    : in out GPR2.Project.Tree.Object)
   is
      use GPRtools.Command_Line;

      Parser          : GPRtools.Options.Command_Line_Parser;
      Install_Group   : GPRtools.Command_Line.Argument_Group;

   begin
      --  Call parent/generic command line setup
      GPRtools.Options.Setup (GPRtools.Install);

      Parser := GPRtools.Options.Create
        (Initial_Year           => "2018",
         Allow_No_Project       => False,
         Allow_Distributed      => False,
         Allow_Implicit_Project => False);

      Options.Tree := Tree.Reference;

      Install_Group := Parser.Add_Argument_Group
        (Name     => "install",
         Callback => On_Switch'Access,
         Help     => "gprinstall options");

      Parser.Add_Argument
        (Install_Group,
         Create
           (Name     => "-p",
            Alt_Name => "--create-missing-dirs",
            Help     => "Create directories when missing"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name   => "-a",
            Help   => "Copy all source files (default)",
            Hidden => True));
      --  This option is kept for upward compatibility but does nothing, the
      --  default is True and passing -a is actually to select the default
      --  (install all sources). See option below which actually change the
      --  way sources are installed.

      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "-m",
            Help => "Minimal copy of sources (only those needed)"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "-r",
            Help => "Recursive"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name     => "-f",
            Alt_Name => "--force",
            Help     => "Force installation, overwrite files"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name     => "-d",
            Alt_Name => "--dry-run",
            Help     => "Execute nothing, display commands"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--stat",
            Help => "Display stats about installed projects," &
                    " must be used with --list"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--no-build-var",
            Help => "Do not generate external build variable"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--no-lib-link",
            Help => "Do not copy shared lib in exec/lib directory"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--side-debug",
            Help => "Write debug information into a separate file"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--list",
            Help => "List all installed projects"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--uninstall",
            Help => "Remove all previously installed files"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--sources-only",
            Help => "Copy project sources only"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--no-project",
            Help => "Do not install project file"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--prefix",
            Help      => "Install destination directory",
            Delimiter => Equal,
            Parameter => "<dir>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--exec-subdir",
            Help      => "The executable directory/sub-directory",
            Delimiter => Equal,
            Parameter => "<dir>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--lib-subdir",
            Help      => "The library directory/sub-directory",
            Delimiter => Equal,
            Parameter => "<dir>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--link-lib-subdir",
            Help      => "The symlib directory/sub-directory to libraries",
            Delimiter => Equal,
            Parameter => "<dir>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--ali-subdir",
            Help      => "The ALI directory/sub-directory",
            Delimiter => Equal,
            Parameter => "<dir>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--sources-subdir",
            Help      => "The sources directory/sub-directory",
            Delimiter => Equal,
            Parameter => "<dir>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--project-subdir",
            Help      => "The project directory/sub-directory",
            Delimiter => Equal,
            Parameter => "<dir>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--build-name",
            Help      => "Build name value (default is ""Default"")",
            Delimiter => Equal,
            Parameter => "<name>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--install-name",
            Help      => "The name of the installation (manifest)",
            Delimiter => Equal,
            Parameter => "<name>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--mode",
            Help      => "Kind of installation (default is ""dev"")",
            Delimiter => Equal,
            Parameter => "dev|usage"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name      => "--build-var",
            Help      => "Name of the variable which identify a build)",
            Delimiter => Equal,
            Parameter => "<name>"));
      Parser.Add_Argument
        (Install_Group,
         Create
           (Name => "--no-manifest",
            Help => "Do not generate the manifest file"));

      Parser.Get_Opt (Options);

      --  Now read arguments

      if Options.Uninstall_Mode then
         if Options.Project_File.Is_Defined then
            Options.Args.Include
              (String (Options.Project_File.Name (Extension => False)));
         end if;

         case Options.Args.Length is
            when 0 =>
               raise GPRtools.Usage_Error with
                 "A project file or an install name is"
                 & " required with --uninstall";
            when 1 =>
               null;
            when others =>
               raise GPRtools.Usage_Error with
                 "Can have only one uninstall name";
         end case;

      elsif not Options.Args.Is_Empty then
         raise GPRtools.Usage_Error with
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
         Options.Global_ALI_Subdir := Options.Global_Lib_Subdir;
      end if;

      if To_String (Options.Build_Name) /= "default"
        and then Options.Uninstall_Mode
      then
         raise GPRtools.Usage_Error with
           "cannot specify --build-name in uninstall mode";
      end if;

      if Length (Options.Build_Vars) > 0 and then Options.Uninstall_Mode then
         raise GPRtools.Usage_Error with
           "cannot specify --build-var in uninstall mode";
      end if;

      if Length (Options.Build_Vars) > 0 and then Options.No_Build_Var then
         raise GPRtools.Usage_Error with
           "cannot specify --build-var and --no-build-var";
      end if;

      if Options.Output_Stats and then not Options.List_Mode then
         raise GPRtools.Usage_Error with
           "cannot specify --stat in install/uninstall mode";
      end if;

      if Options.No_GPR_Install
        and then not Options.Global_Project_Subdir.Default
      then
         raise GPRtools.Usage_Error with
           "cannot specify --no-project and --project-subdir";
      end if;

      --  If no project file was specified, this is an error

      if not Options.Project_File.Is_Defined
        and then not Options.List_Mode
        and then not Options.Uninstall_Mode
      then
         raise GPRtools.Usage_Error with "no project file specified";
      end if;

      --  Check prefix, if not specified set to default toolchain

      if Options.Global_Prefix_Dir.Default then
         --  Set to default for current toolchain
         Options.Global_Prefix_Dir :=
           (+(GPRtools.Util.Executable_Prefix_Path), True);
      end if;
   end Parse_Command_Line;
   -----------------
   -- Project_Dir --
   -----------------

   function Project_Dir (Self : Object) return String is
      use GPR2;
   begin
      if OS_Lib.Is_Absolute_Path (+Self.Global_Project_Subdir.V) then
         return +Self.Global_Project_Subdir.V;
      else
         return Path_Name.Create_Directory
           (Filename_Type (+Self.Global_Project_Subdir.V),
            Filename_Type (+Self.Global_Prefix_Dir.V)).Dir_Name;
      end if;
   end Project_Dir;

   ---------------
   -- Set_Param --
   ---------------

   procedure Set_Param
     (P     : in out GPRinstall.Options.Param;
      Value : String) is
   begin
      P := (To_Unbounded_String (Value), False);
   end Set_Param;
end GPRinstall.Options;
