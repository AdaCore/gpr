------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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
with Ada.Task_Attributes;

with GNAT.OS_Lib;

with GPR2.Compilation.Registry;
with GPR2.Project.Registry.Pack;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

with GNATCOLL.Utils;

with GPRtools.Util;

package body GPRtools.Options is

   type Object_Class is access all Object'Class;

   package TLS is new Ada.Task_Attributes (Object_Class, null);

   package PRP renames GPR2.Project.Registry.Pack;

   procedure Value_Callback (Switch, Value : String);
   --  Used for the command-line options parsing

   ------------
   -- Append --
   ------------

   procedure Append (Self : in out Object; Next : Object) is
   begin
      Self.Verbosity := Verbosity_Level'Max (Self.Verbosity, Next.Verbosity);
   end Append;

   ----------------------
   -- Clean_Build_Path --
   ----------------------

   procedure Clean_Build_Path
     (Self : in out Object; Project : GPR2.Path_Name.Object)
   is
      use GPR2, GPR2.Path_Name;

      function Project_Dir return GPR2.Path_Name.Object is
        (Create_Directory (Filename_Type (Project.Dir_Name)));

   begin
      if not Self.Build_Path.Is_Defined then
         --  Check consistency of out-of-tree build options

         if Self.Root_Path.Is_Defined then
            raise Usage_Error with
              "cannot use --root-dir without --relocate-build-tree option";
         end if;

         Self.Build_Path := Project_Dir;

      elsif Self.Root_Path.Is_Defined then
         Self.Build_Path := Create_Directory
           (Project_Dir.Relative_Path (Self.Root_Path).Name,
            Filename_Type (Self.Build_Path.Value));
      end if;
   end Clean_Build_Path;

   ------------------------------
   -- Read_Remaining_Arguments --
   ------------------------------

   procedure Read_Remaining_Arguments (Self : in out Object; Tool : Which) is
      use GNATCOLL;
      use GPR2;

      Got_Prj : Boolean := False;

      function Get_Next_Argument return String;
      --  Returns the next non-empty and non-switch argument from the command
      --  line.
      --  Due to a limitation of GNAT.Command_Line functionality there is no
      --  way to distinguish between the end of arguments and an empty
      --  argument; we work around this by skipping over four empty arguments
      --  before giving up and returning an empty argument
      --  (we assume that this means we are really at the end).
      --  ??? We should remove this code and use GMAT.Command_Line.Get_Argument
      --  with Boolean out parameter instead once this routine is working
      --  correctly in the oldest compiler GPR2 should be compatible with.

      -----------------------
      -- Get_Next_Argument --
      -----------------------

      function Get_Next_Argument return String is
      begin
         for J in 1 .. 4 loop
            declare
               Next : constant String := Get_Argument;
            begin
               if Next /= "" then
                  return Next;
               end if;
            end;
         end loop;

         return "";
      end Get_Next_Argument;

   begin
      Read_Arguments : loop
         declare
            Arg : constant String := Get_Next_Argument;
         begin
            exit Read_Arguments when Arg = "";

            if Utils.Ends_With (GPR2.Path_Name.To_OS_Case (Arg), ".gpr") then
               if not Self.Project_File.Is_Defined then
                  Value_Callback ("-P", Arg);
                  Got_Prj := True;

               elsif not Got_Prj then
                  raise Usage_Error with
                    "cannot have -P<proj> and <proj> on the same command line";

               else
                  raise Usage_Error with
                    "cannot have multiple <proj> on the same command line";
               end if;

            else
               Self.Args.Include (Arg);
            end if;
         end;
      end loop Read_Arguments;

      if Self.Project_File.Is_Defined
        and then not Self.Project_File.Has_Dir_Name
        and then Self.Root_Path.Is_Defined
      then
         --  We have to resolve the project directory without target specific
         --  directories in search path because --root-dir exists in command
         --  line parameters.

         Self.Project_File := GPR2.Project.Create
           (Self.Project_File.Name, Self.Tree.Project_Search_Paths);
      end if;

      Self.Project_Is_Defined := Self.Project_File.Is_Defined;

      if Tool /= Install then
         if not Self.Project_File.Is_Defined then
            if Self.No_Project then
               Self.Project_File := Path_Name.Implicit_Project;
               Self.Project_Base :=
                 Path_Name.Create_Directory
                   (Filename_Type (Ada.Directories.Current_Directory));
            else
               Util.Check_For_Default_Project (Self);
            end if;

            if not Self.Project_File.Is_Defined then
               Display_Help (Self.Config);
               raise GPRtools.Usage_Error with
                 "Can't determine project file to work with";
            end if;

         elsif Self.No_Project then
            raise GPRtools.Usage_Error with
              "cannot specify --no-project with a project file";
         end if;

         if Self.Project_Base.Is_Defined then
            Self.Clean_Build_Path (Self.Project_Base);

         elsif Self.Project_File.Has_Dir_Name then
            Self.Clean_Build_Path (Self.Project_File);
         end if;
      end if;
   end Read_Remaining_Arguments;

   -----------
   -- Setup --
   -----------

   procedure Setup
     (Self : aliased in out Object'Class;
      Tool : Which) is
   begin
      PRP.Check_Attributes (PRP.Naming);
      Self.Tool := Tool;

      case Tool is
         when Build   =>
            PRP.Check_Attributes (PRP.Builder);
            PRP.Check_Attributes (PRP.Binder);
            PRP.Check_Attributes (PRP.Linker);
            PRP.Check_Attributes (PRP.Compiler);

         when Clean  =>
            PRP.Check_Attributes (PRP.Clean);

         when Install =>
            PRP.Check_Attributes (PRP.Install);

         when Remote =>
            PRP.Check_Attributes (PRP.Remote);

         when Ls | Name => null;
      end case;

      TLS.Set_Value (Self'Unchecked_Access);

      Define_Switch
        (Self.Config, Self.Help'Access,
         "-h", Long_Switch => "--help",
         Help              => "Display this help message and exit");

      Define_Switch
        (Self.Config, Self.Version'Access,
         Long_Switch => "--version",
         Help        => "Display version and exit");

      Define_Switch
        (Self.Config, Value_Callback'Unrestricted_Access,
         "-v", "--verbose",
         Help => "Verbose output");

      Define_Switch
        (Self.Config, Self.Unchecked_Shared_Lib'Access,
         Long_Switch => "--unchecked-shared-lib-imports",
         Help => "Shared lib projects may import any project");

      if Tool not in Remote | Ls then
         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            "-q", "--quiet",
            Help => "Be quiet/terse");

         --  The code below should be uncommented when we will be able to give
         --  a name for the hiding warnings switch.
         --
         --  Define_Switch
         --    (Self.Config, Self.Warnings'Unrestricted_Access,
         --     Switch => "-ws",
         --     Help   => "Suppress all warnings",
         --     Value  => False);

         Define_Switch
           (Self.Config, Self.Full_Path_Name_For_Brief'Access,
            Switch => "-F",
            Help   => "Full project path name in brief log messages");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--root-dir:",
            Help        => "Root directory of obj/lib/exec to relocate",
            Argument    => "<dir>");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--relocate-build-tree?",
            Help        =>
              "Root obj/lib/exec dirs are current-directory or dir",
            Argument    => "<dir>");

         Define_Switch
           (Self.Config, Self.Debug_Mode'Access,
            Long_Switch => "--debug",
            Help        => "Debug mode");
      end if;

      if Tool in Build | Clean | Install | Ls then
         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access, "-P:",
            Help => "Project file to "
            & (case Tool is
                 when Build   => "build",
                 when Install => "install",
                 when Clean   => "cleanup",
                 when Ls      => "browse",
                 when others => ""));

         if Tool /= Ls then
            Define_Switch
              (Self.Config, Self.No_Project'Access,
               Long_Switch => "--no-project",
               Help        => "Do not "
                              & (if Tool = Install
                                 then "install"
                                 else "use") & " project file");
         end if;

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            "-aP:",
            Help     => "Add directory <dir> to project search path",
            Argument => "<dir>");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--target=",
            Help        => "Specify a target for cross platforms",
            Argument    => "<name>");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--RTS:",
            Help        => "Use runtime <runtime> for language Ada",
            Argument    => "<runtime>");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            "-X:",
            Help     => "Specify an external reference for Project Files",
            Argument => "<NAME>=<VALUE>");

         Define_Switch
           (Self.Config, Self.Skip_Default_KB'Access,
            Long_Switch => "--db-",
            Help        => "Do not load the standard knowledge base");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--db:",
            Help        => "Parse dir as an additional knowledge base");
      end if;

      if Tool in Build | Clean then
         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--implicit-with:",
            Help        =>
              "Add the given projects as a dependency on all loaded"
            & " projects",
            Argument    => "<filename>");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--distributed?",
            Help        =>
              "Activate the distributed mode for the given slaves",
            Argument    => "<host>[,<host>]");

         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--slave-env?",
            Help        =>
              "Use a specific slave's environment",
            Argument    => "<name>");
      end if;

      if Tool in Build | Clean | Install then
         Define_Switch
           (Self.Config, Value_Callback'Unrestricted_Access,
            Long_Switch => "--src-subdirs:",
            Help        => "Prepend <obj>/dir to the list of source"
                           & " dirs for each project",
            Argument    => "<dir>");
      end if;
   end Setup;

   --------------------
   -- Value_Callback --
   --------------------

   procedure Value_Callback (Switch, Value : String) is

      Self : constant not null access Object'Class := TLS.Reference.all;

      function Normalize_Value (Default : String := "") return String is
        (if Value in "" | "=" | ":" then Default
         elsif Value (Value'First) in '=' | ':'
         then Value (Value'First + 1 .. Value'Last)
         else Value);
      --  Remove leading '=' symbol from value for options like
      --  --config=file.cgrp

   begin
      if Switch = "-P" then
         if not Self.Project_File.Is_Defined then
            Self.Project_File :=
              GPR2.Path_Name.Create_File
                (GPR2.Project.Ensure_Extension
                   (GPR2.Filename_Type (Normalize_Value)),
                 GPR2.Path_Name.No_Resolution);
         else
            raise GPRtools.Usage_Error with
              '"' & Normalize_Value & """, project already """
              & (if Self.Project_File.Has_Dir_Name
                 then Self.Project_File.Value
                 else String (Self.Project_File.Name)) & '"';
         end if;

      elsif Switch = "--relocate-build-tree" then
         Self.Build_Path :=
           GPR2.Path_Name.Create_Directory
             (GPR2.Filename_Type (Normalize_Value (".")));

      elsif Switch = "--root-dir" then
         Self.Root_Path :=
           GPR2.Path_Name.Create_Directory
             (GPR2.Filename_Type (Normalize_Value));

      elsif Switch = "-q" or else Switch = "--quiet" then
         if Self.Verbosity = Regular then
            Self.Verbosity := Quiet;
         end if;

      elsif Switch = "-v" or else Switch = "--verbose" then
         Self.Verbosity := Verbose;

      elsif Switch = "--implicit-with" then
         Self.Implicit_With.Append
           (GPR2.Path_Name.Create_File
              (GPR2.Project.Ensure_Extension
                   (GPR2.Filename_Type (Normalize_Value))));

      elsif Switch = "--target" then
         Self.Target := To_Unbounded_String (Normalize_Value);

      elsif Switch = "--RTS" then
         declare
            Value : constant String  := Normalize_Value;
            Del   : constant Natural := Ada.Strings.Fixed.Index (Value, "=");
         begin
            if Del = 0 then
               Self.RTS_Map.Insert (GPR2.Ada_Language, Value);
            else
               Self.RTS_Map.Insert
                 (GPR2."+" (GPR2.Name_Type (Value (Value'First .. Del - 1))),
                  Value (Del + 1 .. Value'Last));
            end if;
         end;

      elsif Switch = "-X" then
         declare
            Idx : constant Natural := Ada.Strings.Fixed.Index (Value, "=");
         begin
            if Idx = 0 then
               raise GPRtools.Usage_Error with
                 "Can't split '" & Value & "' to name and value";
            end if;

            Self.Context.Include
              (GPR2.Name_Type (Value (Value'First .. Idx - 1)),
               Value (Idx + 1 .. Value'Last));
         end;

      elsif Switch = "-aP" then
         Self.Tree.Register_Project_Search_Path
           (GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Value)));

      elsif Switch = "--distributed" then
         declare
            use type GPR2.Containers.Count_Type;

            --  If Value is set, the first character is a =, we remove it

            Hosts : constant GPR2.Containers.Name_List :=
                      (if Value = ""
                       then GPR2.Compilation.Registry.Get_Hosts
                       else GPR2.Containers.Create
                              (GPR2.Name_Type (Normalize_Value),
                               Separator => ","));
         begin
            if Hosts.Length = 0 then
               Util.Fail_Program
                 ("missing hosts for distributed mode compilation");

            else
               GPR2.Compilation.Registry.Record_Slaves (Hosts);
               Self.Distributed_Mode := True;
            end if;
         end;

      elsif Switch = "--slave-env" then
         if Value = "" then
            Self.Slave_Env_Auto := True;
         else
            Self.Slave_Env := To_Unbounded_String (Normalize_Value);
         end if;

      elsif Switch = "--src-subdirs" then
         Self.Src_Subdirs := To_Unbounded_String (Normalize_Value);

      elsif Switch = "--db" then
         declare
            KB_Norm : constant String :=
                        GNAT.OS_Lib.Normalize_Pathname (Normalize_Value);
            KB_Path : GPR2.Path_Name.Object;
         begin
            if GNAT.OS_Lib.Is_Directory (KB_Norm) then
               KB_Path :=
                 GPR2.Path_Name.Create_Directory
                   (GPR2.Filename_Type (KB_Norm));
            elsif GNAT.OS_Lib.Is_Regular_File (KB_Norm) then
               KB_Path :=
                 GPR2.Path_Name.Create_File (GPR2.Filename_Type (KB_Norm));
            else
               raise GPRtools.Usage_Error with
                 KB_Norm & " is not a file or directory";
            end if;

            Self.KB_Locations.Append (KB_Path);
         end;
      end if;
   end Value_Callback;

end GPRtools.Options;
