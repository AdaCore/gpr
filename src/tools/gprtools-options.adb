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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with GPR2.Compilation.Registry;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Configuration;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

with GNATCOLL.Utils;

with GPRtools.Util;

package body GPRtools.Options is

   package PRP renames GPR2.Project.Registry.Pack;

   procedure Get_Opt_Internal
     (Parser : Command_Line_Parser;
      Result : in out Base_Options'Class);

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   ------------
   -- Create --
   ------------

   function Create
     (Initial_Year           : String;
      Cmd_Line               : String := "";
      Tool_Name              : String := "";
      Help                   : String := "";
      Allow_No_Project       : Boolean := True;
      Allow_Autoconf         : Boolean := False;
      Allow_Distributed      : Boolean := False;
      Allow_Quiet            : Boolean := True;
      No_Project_Support     : Boolean := False;
      Allow_Implicit_Project : Boolean := True) return Command_Line_Parser
   is
      use GPRtools.Command_Line;
      Parser            : Command_Line_Parser;
      Project_Group     : GPRtools.Command_Line.Argument_Group;
      Config_Group      : GPRtools.Command_Line.Argument_Group;
      Verbosity_Group   : GPRtools.Command_Line.Argument_Group;
      Distributed_Group : GPRtools.Command_Line.Argument_Group;
      Hidden_Group      : GPRtools.Command_Line.Argument_Group;

   begin
      Parser := Command_Line_Parser'
        (GPRtools.Command_Line.Command_Line_Parser
           (GPRtools.Command_Line.Create
                (Initial_Year,
                 Cmd_Line  => Cmd_Line,
                 Tool_Name => Tool_Name,
                 Help      => Help))
         with Find_Implicit_Project => Allow_Implicit_Project);

      if not No_Project_Support then
         Project_Group :=
           Parser.Add_Argument_Group
             ("Project",
              On_Switch'Access,
              "Project file handling switches",
              Last => True);

         Parser.Add_Argument
           (Project_Group,
            Create (Name           =>  "-P",
                    Help           =>  "Use project file ""proj.gpr""",
                    In_Switch_Attr => False,
                    Delimiter      =>  Optional_Space,
                    Parameter      =>  "proj.gpr"));
         Parser.Add_Argument
           (Project_Group,
            Create (Name           =>  "-aP",
                    Help           =>  "Add directory ""dir"" to project" &
                                       " search path",
                    In_Switch_Attr => False,
                    Delimiter      =>  Optional_Space,
                    Parameter      =>  "dir"));
         Parser.Add_Argument
           (Project_Group,
            Create (Name           =>  "-X",
                    Help           =>  "Set the project external reference" &
                                       " ""NAME"" to ""Value""",
                    In_Switch_Attr => False,
                    Delimiter      => Optional_Space,
                    Parameter      => "NAME=Value"));
         --  -eL not used ???
         Parser.Add_Argument
           (Project_Group,
            Create (Name           => "-eL",
                    Help           => "Follow sybolic links when processing" &
                                      " project files",
                    In_Switch_Attr => False,
                    Hidden         => True));

         if Allow_No_Project then
            Parser.Add_Argument
              (Project_Group,
               Create (Name           => "--no-project",
                       Help           => "Do not use project file",
                       In_Switch_Attr => False));
         end if;

         Parser.Add_Argument
           (Project_Group,
            Create (Name           => "--implicit-with",
                    Help           => "Add the given  project as a " &
                                      "dependency of all loaded projects",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "proj.gpr"));
         Parser.Add_Argument
           (Project_Group,
            Create (Name           => "--unchecked-shared-lib-imports",
                    Help           => "Shared lib projects may import any" &
                                      " project",
                    In_Switch_Attr => False));
         Parser.Add_Argument
           (Project_Group,
            Create (Name           => "--relocate-build-tree",
                    Help           => "Root obj/lib/exec dirs are current" &
                                      " directory or ""dir""",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "dir",
                    Default        =>
                      GNAT.Directory_Operations.Get_Current_Dir));
         Parser.Add_Argument
           (Project_Group,
            Create (Name           => "--root-dir",
                    Help           => "Root directory of obj/lib/exec to" &
                                      " relocate",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "dir"));
         Parser.Add_Argument
           (Project_Group,
            Create (Name           => "--src-subdirs",
                    Help           => "Prepend <obj>/dir to the list of" &
                                      " source dirs for each project",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "dir"));
         Parser.Add_Argument
           (Project_Group,
            Create (Name           => "--subdirs",
                    Help           => "Use ""dir"" as suffix to obj/lib/exec" &
                                      " directories",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "dir"));

         --  CONFIG/AUTOCONF

         Config_Group :=
           Parser.Add_Argument_Group
             ("Project configuration",
              On_Switch'Access,
              "Switches used to set or modify the way the " &
                "project configuration is done",
              Last => True);

         Parser.Add_Argument
           (Config_Group,
            Create (Name           => "--config",
                    Help           => "Specify the main config project file" &
                                      " name",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "file.cgpr"));

         if Allow_Autoconf then
            Parser.Add_Argument
              (Config_Group,
               Create (Name           => "--autoconf",
                       Help           => "Specify/create the main config" &
                                         " project file name",
                       In_Switch_Attr => False,
                       Delimiter      => Equal,
                       Parameter      => "file.cgpr"));
         end if;

         Parser.Add_Argument
           (Config_Group,
            Create (Name           => "--target",
                    Help           => "Specify a target for cross platforms",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "targetname"));
         Parser.Add_Argument
           (Config_Group,
            Create (Name           =>  "--RTS",
                    Help           =>  "Specify a runtime for <lang> or Ada" &
                                       " (default)",
                    In_Switch_Attr => False,
                    Delimiter      =>  Equal,
                    Parameter      =>  "runtime",
                    Index          => "<lang>"));
         Parser.Add_Argument
           (Config_Group,
            Create (Name           => "--db",
                    Help           => "Parse ""dir"" as an additional" &
                                      " knowledge base",
                    In_Switch_Attr => False,
                    Delimiter      => Space,
                    Parameter      => "dir"));
         Parser.Add_Argument
           (Config_Group,
            Create (Name           => "--db-",
                    Help           => "Do not load the standard knowledge" &
                                      " base",
                    In_Switch_Attr => False));
      end if;

      if Allow_Distributed then
         Distributed_Group :=
           Parser.Add_Argument_Group
             ("Distributed build", On_Switch'Access,
              Help => "Distributed compilation mode switches.",
              Last => True);

         Parser.Add_Argument
           (Distributed_Group,
            Create (Name           => "--distributed",
                    Help           => "Activate the remote mode on specified" &
                                      " node(s), or automatically.",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "node1[,node2]",
                    Default        => "@auto@"));
         Parser.Add_Argument
           (Distributed_Group,
            Create (Name           => "--slave-env",
                    Help           => "Use a specific slave's environment",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "node",
                    Default        => "@auto@"));
         Parser.Add_Argument
           (Distributed_Group,
            Create (Name           => "--hash",
                    Help           => "Set a hash string to identified" &
                                      " environment",
                    In_Switch_Attr => False,
                    Delimiter      => Equal,
                    Parameter      => "<string>"));
      end if;

      --  Verbosity

      Verbosity_Group :=
        Parser.Add_Argument_Group
          ("Verbosity", On_Switch'Access, Last => True);

      Parser.Add_Argument
        (Verbosity_Group,
         Create (Name           => "-F",
                 Help           => "Full project path name in brief error" &
                                   " messages",
                 In_Switch_Attr => False));

      if Allow_Quiet then
         Parser.Add_Argument
           (Verbosity_Group,
            Create (Name   => "-q",
                    Help   => "Be quiet/terse"));
      end if;

      Parser.Add_Argument
        (Verbosity_Group,
         Create (Name   => "-v",
                 Help   => "Verbose output"));

      --  Internal switch

      Hidden_Group :=
        Parser.Add_Argument_Group
          ("_internal_gprtools_switches",
           On_Switch'Access,
           Last => True);
      Parser.Add_Argument
        (Hidden_Group,
         Create (Name      => "--debug",
                 Help      => "",
                 Delimiter => None,
                 Parameter => "flags",
                 Default   => "*",
                 Hidden    => True));

      return Parser;
   end Create;

   -------------
   -- Get_Opt --
   -------------

   overriding procedure Get_Opt
     (Parser : Command_Line_Parser;
      Result : in out GPRtools.Command_Line.Command_Line_Result'Class)
   is
   begin
      Get_Opt_Internal (Parser, Base_Options'Class (Result));
   end Get_Opt;

   ----------------------
   -- Get_Opt_Internal --
   ----------------------

   procedure Get_Opt_Internal
     (Parser : Command_Line_Parser;
      Result : in out Base_Options'Class)
   is
      use GPR2;
      Got_Prj : Boolean := False;
   begin
      GPRtools.Command_Line.Command_Line_Parser (Parser).Get_Opt (Result);

      for Arg of Result.Remaining_Arguments loop
         if GNATCOLL.Utils.Ends_With
           (GPR2.Path_Name.To_OS_Case (Arg),
            String (GPR2.Project.Project_File_Extension))
         then
            if not Result.Project_File.Is_Defined then
               On_Switch
                 (Parser, Result'Access,
                  Arg   => "-P",
                  Index => "",
                  Param => Arg);
               Got_Prj := True;

            elsif not Got_Prj then
               raise GPRtools.Usage_Error with
                 "cannot have -P<prj> and <prj> on the same command line";

            else
               raise GPRtools.Usage_Error with
                 "cannot have multiple <proj> on the same command line";
            end if;

         else
            Result.Args.Include (Arg);
         end if;
      end loop;

      if Result.Project_File.Is_Defined
        and then not Result.Project_File.Has_Dir_Name
        and then Result.Root_Path.Is_Defined
      then
         --  We have to resolve the project directory without target specific
         --  directories in search path because --root-dir exists in command
         --  line parameters.

         Result.Project_File := GPR2.Project.Create
           (Result.Project_File.Name, Result.Tree.Project_Search_Paths);
      end if;

      Result.Project_Is_Defined := Result.Project_File.Is_Defined;

      if not Result.Project_File.Is_Defined then
         if Result.No_Project then
            Result.Project_Base := GPR2.Path_Name.Create_Directory
              (GPR2.Filename_Type (Ada.Directories.Current_Directory));

         elsif Parser.Find_Implicit_Project then
            Result.Project_File := GPRtools.Util.Check_For_Default_Project;

            if not Result.Project_File.Is_Defined then
               Result.Project_Base :=
                 GPR2.Path_Name.Create_Directory
                   (GPR2.Filename_Type (Ada.Directories.Current_Directory));

               if not Result.Quiet then
                  Ada.Text_IO.Put_Line
                    ("use implicit project in " & Result.Project_Base.Value);
               end if;

            elsif not Result.Quiet then
               Ada.Text_IO.Put_Line
                 ("using project file " & Result.Project_File.Value);
            end if;
         end if;

      elsif Result.No_Project then
         raise GPRtools.Usage_Error with
           "cannot specify --no-project with a project file";
      end if;

      if not Result.Build_Path.Is_Defined
        and then Result.Root_Path.Is_Defined
      then
         raise GPRtools.Usage_Error with
           "cannot use --root-dir without --relocate-build-tree option";
      end if;

      declare
         Project_Dir : constant GPR2.Path_Name.Object :=
                         (if Result.Project_Base.Is_Defined
                          then Result.Project_Base
                          elsif Result.Project_File.Is_Defined
                            and then Result.Project_File.Has_Dir_Name
                          then GPR2.Path_Name.Create_Directory
                            (Filename_Type (Result.Project_File.Dir_Name))
                          else
                             GPR2.Path_Name.Undefined);
      begin
         if Project_Dir.Is_Defined then
            if not Result.Build_Path.Is_Defined then
               Result.Build_Path := Project_Dir;
            elsif Result.Root_Path.Is_Defined then
               Result.Build_Path := GPR2.Path_Name.Create_Directory
                 (Project_Dir.Relative_Path (Result.Root_Path).Name,
                  Filename_Type (Result.Build_Path.Value));
            end if;
         end if;
      end;
   end Get_Opt_Internal;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Opt                : in out Base_Options'Class;
      Absent_Dir_Error   : Boolean;
      Handle_Information : Boolean := False;
      Handle_Errors      : Boolean := True;
      Handle_Lint        : Boolean := False)
      return Boolean
   is

      procedure Display (Logs : GPR2.Log.Object);
      --  Display errors and/or warnings messages in Logs. Warnings are only
      --  displayed if tool not run in quiet mode and Handle_Warnings is set
      --  to True.

      -------------
      -- Display --
      -------------

      procedure Display (Logs : GPR2.Log.Object) is
      begin
         if Logs.Has_Error then
            --  If there are errors, just display them: any warning may just
            --  be a consequence of the initial error and thus be false
            --  negatives.

            for C in Logs.Iterate
              (Information => False,
               Warning     => False,
               Error       => True,
               Lint        => False,
               Read        => False,
               Unread      => True)
            loop
               GPR2.Log.Element (C).Output
                 (Full_Path_Name => Opt.Full_Path_Name_For_Brief);
            end loop;

         elsif not Opt.Quiet then
            for C in Logs.Iterate
              (Information => Handle_Information,
               Warning     => True,
               Error       => False,
               Lint        => Handle_Lint,
               Read        => False,
               Unread      => True)
            loop
               GPR2.Log.Element (C).Output
                 (Full_Path_Name => Opt.Full_Path_Name_For_Brief);
            end loop;
         end if;
      end Display;

      Conf        : GPR2.Project.Configuration.Object;
      Create_Cgpr : Boolean := False;

   begin
      if Opt.Config_Project.Is_Defined
           and then
         (not Opt.Create_Missing_Config or else Opt.Config_Project.Exists)
      then
         Conf := GPR2.Project.Configuration.Load (Opt.Config_Project);

         Display (Conf.Log_Messages);

         if Conf.Has_Error then
            if Handle_Errors then
               GPRtools.Util.Finish_Program
                 (GPRtools.Util.E_Fatal,
                    '"'
                  & String (Opt.Config_Project.Simple_Name)
                  & """ processing failed");
            end if;

            return False;
         end if;

         Opt.Tree.Load
           (Filename         =>  (if Opt.Project_File.Is_Defined
                                  then Opt.Project_File
                                  else Opt.Project_Base),
            Context          =>  Opt.Context,
            Config           =>  Conf,
            Build_Path       =>  Opt.Build_Path,
            Subdirs          =>  Opt.Get_Subdirs,
            Src_Subdirs      =>  Opt.Get_Src_Subdirs,
            Check_Shared_Lib =>  not Opt.Unchecked_Shared_Lib,
            Absent_Dir_Error =>  Absent_Dir_Error,
            Implicit_With    =>  Opt.Implicit_With);

         if To_String (Opt.Target) /= "all" then
            --  if target is defined on the command line, and a config
            --  file is specified, issue an error if the target of the config
            --  is different from the command line.

            declare
               use GPR2;
               package PRA renames GPR2.Project.Registry.Attribute;

               Target_Attr : constant GPR2.Project.Attribute.Object :=
                               Opt.Tree.Configuration.Corresponding_View.
                                 Attribute (PRA.Target);
               Conf_Target : constant Value_Type := Target_Attr.Value.Text;
               Base        : constant GPR2.KB.Object :=
                               (if Opt.Tree.Get_KB.Is_Defined
                                then Opt.Tree.Get_KB
                                else GPR2.KB.Create_Default
                                  (GPR2.KB.Targetset_Only_Flags));
               Conf_Norm   : constant Name_Type :=
                               Base.Normalized_Target
                                 (Name_Type (Conf_Target));
               Opt_Norm    : constant Name_Type :=
                               Base.Normalized_Target
                                 (Name_Type (To_String (Opt.Target)));
            begin
               if Conf_Norm /= Opt_Norm then
                  Opt.Tree.Log_Messages.Append
                    (GPR2.Message.Create
                       (Level   =>  GPR2.Message.Error,
                        Message =>  "--target: '" & To_String (Opt.Target) &
                          "' is different from the target value in the" &
                          " configuration project '" &
                          String (Conf_Norm) & "'",
                        Sloc    => Target_Attr.Value));
               else
                  Opt.Tree.Log_Messages.Append
                    (GPR2.Message.Create
                       (Level   =>  GPR2.Message.Warning,
                        Message =>  "--target is not used when a " &
                          "configuration project is specified.",
                        Sloc    => Target_Attr.Value));
               end if;
            end;
         end if;

      else
         if Opt.Create_Missing_Config
           and then not Opt.Config_Project.Exists
         then
            if not Opt.Quiet then
               Ada.Text_IO.Put_Line
                 ("creating configuration project " &
                    String (Opt.Config_Project.Name));
            end if;

            Create_Cgpr := True;
         end if;

         Opt.Tree.Load_Autoconf
           (Filename          =>  (if Opt.Project_File.Is_Defined
                                   then Opt.Project_File
                                   else Opt.Project_Base),
            Context           =>  Opt.Context,
            Build_Path        =>  Opt.Build_Path,
            Subdirs           =>  Opt.Get_Subdirs,
            Src_Subdirs       =>  Opt.Get_Src_Subdirs,
            Check_Shared_Lib  =>  not Opt.Unchecked_Shared_Lib,
            Absent_Dir_Error  =>  Absent_Dir_Error,
            Implicit_With     =>  Opt.Implicit_With,
            Target            =>  Opt.Get_Target,
            Language_Runtimes =>  Opt.RTS_Map,
            Base              =>  GPR2.KB.Create
                                    (Flags      => GPR2.KB.Default_Flags,
                                     Default_KB => not Opt.Skip_Default_KB,
                                     Custom_KB  => Opt.KB_Locations),
            Config_Project    => (if Create_Cgpr
                                  then Opt.Config_Project
                                  else GPR2.Path_Name.Undefined));
      end if;

      if Handle_Errors then
         Display (Opt.Tree.Log_Messages.all);
      end if;

      if Opt.Tree.Log_Messages.Has_Error then
         if Handle_Errors then
            GPRtools.Util.Finish_Program
              (GPRtools.Util.E_Fatal,
                 '"'
               & (if Opt.Project_File.Is_Defined
                 then String (Opt.Config_Project.Simple_Name)
                 else Opt.Project_Base.Value)
               & """ processing failed");
         end if;
      end if;

      return True;

   exception
      when GPR2.Project_Error =>
         if not Handle_Errors then
            return False;
         end if;

         if Opt.Tree.Has_Messages then
            Display (Opt.Tree.Log_Messages.all);
         end if;

         return False;
   end Load_Project;

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
      pragma Unreferenced (Parser);
      use type GPR2.Language_Id;
      use type GPRtools.Command_Line.Switch_Type;

      Result   : constant access Base_Options := Base_Options (Res.all)'Access;
      Lang_Idx : constant GPR2.Language_Id :=
                   (if Index'Length > 0
                    then GPR2."+" (GPR2.Name_Type (Index))
                    else GPR2.No_Language);

   begin
      if Arg = "-P" then
         if not Result.Project_File.Is_Defined then
            Result.Project_File :=
              GPR2.Path_Name.Create_File
                (GPR2.Project.Ensure_Extension (GPR2.Filename_Type (Param)),
                 GPR2.Path_Name.No_Resolution);
         else
            raise GPRtools.Usage_Error with
              '"' & String (Arg) & """, project already """
              & (if Result.Project_File.Has_Dir_Name
                 then Result.Project_File.Value
                 else String (Result.Project_File.Name)) & '"';
         end if;

      elsif Arg = "-aP" then
         Result.Tree.Register_Project_Search_Path
           (GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param)));

      elsif Arg = "-X" then
         declare
            Idx : constant Natural := Ada.Strings.Fixed.Index (Param, "=");
         begin
            if Idx = 0 then
               raise GPRtools.Usage_Error with
                 "Can't split '" & Param & "' to name and value";
            end if;

            Result.Context.Include
              (GPR2.Name_Type (Param (Param'First .. Idx - 1)),
               Param (Idx + 1 .. Param'Last));
         end;

      elsif Arg = "-eL" then
         --  ??? TODO
         null;

      elsif Arg = "--no-project" then
         Result.No_Project := True;

      elsif Arg = "--implicit-with" then
         Result.Implicit_With.Append
           (GPR2.Path_Name.Create_File
              (GPR2.Project.Ensure_Extension (GPR2.Filename_Type (Param))));

      elsif Arg = "--unchecked-shared-lib-imports" then
         Result.Unchecked_Shared_Lib := True;

      elsif Arg = "--relocate-build-tree" then
         Result.Build_Path :=
           GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param));

      elsif Arg = "--root-dir" then
         Result.Root_Path :=
           GPR2.Path_Name.Create_Directory (GPR2.Filename_Type (Param));

      elsif Arg = "--src-subdirs" then
         Result.Src_Subdirs := To_Unbounded_String (Param);

      elsif Arg = "--subdirs" then
         Result.Subdirs := To_Unbounded_String (Param);

      elsif Arg = "--config" then
         Result.Config_Project :=
           GPR2.Path_Name.Create_File
             (GPR2.Filename_Type (Param));
         Result.Create_Missing_Config := False;

      elsif Arg = "--autoconf" then
         Result.Config_Project :=
           GPR2.Path_Name.Create_File
             (GPR2.Filename_Type (Param));
         Result.Create_Missing_Config := True;

      elsif Arg = "--target" then
         Result.Target := To_Unbounded_String (Param);

      elsif Arg = "--RTS" then
         if Lang_Idx = GPR2.No_Language then
            Result.RTS_Map.Include (GPR2.Ada_Language, Param);
         else
            Result.RTS_Map.Include (Lang_Idx, Param);
         end if;

      elsif Arg = "--db" then
         declare
            KB_Norm : constant String :=
                        GNAT.OS_Lib.Normalize_Pathname (Param);
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

            Result.KB_Locations.Append (KB_Path);
         end;

      elsif Arg = "--db-" then
         Result.Skip_Default_KB := True;

      elsif Arg = "--distributed" then
         declare
            use type GPR2.Containers.Count_Type;

            --  If Value is set, the first character is a =, we remove it

            Hosts : constant GPR2.Containers.Name_List :=
                      (if Param = "@auto@"
                       then GPR2.Compilation.Registry.Get_Hosts
                       else GPR2.Containers.Create
                              (GPR2.Name_Type (Param),
                               Separator => ","));
         begin
            if Hosts.Length = 0 then
               raise Usage_Error with
                 "missing hosts for distributed mode compilation";
            else
               GPR2.Compilation.Registry.Record_Slaves (Hosts);
               Result.Distributed_Mode := True;
            end if;
         end;

      elsif Arg = "--slave-env" then
         if Param = "@auto@" then
            Result.Slave_Env_Auto := True;
         else
            Result.Slave_Env := To_Unbounded_String (Param);
         end if;

      elsif Arg = "--hash" then
         Result.Hash_Value := To_Unbounded_String (Param);

      elsif Arg = "-F" then
         Result.Full_Path_Name_For_Brief := True;

      elsif Arg = "-q" then
         Result.Verbosity := Quiet;

      elsif Arg = "-v" then
         case Result.Verbosity is
            when Very_Verbose =>
               null;
            when Verbose =>
               Result.Verbosity := Very_Verbose;
            when others =>
               Result.Verbosity := Verbose;
         end case;

      elsif Arg = "--debug" then
         for C of Param loop
            GPR2.Set_Debug (C);
         end loop;

      else
         raise GPRtools.Command_Line.Command_Line_Definition_Error
           with "unexpected switch " & String (Arg);
      end if;
   end On_Switch;

   -----------
   -- Setup --
   -----------

   procedure Setup (Tool : Which) is
   begin
      PRP.Check_Attributes (PRP.Naming);

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

         when Ls | Name | Inspect =>
            null;
      end case;
      --  GPR tree handling

      if Tool = Name then
         --  GPRName doesn't need any project-related argument
         return;
      end if;
   end Setup;

end GPRtools.Options;
