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

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

with Ada.Containers;
with GNAT.Directory_Operations;

with GPR2.Message;
with GPR2.Project.Registry.Pack;

with GPRtools.Program_Termination;

package body GPRtools.Options is

   use GPRtools.Program_Termination;

   package PRP renames GPR2.Project.Registry.Pack;

   procedure On_Switch
     (Parser : GPRtools.Command_Line.Command_Line_Parser'Class;
      Res    : not null access GPRtools.Command_Line.Command_Line_Result'Class;
      Arg    : GPRtools.Command_Line.Switch_Type;
      Index  : String;
      Param  : String);

   ---------------------
   -- Append_Argument --
   ---------------------

   overriding procedure Append_Argument
     (Result : in out Base_Options; Value : GPR2.Value_Type) is
   begin
      if not Result.On_Extra_Arg (Value) then
         Result.Build_Options.Mains.Include (Value);
      end if;
   end Append_Argument;


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
      Allow_Quiet            : Boolean := True;
      No_Project_Support     : Boolean := False;
      Allow_Implicit_Project : Boolean := True;
      Check_Shared_Libs      : Boolean := False) return Command_Line_Parser
   is
      use GPRtools.Command_Line;
      Parser            : Command_Line_Parser;
      Project_Group     : GPRtools.Command_Line.Argument_Group;
      Config_Group      : GPRtools.Command_Line.Argument_Group;
      Verbosity_Group   : GPRtools.Command_Line.Argument_Group;
      Hidden_Group      : GPRtools.Command_Line.Argument_Group;

   begin
      Parser := Command_Line_Parser'
        (GPRtools.Command_Line.Command_Line_Parser
           (GPRtools.Command_Line.Create
                (Initial_Year,
                 Cmd_Line  => Cmd_Line,
                 Tool_Name => Tool_Name,
                 Help      => Help))
         with Find_Implicit_Project => Allow_Implicit_Project,
              Check_Shared_Libs     => Check_Shared_Libs);

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

         if Check_Shared_Libs then
            Parser.Add_Argument
              (Project_Group,
               Create (Name           => "--unchecked-shared-lib-imports",
                       Help           => "Shared lib projects may import any" &
                                         " project",
                       In_Switch_Attr => False));
         else
            Parser.Add_Argument
              (Project_Group,
               Create (Name           => "--unchecked-shared-lib-imports",
                       Help           => "for compatibility only, unused",
                       In_Switch_Attr => False,
                       Hidden         => True));
         end if;

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

      Parser.Add_Argument
        (Verbosity_Group,
         Create (Name   =>  "-we",
                 Help   => "Treat warnings as errors"));

      Parser.Add_Argument
        (Verbosity_Group,
         Create (Name   =>  "-ws",
                 Help   => "Suppress all warnings"));

      Parser.Add_Argument
        (Verbosity_Group,
         Create (Name   =>  "-wn",
                 Help   => "Treat warnings as warnings"));

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
      use type Ada.Containers.Count_Type;
      use GPR2;

      procedure Adjust_Console_Verbosity (Opt : in out Base_Options'Class);

      procedure Mains_Check (Opt : Base_Options'Class);
      --  Sanity check mains given on the command line:

      ------------------------------
      -- Adjust_Console_Verbosity --
      ------------------------------

      procedure Adjust_Console_Verbosity (Opt : in out Base_Options'Class) is
      begin
         if Opt.Warning_Mode = As_Errors then
            GPR2.Message.Treat_Warnings_As_Error (True);
         end if;

         case Opt.Verbosity is
            when Quiet =>
               --  The GPR tools in general don't comply with the Quiet mode:
               --  1- Warnings and errors are displayed
               --  2- tool messages display the important ones

               if Opt.Warning_Mode = No_Warnings then
                  Opt.Console_Reporter.Set_Verbosity
                    (GPR2.Reporter.No_Warnings);
               end if;

               Opt.Console_Reporter.Set_User_Verbosity
                 (GPR2.Reporter.Important_Only);

            when Regular =>
               if Opt.Warning_Mode = No_Warnings then
                  Opt.Console_Reporter.Set_Verbosity
                    (GPR2.Reporter.No_Warnings);
               end if;

            when Verbose =>
               Opt.Console_Reporter.Set_User_Verbosity
                 (GPR2.Reporter.Verbose);
         end case;
      end Adjust_Console_Verbosity;

      -----------------
      -- Mains_Check --
      -----------------

      procedure Mains_Check (Opt : Base_Options'Class) is
      begin
         if Opt.Build_Options.Unit_Index /= No_Index
           and then (Opt.Build_Options.Mains.Is_Empty
                     or else Opt.Build_Options.Mains.Length > 1)
         then
            raise GPR2.Options.Usage_Error with
              "only one source can be specified with multi-unit index " &
              "specified with '-eI'";
         end if;

         if Ada.Strings.Unbounded.Length (Opt.Build_Options.Output_File) > 0
           and then Opt.Build_Options.Mains.Length > 1
         then
            raise GPR2.Options.Usage_Error with
              "only one source can be specified when the output file is " &
              "specified with '-o'";
         end if;
      end Mains_Check;

   begin
      if Parser.Check_Shared_Libs then
         Base_Options (Result).Check_Shared_Libs := True;
         --  Enable by default in this case. Can be deactivated via
         --  --unchecked-shared-libs
      end if;

      GPRtools.Command_Line.Command_Line_Parser (Parser).Get_Opt (Result);

      --  Handle verbosity options
      Adjust_Console_Verbosity (Base_Options'Class (Result));

      Base_Options (Result).Find_Implicit_Project :=
        Parser.Find_Implicit_Project;

      Mains_Check (Base_Options'Class (Result));
   end Get_Opt;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Opt                      : in out Base_Options'Class;
      Absent_Dir_Error         : GPR2.Error_Level;
      Create_Missing_Dirs      : GPR2.Project.Tree.Missing_Dir_Behavior :=
                                   GPR2.Project.Tree.Do_Nothing;
      Handle_Errors            : Boolean := True;
      Restricted_To_Languages  : GPR2.Containers.Language_Set :=
                                   GPR2.Containers.Empty_Language_Set)
      return Boolean
   is

      Loaded : Boolean := False;

   begin
      if not Restricted_To_Languages.Is_Empty then
         Opt.Tree.Restrict_Autoconf_To_Languages (Restricted_To_Languages);
      end if;

      Loaded := Opt.Tree.Load
        (Opt,
         With_Runtime             => True,
         Reporter                 => Opt.Console_Reporter,
         Absent_Dir_Error         => Absent_Dir_Error,
         Create_Missing_Dirs      => Create_Missing_Dirs,
         Allow_Implicit_Project   => Opt.Find_Implicit_Project,
         Check_Shared_Libs_Import => Opt.Check_Shared_Libs);

      if Handle_Errors and then not Loaded then
         if Opt.Project_File.Is_Defined then
            Handle_Program_Termination
              (Force_Exit => True,
               Exit_Code  => E_Project,
               Message    => '"' & String (Opt.Project_File.Simple_Name)
               & """ processing failed");
         else
            Handle_Program_Termination
              (Force_Exit => True,
               Exit_Code  => E_Project,
               Message    => "processing failed");
         end if;
      end if;

      return Loaded;
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
      use type GPRtools.Command_Line.Switch_Type;

      Result   : constant access Base_Options := Base_Options (Res.all)'Access;

   begin
      if Arg = "-P" then
         Result.Add_Switch
           (Switch => GPR2.Options.P,
            Param  => Param,
            Index  => "");

      elsif Arg = "-aP" then
         Result.Add_Switch
           (Switch => GPR2.Options.AP,
            Param  => Param,
            Index  => "");

      elsif Arg = "-X" then
         Result.Add_Switch
           (Switch => GPR2.Options.X,
            Param  => Param,
            Index  => "");

      elsif Arg = "-eL" then
         Result.Add_Switch
           (Switch => GPR2.Options.Resolve_Links,
            Param  => Param,
            Index  => "");

      elsif Arg = "--no-project" then
         Result.Add_Switch
           (Switch => GPR2.Options.No_Project,
            Param  => Param,
            Index  => "");

      elsif Arg = "--implicit-with" then
         Result.Add_Switch
           (Switch => GPR2.Options.Implicit_With,
            Param  => Param,
            Index  => "");

      elsif Arg = "--unchecked-shared-lib-imports" then
         Result.Check_Shared_Libs := False;

      elsif Arg = "--relocate-build-tree" then
         Result.Add_Switch
           (Switch => GPR2.Options.Relocate_Build_Tree,
            Param  => Param,
            Index  => "");

      elsif Arg = "--root-dir" then
         Result.Add_Switch
           (Switch => GPR2.Options.Root_Dir,
            Param  => Param,
            Index  => "");

      elsif Arg = "--src-subdirs" then
         Result.Add_Switch
           (Switch => GPR2.Options.Src_Subdirs,
            Param  => Param,
            Index  => "");

      elsif Arg = "--subdirs" then
         Result.Add_Switch
           (Switch => GPR2.Options.Subdirs,
            Param  => Param,
            Index  => "");

      elsif Arg = "--config" then
         Result.Add_Switch
           (Switch => GPR2.Options.Config,
            Param  => Param,
            Index  => "");

      elsif Arg = "--autoconf" then
         Result.Add_Switch
           (Switch => GPR2.Options.Autoconf,
            Param  => Param,
            Index  => "");

      elsif Arg = "--target" then
         Result.Add_Switch
           (Switch => GPR2.Options.Target,
            Param  => Param,
            Index  => "");

      elsif Arg = "--RTS" then
         Result.Add_Switch
           (Switch => GPR2.Options.RTS,
            Param  => Param,
            Index  => Index);

      elsif Arg = "--db" then
         Result.Add_Switch
           (Switch => GPR2.Options.Db,
            Param  => Param,
            Index  => "");

      elsif Arg = "--db-" then
         Result.Add_Switch
           (Switch => GPR2.Options.Db_Minus,
            Param  => Param,
            Index  => "");

      elsif Arg = "-F" then
         Result.Console_Reporter.Set_Full_Pathname (True);

      elsif Arg = "-q" then
         Result.Verbosity := Quiet;

      elsif Arg = "-v" then
         Result.Verbosity := Verbose;

      elsif Arg = "-we" then
         Result.Warning_Mode := As_Errors;

      elsif Arg = "-ws" then
         Result.Warning_Mode := No_Warnings;

      elsif Arg = "-wn" then
         Result.Warning_Mode := Regular;

      elsif Arg = "--debug" then
         for C of Param loop
            GPR2.Set_Debug (C);
         end loop;

      else
         raise GPR2.Options.Usage_Error
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

         when Ls | Inspect =>
            null;
      end case;
   end Setup;

end GPRtools.Options;
