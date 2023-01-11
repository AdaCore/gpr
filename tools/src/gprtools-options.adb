------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

with GNAT.Directory_Operations;

with GPR2.Compilation.Registry;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Registry.Pack;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

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

   ---------------------
   -- Append_Argument --
   ---------------------

   overriding procedure Append_Argument
     (Result : in out Base_Options; Value : GPR2.Value_Type) is
   begin
      Result.Remaining.Append (Value);
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

      Parser.Add_Argument
        (Verbosity_Group,
         Create (Name   =>  "-ws",
                 Help   => "Suppress all warnings"));

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
   begin
      GPRtools.Command_Line.Command_Line_Parser (Parser).Get_Opt (Result);

      for Arg of Result.Remaining_Arguments loop
         if not Result.On_Extra_Arg (Arg) then
            Result.Args.Include (Arg);
         end if;
      end loop;

      Result.Finalize
        (Allow_Implicit_Project => Parser.Find_Implicit_Project,
         Quiet                  => Result.Quiet);

   end Get_Opt_Internal;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (Opt                : in out Base_Options'Class;
      Absent_Dir_Error   : GPR2.Project.Tree.Error_Level;
      Handle_Information : Boolean := False;
      Handle_Errors      : Boolean := True;
      Handle_Lint        : Boolean := False) return Boolean
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
               Warning     => Opt.Warnings,
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

      Loaded : Boolean := False;
   begin

      Loaded := Opt.Load_Project
        (Tree             => Opt.Tree.all,
         Absent_Dir_Error => Absent_Dir_Error);

      if Handle_Errors then
         Display (Opt.Config_Project_Log);
         if Opt.Tree /= null and then Opt.Tree.Has_Messages then
            Display (Opt.Tree.all.Log_Messages.all);
         end if;
         if Opt.Config_Project_Has_Error then
            GPRtools.Util.Finish_Program
              (GPRtools.Util.E_Fatal,
            '"'
               & String (Opt.Config_Project.Simple_Name)
               & """ processing failed");
         end if;
         if Opt.Tree /= null
           and then Opt.Tree.Has_Messages
           and then Opt.Tree.Log_Messages.Has_Error
         then
            GPRtools.Util.Finish_Program
              (GPRtools.Util.E_Fatal,
            '"'
               & String (Opt.Filename.Simple_Name)
               & """ processing failed");
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
         --  ??? TODO
         null;

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
         Result.Add_Switch
           (Switch => GPR2.Options.Unchecked_Shared_Lib_Imports,
            Param  => Param,
            Index  => "");

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
               raise GPR2.Options.Usage_Error with
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

      elsif Arg = "-ws" then
         Result.Warnings := False;

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
