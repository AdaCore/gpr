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

with Ada.Strings.Unbounded;

with GPR2.Build.Options;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Reporter.Console;

with GPRtools.Command_Line;

package GPRtools.Options is

   use Ada.Strings.Unbounded;

   type Command_Line_Parser is
     new Command_Line.Command_Line_Parser with private;

   type Base_Options is new GPR2.Options.Object
     and Command_Line.Command_Line_Result
   with record
      Check_Shared_Libs     : Boolean := False;
      --  Whether shared libs importing static libs should be checked.
      --  Unset by default for all tools unless specified during creation
      --  of the command line parser.

      Find_Implicit_Project : Boolean := True;
      --  Whether the tool allows finding implicit projects

      Tree                  : GPR2.Project.Tree.Object;
      --  The project tree once loaded

      Console_Reporter      : GPR2.Reporter.Console.Object :=
                                   GPR2.Reporter.Console.Create;
      No_Warnings           : Boolean := False;

      Build_Options         : GPR2.Build.Options.Build_Options;
   end record;
   --  Options common to most gpr tools

   Empty_Options : constant Base_Options;

   overriding function Remaining_Arguments
     (Result : Base_Options) return GPR2.Containers.Value_Set
     is (Result.Build_Options.Mains);

   overriding procedure Append_Argument
     (Result : in out Base_Options; Value : GPR2.Value_Type);

   procedure Setup (Tool : Which);
   --  Setup the GPR2 library options to properly handle the tool's attributes

   overriding procedure Get_Opt
     (Parser : Command_Line_Parser;
      Result : in out GPRtools.Command_Line.Command_Line_Result'Class);

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
      Check_Shared_Libs      : Boolean := False) return Command_Line_Parser;
   --  Defines the common switches to handle configuration and project tree
   --  load.
   --  Allow_No_Project: enables working without project files via --no-project
   --  Allow_Autoconf: enables the --autoconf switch that generates the
   --   configuration project if it does not exists.
   --  Allow_Distributed : enables the distributed compilation options
   --  Allow_Quiet: allows the quiet mode of the tool (-q)
   --  No_Project_Support: deactivate most switches, keeping only the
   --   distributed mode group.
   --  Allow_Implicit_Project: allow to specify the project file without
   --   the -P switch, or to use the project of the current directory if
   --   one exist, or the empty project.

   function Load_Project
     (Opt                      : in out Base_Options'Class;
      Absent_Dir_Error         : GPR2.Error_Level;
      Create_Missing_Dirs      : GPR2.Project.Tree.Missing_Dir_Behavior :=
                                   GPR2.Project.Tree.Do_Nothing;
      Handle_Errors            : Boolean := True;
      Restricted_To_Languages  : GPR2.Containers.Language_Set :=
                                   GPR2.Containers.Empty_Language_Set)
      return Boolean;
   --  Load project given in the options and display logs .

   function Quiet (Self : Base_Options) return Boolean;

   function Verbose (Self : Base_Options) return Boolean;

   function Very_Verbose (Self : Base_Options) return Boolean;

private
   use GPR2.Reporter;

   type Command_Line_Parser is new Command_Line.Command_Line_Parser with record
      Find_Implicit_Project : Boolean := True;
      Check_Shared_Libs     : Boolean := False;
   end record;

   function Quiet (Self : Base_Options) return Boolean is
     (Self.Console_Reporter.Verbosity = GPR2.Reporter.Quiet);

   function Verbose (Self : Base_Options) return Boolean is
     (Self.Console_Reporter.Verbosity = GPR2.Reporter.Verbose);

   function Very_Verbose (Self : Base_Options) return Boolean is
     (Self.Console_Reporter.Verbosity = GPR2.Reporter.Very_Verbose);

   Empty_Options : constant Base_Options :=
                     (GPR2.Options.Empty_Options with others => <>);

end GPRtools.Options;
