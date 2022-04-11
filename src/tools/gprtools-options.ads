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

with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;

with GPRtools.Command_Line;

package GPRtools.Options is

   use Ada.Strings.Unbounded;

   type Command_Line_Parser is
     new Command_Line.Command_Line_Parser with private;

   type Base_Options is new Command_Line.Command_Line_Result with record
      --  Project file and context:

      Context                  : GPR2.Context.Object;
      Project_File             : GPR2.Path_Name.Object;
      Project_Is_Defined       : Boolean := False;
      No_Project               : aliased Boolean := False;
      Project_Base             : GPR2.Path_Name.Object;
      --  If defined, then process Project_File like it is located in the
      --  Project_Base.

      --  Project tree modifiers

      Root_Path                : GPR2.Path_Name.Object;
      Build_Path               : GPR2.Path_Name.Object;
      Src_Subdirs              : Ada.Strings.Unbounded.Unbounded_String;
      Subdirs                  : Ada.Strings.Unbounded.Unbounded_String;
      Implicit_With            : GPR2.Path_Name.Set.Object;
      Unchecked_Shared_Lib     : Boolean := False;

      --  Conf/Autoconf

      Config_Project           : GPR2.Path_Name.Object;
      Create_Missing_Config    : Boolean := False;
      Target                   : Ada.Strings.Unbounded.Unbounded_String :=
                                   Ada.Strings.Unbounded.To_Unbounded_String
                                     ("all");
      RTS_Map                  : GPR2.Containers.Lang_Value_Map;
      Skip_Default_KB          : aliased Boolean := False;
      KB_Locations             : GPR2.Path_Name.Set.Object;

      --  Non-switch arguments

      Args                     : GPR2.Containers.Value_Set;

      --  The project tree once loaded

      Tree                     : access GPR2.Project.Tree.Object;

      --  Verbosity control

      Verbosity                : GPRtools.Verbosity_Level := GPRtools.Regular;
      Full_Path_Name_For_Brief : aliased Boolean := False;
      Warnings                 : aliased Boolean := True;

      --  Distributed mode

      Distributed_Mode         : Boolean := False;
      Slaves                   : Unbounded_String;
      Slave_Env                : Unbounded_String;
      Slave_Env_Auto           : Boolean := False;
      Hash_Value               : Unbounded_String;
   end record;
   --  Options common to most gpr tools

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
      Allow_Distributed      : Boolean := False;
      Allow_Quiet            : Boolean := True;
      No_Project_Support     : Boolean := False;
      Allow_Implicit_Project : Boolean := True) return Command_Line_Parser;
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
     (Opt                : in out Base_Options'Class;
      Absent_Dir_Error   : Boolean;
      Handle_Information : Boolean := False;
      Handle_Errors      : Boolean := True;
      Handle_Lint        : Boolean := False) return Boolean
     with Pre => Opt.Tree /= null;
   --  Load project giiven in the options and display errors based on the
   --  selection given by Handle_{Error|Lint|Information).

   function Quiet (Self : Base_Options) return Boolean;

   function Verbose (Self : Base_Options) return Boolean;

   function Very_Verbose (Self : Base_Options) return Boolean;

   function Get_Target (Self : Base_Options) return GPR2.Name_Type;

   function Get_Subdirs
     (Self : Base_Options) return GPR2.Optional_Name_Type;

   function Get_Src_Subdirs
     (Self : Base_Options) return GPR2.Optional_Name_Type;

private

   type Command_Line_Parser is new Command_Line.Command_Line_Parser with record
      Find_Implicit_Project : Boolean := True;
   end record;

   function Quiet (Self : Base_Options) return Boolean is
     (Self.Verbosity = GPRtools.Quiet);

   function Verbose (Self : Base_Options) return Boolean is
     (Self.Verbosity = GPRtools.Verbose
      or else Self.Verbosity = GPRtools.Very_Verbose);

   function Very_Verbose (Self : Base_Options) return Boolean is
     (Self.Verbosity = GPRtools.Very_Verbose);

   function Get_Target
     (Self : Base_Options) return GPR2.Name_Type
   is (GPR2.Name_Type (To_String (Self.Target)));

   function Get_Subdirs
     (Self : Base_Options) return GPR2.Optional_Name_Type
   is (GPR2.Optional_Name_Type (To_String (Self.Subdirs)));

   function Get_Src_Subdirs
     (Self : Base_Options) return GPR2.Optional_Name_Type
   is (GPR2.Optional_Name_Type (To_String (Self.Src_Subdirs)));

end GPRtools.Options;
