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

with Ada.Strings.Unbounded;

with GNAT.Command_Line;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Path_Name.Set;
with GPR2.Project.Tree;

package GPRtools.Options is

   use Ada.Strings.Unbounded;
   use GNAT.Command_Line;

   type Object is tagged record
      Tool    : Which;
      Config  : Command_Line_Configuration;

      Help    : aliased Boolean := False;
      --  Set by switch -h: usage will be displayed after all command line
      --  switches have been scanned.

      Project_File : GPR2.Path_Name.Object;
      --  The project to be processed

      Project_Base : GPR2.Path_Name.Object;
      --  If defined, then process Project_File like it is located in the
      --  Project_Base.

      Tree : access GPR2.Project.Tree.Object;

      Args : GPR2.Containers.Value_Set;
      --  Another arguments from command line except project.
      --  It is mains for gprclean and gprbuild.
      --  It is install name for gprinstall --uninstall.
      --  It could be object or dependency files for gprls.

      No_Project               : aliased Boolean := False;
      Unchecked_Shared_Lib     : aliased Boolean := False;
      Full_Path_Name_For_Brief : aliased Boolean := False;
      Version                  : aliased Boolean := False;
      Warnings                 : aliased Boolean := True;
      Target                   : Unbounded_String :=
                                   To_Unbounded_String ("all");
      RTS_Map                  : GPR2.Containers.Name_Value_Map;
      Context                  : GPR2.Context.Object;
      Distributed_Mode         : Boolean := False;
      Slaves                   : Unbounded_String;
      Slave_Env                : Unbounded_String;
      Slave_Env_Auto           : Boolean := False;
      Hash_Value               : Unbounded_String;

      Debug_Mode               : aliased Boolean := False;
      Verbosity                : Verbosity_Level := Regular;
      Root_Path                : GPR2.Path_Name.Object;
      Build_Path               : GPR2.Path_Name.Object;
      Src_Subdirs              : Unbounded_String;
      Implicit_With            : GPR2.Path_Name.Set.Object;
      Maximum_Processes        : Natural := 0;

      Skip_Default_KB          : aliased Boolean := False;
      KB_Locations             : GPR2.Path_Name.Set.Object;
   end record;

   procedure Setup
     (Self : aliased in out Object'Class;
      Tool : Which);
   --  Setup command line parsing options

   procedure Read_Remaining_Arguments (Self : in out Object; Tool : Which);
   --  This is called after processing all the switches, to read all remaining
   --  parameters from the command line. This is intended for gprclean,
   --  gprbuild and gprinstall.
   --
   --  Strings ending with a gpr file name extension are interpreted as a
   --  project file name. All the other strings are recorded as arguments
   --  (in Self.Args), which each specific tool processes on its own.

   procedure Clean_Build_Path
     (Self : in out Object; Project : GPR2.Path_Name.Object)
     with Pre  => Project.Is_Defined,
          Post => Self.Build_Path.Is_Defined;
   --  If Self.Build_Path is not defined, set it to the Project directory and
   --  return. If Self.Root_Path is not defined, Self.Build_Path is kept as is,
   --  otherwise add to Self.Build_Path the relative path offset between
   --  Project and Self.Root_Path.
   --  For example, if Build is /one/two/three, the project is in the
   --  /four/five/six, the Root is /four, the difference between project
   --  directory and Root is five/six, then the resulting build directory is
   --  /one/two/three plus five/six, i.e /one/two/three/five/six.

   function Verbose (Self : Object) return Boolean is
     (Self.Verbosity = Verbose);

   function Quiet (Self : Object) return Boolean is
     (Self.Verbosity = Quiet);

   procedure Append (Self : in out Object; Next : Object);
   --  Append options values from Next to Self. Could be used to concatenate
   --  additional switches from GPR tools related project packages with command
   --  line taken switches.

end GPRtools.Options;
