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

with GNAT.Command_Line;

with GPR2.Containers;
with GPR2.Path_Name;

package GPRtools.Options is

   use GNAT.Command_Line;

   type Object is tagged record
      Config  : Command_Line_Configuration;

      Help    : aliased Boolean := False;
      --  Set by switch -h: usage will be displayed after all command line
      --  switches have been scanned.

      Version : aliased Boolean := False;
      Quiet   : aliased Boolean := False;
      Verbose : aliased Boolean := False;

      Root_Path  : GPR2.Path_Name.Object;
      Build_Path : GPR2.Path_Name.Object;
   end record;

   procedure Setup (Self : in out Object);
   --  Setup command line parsing options

   procedure Read_Remaining_Arguments
     (Project : in out GPR2.Path_Name.Object;
      Mains   :    out GPR2.Containers.Value_Set);
   --  Take project and main file names from command line parameters.
   --  Should be used in gprclean and gprbuild.

   procedure Clean_Build_Path
     (Self : in out Object; Project : GPR2.Path_Name.Object)
     with Pre => Project.Is_Defined,
          Post => Self.Build_Path.Is_Defined;
   --  If Self.Build_Path is not defined, set it to the Project directory and
   --  return. If Self.Root_Path is not defined, Self.Build_Path is kept as is,
   --  otherwise add to Self.Build_Path the relative path offset between
   --  Project and Self.Root_Path.
   --  For example, if Build is /one/two/three, the project is in the
   --  /four/five/six, the Root is /four, the difference between project
   --  directory and Root is five/six, then the resulting build directory is
   --  /one/two/three plus five/six, i.e /one/two/three/five/six.

end GPRtools.Options;
