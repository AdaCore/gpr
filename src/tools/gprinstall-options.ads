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

with GPRtools.Options;

with GPR2.Project.Tree;

package GPRinstall.Options is

   use Ada.Strings.Unbounded;
   use GNAT;

   --  A Param, track if it is set on the command line or if it is the default
   --  value.

   type Param is record
      V       : Unbounded_String;
      Default : Boolean := False;
   end record;

   type Object is new GPRtools.Options.Base_Options with record
      List_Mode        : Boolean := False;
      Uninstall_Mode   : Boolean := False;
      Install_Manifest : Boolean := True;

      Force_Installations : Boolean := False;
      --  True if gprinstall is allowed to overwrite existing files

      Global_Prefix_Dir : Param := (Null_Unbounded_String, True);
      --  Root installation directory

      Global_Exec_Subdir : Param := (To_Unbounded_String ("bin" & DS), True);
      --  Subdirectory for executable

      Global_Lib_Subdir : Param := (To_Unbounded_String ("lib" & DS), True);
      --  Subdirectory for libraries

      Global_ALI_Subdir : Param := (To_Unbounded_String ("lib" & DS), True);
      --  Subdirectory for libraries' .ali file

      Global_Link_Lib_Subdir : Param := (To_Unbounded_String ("lib" & DS),
                                         True);
      --  Subdirectory for libraries sym links (on UNIX)

      Global_Sources_Subdir : Param := (To_Unbounded_String ("include" & DS),
                                        True);
      --  Subdirectory for sources

      Global_Project_Subdir : Param :=
                                (To_Unbounded_String ("share" & DS &
                                                      "gpr" & DS),
                                 True);
      --  Subdirectory used for the installed generated project file

      Global_Install_Mode : Param := (To_Unbounded_String ("dev"), True);
      --  Either dev or usage.
      --  "dev" if the installation is for developers (source of the libraries
      --  are also installed). If set to "usage" only the shared libraries are
      --  installed and/or the main executables.

      Global_Install_Name : Param := (To_Unbounded_String ("default"), True);
      --  The installation name, the default value is the project name without
      --  extension.

      Build_Vars        : Unbounded_String;
      --  Name of the build variables for the installed project file

      No_Build_Var      : Boolean := False;
      --  Whether a build variable is to be generated

      Build_Name        : Unbounded_String := To_Unbounded_String ("default");
      --  Name of the current build

      Recursive         : Boolean := False;
      --  Installation will recurse into all imported projects

      Dry_Run           : Boolean := False;
      --  Whether the actual installation takes place or not. If Dry_Run is set
      --  to True then the action will be displayed on the console but actually
      --  not performed.

      Output_Stats      : Boolean := False;
      --  Whether the stats are to be displayed when listing installed packages

      All_Sources       : Boolean := True;
      --  By default install all the sources. If set to False install only
      --  the sources needed to use the project (the interface for a SAL).

      No_Lib_Link       : Boolean := False;
      --  Whether to copy the shared library into the executable directory on
      --  Windows or create a link into the lib directory on UNIX.

      Create_Dest_Dir   : Boolean := False;
      --  Whether to create the missing directories in the destination point

      Sources_Only      : Boolean := False;
      --  Whether to copy only the projects sources. This means that the
      --  object, library, executable files are not to be copied.

      No_GPR_Install    : Boolean := False;
      --  Do not install a project file.

      Side_Debug        : Boolean := False;
      --  Whether the debug symbols are kept into the main executable (default)
      --  or written into a side debug file.
   end record;

   function Project_Dir (Self : Object) return String;
   --  Returns the full pathname to the destination project directory

   procedure Parse_Command_Line
     (Options : in out Object;
      Tree    : in out GPR2.Project.Tree.Object);

end GPRinstall.Options;
