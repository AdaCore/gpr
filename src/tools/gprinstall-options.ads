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

with GNAT.OS_Lib;

with GPRtools.Options;

package GPRinstall.Options is

   use GNAT;

   --  A Param, track if it is set on the command line or if it is the default
   --  value.

   type Param is record
      V       : OS_Lib.String_Access;
      Default : Boolean := False;
   end record;

   function Dup (P : Param) return Param;
   --  Return a copy of P

   procedure Free (P : in out Param);
   --  Free P

   type Object is new GPRtools.Options.Object with record
      List_Mode        : aliased Boolean := False;
      Uninstall_Mode   : aliased Boolean := False;
      Install_Manifest : aliased Boolean := True;

      Config_Project      : aliased OS_Lib.String_Access;
      Auto_Config_Project : aliased OS_Lib.String_Access;

      Install_Name        : aliased OS_Lib.String_Access;
      Mode                : aliased OS_Lib.String_Access;

      Force_Installations : aliased Boolean := False;
      --  True if gprinstall is allowed to overwrite existing files

      Global_Prefix_Dir : Param := (null, True);
      --  Root installation directory

      Global_Exec_Subdir : Param := (new String'("bin" & DS), True);
      --  Subdirectory for executable

      Global_Lib_Subdir : Param := (new String'("lib" & DS), True);
      --  Subdirectory for libraries

      Global_ALI_Subdir : Param := (new String'("lib" & DS), True);
      --  Subdirectory for libraries' .ali file

      Global_Link_Lib_Subdir : Param := (new String'("lib" & DS), True);
      --  Subdirectory for libraries sym links (on UNIX)

      Global_Sources_Subdir : Param := (new String'("include" & DS), True);
      --  Subdirectory for sources

      Global_Project_Subdir : Param :=
                                (new String'("share" & DS & "gpr" & DS), True);
      --  Subdirectory used for the installed generated project file

      Global_Install_Mode : Param := (new String'("dev"), True);
      --  Either dev or usage.
      --  "dev" if the installation is for developers (source of the libraries
      --  are also installed). If set to "usage" only the shared libraries are
      --  installed and/or the main executables.

      Global_Install_Name : Param := (new String'("default"), True);
      --  The installation name, the default value is the project name without
      --  extension.

      Build_Vars        : OS_Lib.String_Access;
      --  Name of the build variables for the installed project file

      No_Build_Var      : aliased Boolean := False;
      --  Whether a build variable is to be generated

      Build_Name        : aliased OS_Lib.String_Access :=
                              new String'("default");
      --  Name of the current build

      Recursive         : aliased Boolean := False;
      --  Installation will recurse into all imported projects

      Dry_Run           : aliased Boolean := False;
      --  Whether the actual installation takes place or not. If Dry_Run is set
      --  to True then the action will be displayed on the console but actually
      --  not performed.

      Output_Stats      : aliased Boolean := False;
      --  Whether the stats are to be displayed when listing installed packages

      All_Sources       : aliased Boolean := True;
      --  By default install all the sources. If set to False install only
      --  the sources needed to use the project (the interface for a SAL).

      No_Lib_Link       : aliased Boolean := False;
      --  Whether to copy the shared library into the executable directory on
      --  Windows or create a link into the lib directory on UNIX.

      Create_Dest_Dir   : aliased Boolean := False;
      --  Whether to create the missing directories in the destination point

      Sources_Only      : aliased Boolean := False;
      --  Whether to copy only the projects sources. This means that the
      --  object, library, executable files are not to be copied.

      Side_Debug        : aliased Boolean := False;
      --  Whether the debug symbols are kept into the main executable (default)
      --  or written into a side debug file.

      Subdirs           : aliased OS_Lib.String_Access;
      --  Make obj/lib/exec dirs as subdirs
   end record;

   function Project_Dir (Self : Object) return String;
   --  Returns the full pathname to the destination project directory

end GPRinstall.Options;
