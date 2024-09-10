------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2024, AdaCore                      --
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

--  Common utilities for all gpr tools

with GPR2;

package GPRtools.Util is

   use GPR2;

   procedure Set_Program_Name (Name : String);
   --  Set GPR_TOOL environment variable if it is not yet defined.
   --  Note: gprclean, gprbuild, gprls, gprinstall, gprdump & gprdoc
   --        tools are setting GPR_TOOL to gprbuild instead of their own names.

   function Get_Program_Name return String;
   --  Get the internal value of the program name

   function Is_Ada_Predefined_Unit (Unit : Name_Type) return Boolean;
   --  Return True if Unit is an Ada runtime unit

   function Executable_Prefix_Path return String;
   --  Return the absolute path parent directory of the directory where the
   --  current executable resides, if its directory is named "bin", otherwise
   --  return an empty string. When a directory is returned, it is guaranteed
   --  to end with a directory separator.

   function Locate_Exec_On_Path (Exec_Name : String) return String;
   --  Get the path location of a given executable

   function Partial_Name
     (Lib_Name      : Simple_Name;
      Number        : Natural;
      Object_Suffix : Simple_Name) return Simple_Name;
   --  Returns the name of an object file created by the partial linker

end GPRtools.Util;
