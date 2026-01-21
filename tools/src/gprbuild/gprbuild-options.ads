------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GPR2;
with GPR2.Build.Process_Manager;
with GPR2.Build.External_Options;
with GPR2.Containers;

with GPRtools.Options;

package GPRbuild.Options is

   type Object is new GPRtools.Options.Base_Options with record
      Single_Build_Per_Obj_Dir : Boolean := False;
      No_Object_Check          : Boolean := False;
      No_Split_Units           : Boolean := False;

      Build_If_Switch_Changes  : Boolean := False;
      Create_Missing_Dirs      : Boolean := False;
      Force_Recursive_Build    : Boolean := False;

      Json_Summary             : Boolean := False;

      Extra_Args               : GPR2.Build.External_Options.Object;
      Config_Args              : GPR2.Containers.Value_List;
      PM_Options               : GPR2.Build.Process_Manager.PM_Options;

      Dash_A_Option            : Boolean := False;
      --  Ignored but reporting has to be done after the options are parsed
      --  to prevent multiple messages if the switch appears several times.

      Compiler_Substitution    : GPR2.Containers.Value_List;
   end record;
   --  Options for gprbuild

   type GPRbuild_Parser is
     new GPRtools.Options.Command_Line_Parser with null record;

   function Create return GPRbuild_Parser;

end GPRbuild.Options;
