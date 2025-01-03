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

with GPR2;
with GPR2.Containers;
with GPR2.Path_Name;

with GPRtools.Command_Line;
with GPRtools.Options;

package GPRbuild.Options is

   type Object is new GPRtools.Options.Base_Options with record
      Single_Build_Per_Obj_Dir : Boolean := False;
      Build_Script             : GPR2.Path_Name.Object;
      No_Object_Check          : Boolean := False;
      Restricted_To_Languages  : GPR2.Containers.Language_Set;
      Display_Progress         : Boolean := False;

      Build_If_Switch_Changes  : Boolean := False;
      Force                    : Boolean := False;
      Keep_Going               : Boolean := False;
      Keep_Temp_Files          : Boolean := False;
      Create_Missing_Dirs      : Boolean := False;
      Force_Recursive_Build    : Boolean := False;

      Parallel_Tasks           : Natural := 1;
      Json_Summary             : Boolean := False;

      Config_Args              : GPR2.Containers.Value_List;
   end record;
   --  Options for gprls

   type GPRbuild_Parser is
     new GPRtools.Options.Command_Line_Parser with null record;

   function Create return GPRbuild_Parser;

   overriding procedure Get_Opt
     (Parser : GPRbuild_Parser;
      Result : in out GPRtools.Command_Line.Command_Line_Result'Class);

end GPRbuild.Options;
