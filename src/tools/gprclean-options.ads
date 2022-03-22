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

with GPR2.Containers;

with GPRtools.Options;

package GPRclean.Options is

   use GPR2;

   type Object is new GPRtools.Options.Base_Options with record
      Dry_Run           : aliased Boolean := False;
      All_Projects      : aliased Boolean := False;
      Remain_Useful     : aliased Boolean := False;
      Remove_Empty_Dirs : aliased Boolean := False;
      Force_Deletions   : aliased Boolean := False;

      Arg_Mains         : Boolean := False;
      Mains             : GPR2.Containers.Filename_Set;
      --  As soon as main found in a project, it is removed from Mains_In_Cmd
      --  and Mains is updated if extension added.
      Remove_Config     : Boolean := False;
   end record;

   procedure Setup (Parser : out GPRtools.Options.Command_Line_Parser);

   procedure Parse_Command_Line
     (Parser       : GPRtools.Options.Command_Line_Parser;
      Options      : in out Object);

   procedure Parse_Attribute_Switches
     (Parser  : GPRtools.Options.Command_Line_Parser;
      Options : in out Object;
      Values  : GPR2.Containers.Source_Value_List);

end GPRclean.Options;
