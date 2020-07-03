------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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
with GPR2.Project.Configuration;
with GPR2.Project.Tree;

with GPRtools.Options;

with GNAT.Command_Line;

package GPRclean.Options is

   use Ada.Strings.Unbounded;
   use GNAT.Command_Line;
   use GPR2;

   type Object is new GPRtools.Options.Object with record
      Dry_Run                     : aliased Boolean := False;
      All_Projects                : aliased Boolean := False;
      Remain_Useful               : aliased Boolean := False;
      No_Project                  : aliased Boolean := False;
      Remove_Empty_Dirs           : aliased Boolean := False;
      Force_Deletions             : aliased Boolean := False;

      Arg_Mains     : Boolean;
      Implicit_Proj : Boolean := False;
      Config_File   : Path_Name.Object;
      Remove_Config : Boolean := False;
      Subdirs       : Unbounded_String;
      Src_Subdirs   : Unbounded_String;
   end record;

   procedure Parse_Command_Line
     (Options      : in out Object;
      Project_Tree : in out Project.Tree.Object;
      Parser       : Opt_Parser := Command_Line_Parser);

   function Mains (Self : Object) return GPR2.Containers.Value_Set;
   --  Return list of mains from command line

   overriding procedure Append (Self : in out Object; Next : Object);
   --  Append options values from Next to Self. Could be used to concatenate
   --  additional switches from Clean project package with command line taken
   --  switches.

private

   function Mains (Self : Object) return GPR2.Containers.Value_Set is
      (Self.Args);

end GPRclean.Options;
