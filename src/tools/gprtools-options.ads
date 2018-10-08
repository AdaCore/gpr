------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

package GPRtools.Options is

   use GNAT.Command_Line;

   type Object is tagged record
      Config : Command_Line_Configuration;

      Help    : aliased Boolean := False;
      --  Set by switch -h: usage will be displayed after all command line
      --  switches have been scanned.

      Version : aliased Boolean := False;
      Quiet   : aliased Boolean := False;
      Verbose : aliased Boolean := False;
   end record;

   procedure Setup (Self : in out Object);
   --  Setup command line parsing options

end GPRtools.Options;
