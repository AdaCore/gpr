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

package body GPRtools.Options is

   -----------
   -- Setup --
   -----------

   procedure Setup (Self : in out Object) is
   begin
      Define_Switch
        (Self.Config, Self.Help'Access,
         "-h", Long_Switch => "--help",
         Help           => "Display this help message and exit");

      Define_Switch
        (Self.Config, Self.Version'Access,
         Long_Switch => "--version",
         Help => "Display version and exit");

      Define_Switch
        (Self.Config, Self.Verbose'Access,
         "-v", "--verbose",
         Help => "Verbose output");

      Define_Switch
        (Self.Config, Self.Quiet'Access,
         "-q", "--quiet",
         Help => "Be quiet/terse");
   end Setup;

end GPRtools.Options;
