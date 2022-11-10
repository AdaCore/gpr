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

package GPRtools is

   Usage_Error : exception;
   --  Raised when a wrong usage is detected

   type Which is (Build, Clean, Ls, Install, Name, Remote, Inspect);
   --  Names of the supported tools

   type Verbosity_Level is (Quiet, Regular, Verbose, Very_Verbose);
   --  Verbosilty level of logging to standard and/or error/warning output

   type Display_Kind is (K_JSON_Compact, K_JSON, K_Textual_IO);
   --  Kind of display to be used by any tool that uses it.

end GPRtools;
