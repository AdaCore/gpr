------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GPR2.Compilation_Unit.List;

private package GPR2.Source.Parser is
   --  The following package provides unit information from Ada sources,
   --  using Libadalang.

   function Parse
     (Filename : GPR2.Path_Name.Object) return Compilation_Unit.List.Object
     with Pre => Filename.Is_Defined;
   --  Parses the source Filename with Libadalang and returns a list of
   --  Compilation Units.
   --  An empty result means that something went wrong, and we should not
   --  attempt to use information from the parser.
   --  In any case, information from the project has precedence because Ada
   --  sources may contain bad syntax and we still want to try building the
   --  project as the user intended.

end GPR2.Source.Parser;
