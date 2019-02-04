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

with Ada.Strings.Unbounded;

with GPR2.Source_Reference.Set;

private package GPR2.Source.Parser is

   use Ada.Strings.Unbounded;

   --  This is to get some information from the Ada sources. We need for
   --  example to know if the unit is a separate unit (not possible to detect
   --  this based on the file extension if the same as the body). We also need
   --  to get the list of withed units to be able to compute the full closure
   --  of a given unit.

   type Data is record
      Is_Separate : Boolean := False;
      Sep_From    : Unbounded_String;
      W_Units     : Source_Reference.Set.Object;
      Unit_Name   : Unbounded_String;
   end record;

   function Check (Filename : GPR2.Path_Name.Object) return Data;
   --  Checks the sources and returns the corresponding information

end GPR2.Source.Parser;
