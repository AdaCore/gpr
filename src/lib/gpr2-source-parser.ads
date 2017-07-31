------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GPR2.Source_Reference.Set;

private package GPR2.Source.Parser is

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

   function Check (Filename : Path_Name_Type) return Data;
   --  Check the sources and returns the corresponding information

end GPR2.Source.Parser;
