------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2021-2022, AdaCore                     --
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

with GPR2.Project.Source;

package GPRls.Gnatdist is
   --  Any modification to this subunit requires synchronization with the
   --  GNATDIST sources.

   procedure Output_ALI
     (Source : GPR2.Project.Source.Object; Index : GPR2.Unit_Index);
   --  Output the unit information for GNATDIST

   procedure Output_No_ALI
     (Source : GPR2.Project.Source.Object; Index : GPR2.Unit_Index);
   --  Indicate that an ALI file cannot be found

end GPRls.Gnatdist;
