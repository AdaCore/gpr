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

--  A container of slave object

with Ada.Containers.Doubly_Linked_Lists;

package GPR2.Compilation.Slave.List is

   package List is new Ada.Containers.Doubly_Linked_Lists (Object);

   type Object is new List.List with private;

   Empty_Set : constant Object;

private

   type Object is new List.List with null record;

   Empty_Set : constant Object := (List.Empty_List with null record);

end GPR2.Compilation.Slave.List;
