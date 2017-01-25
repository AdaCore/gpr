------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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

with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Project.Import.Set is

   package Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Path_Name_Type, Object);

   subtype Object is Set.Map;

   function Contains (Self : Object; Base_Name : Name_Type) return Boolean;
   --  Returns True if the Base_Name of a project is part of the imported
   --  projects.

private

   function Contains
     (Self : Object; Base_Name : Name_Type) return Boolean
   is (for some C in Self.Iterate => GPR2.Base_Name (Set.Key (C)) = Base_Name);

end GPR2.Project.Import.Set;
