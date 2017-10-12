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

with Ada.Containers.Ordered_Maps;

package GPR2.Project.Import.Set is

   package Set is new Ada.Containers.Ordered_Maps (Path_Name_Type, Object);

   subtype Object is Set.Map;

   --  ?? both routines below are not efficient at all as we need to parse the
   --  imported list to check for the base name. This will need to be reworked.

   function Contains (Self : Object; Base_Name : Name_Type) return Boolean;
   --  Returns True if the Base_Name of a project is part of the imported
   --  projects.

   function Get (Self : Object; Base_Name : Name_Type) return Import.Object
     with Pre => Contains (Self, Base_Name);
   --  Returns the imported project object given the base name

private

   function Contains
     (Self : Object; Base_Name : Name_Type) return Boolean
   is (for some C in Self.Iterate => GPR2.Base_Name (Set.Key (C)) = Base_Name);

end GPR2.Project.Import.Set;
