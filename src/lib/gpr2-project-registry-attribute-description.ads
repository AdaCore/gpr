------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

--  This package provides to GPR library the known description of defined at-
--  tributes.

with Ada.Containers; use Ada.Containers;

package GPR2.Project.Registry.Attribute.Description is

   function Get_Attribute_Description (Key : Qualified_Name)
                                       return String;
   --  Retrieve a description for a given attribute and package.

private

   function Hash (Key : Qualified_Name) return Hash_Type;

   package Pack_Attribute_Description is new Indefinite_Hashed_Maps
     (Qualified_Name, String, Hash, "=", "=");

   Attribute_Description : Pack_Attribute_Description.Map;

end GPR2.Project.Registry.Attribute.Description;
