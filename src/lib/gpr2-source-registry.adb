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

package body GPR2.Source.Registry is

   ------------
   -- Shared --
   ------------

   protected body Shared is

      ---------
      -- Get --
      ---------

      function Get (Object : Source.Object) return Data is
      begin
         return Store (Object.Pathname);
      end Get;

      --------------
      -- Register --
      --------------

      procedure Register (Def : Data) is
      begin
         if not Store.Contains (Def.Path_Name) then
            Store.Insert (Def.Path_Name, Def);
         end if;
      end Register;

      ---------
      -- Set --
      ---------

      procedure Set (Object : Source.Object; Def : Data) is
      begin
         Store (Object.Pathname) := Def;
      end Set;

      --------------------
      -- Set_Other_Part --
      --------------------

      procedure Set_Other_Part (Object1, Object2 : Object) is
      begin
         Store (Object1.Pathname).Other_Part := Object2.Pathname;
         Store (Object2.Pathname).Other_Part := Object1.Pathname;
      end Set_Other_Part;

   end Shared;

end GPR2.Source.Registry;
