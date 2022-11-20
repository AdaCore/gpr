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

with GPR2.Containers;

with GPRtools;
with GPRtools.Options;

package GPRinspect is

   type Restricted_Scope (Restrict : Boolean := False) is record
      case Restrict is
         when True =>
            Views : GPR2.Containers.Name_List;
         when False =>
            null;
      end case;
   end record;

   type GPRinspect_Options is new GPRtools.Options.Base_Options with record
      Kind_Of_Display           : GPRtools.Display_Kind
        := GPRtools.K_Textual_IO;
      All_Projects              : Boolean := False;
      Display_Everything        : Boolean := False;
      Display_Attributes        : Boolean := False;
      Display_Config_Attributes : Boolean := False;
      Display_Packages          : Boolean := False;
      Display_Variables         : Boolean := False;
      Restricted_Views          : Restricted_Scope;
   end record;

end GPRinspect;
