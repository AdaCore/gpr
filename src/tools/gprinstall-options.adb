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

package body GPRinstall.Options is

   ---------
   -- Dup --
   ---------

   function Dup (P : Param) return Param is
   begin
      return (new String'(P.V.all), P.Default);
   end Dup;

   ----------
   -- Free --
   ----------

   procedure Free (P : in out Param) is
   begin
      OS_Lib.Free (P.V);
   end Free;

   -----------------
   -- Project_Dir --
   -----------------

   function Project_Dir (Self : Object) return String is
   begin
      if OS_Lib.Is_Absolute_Path (Self.Global_Project_Subdir.V.all) then
         return Self.Global_Project_Subdir.V.all;
      else
         return Self.Global_Prefix_Dir.V.all
                & Self.Global_Project_Subdir.V.all;
      end if;
   end Project_Dir;

end GPRinstall.Options;
