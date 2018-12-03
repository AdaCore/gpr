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

package body GPR2.ALI.Unit_Data is

   procedure Add_With (Self : in out Object; W : With_Data.Object) is
   begin
      Self.Withs.Append (W);
   end Add_With;

   procedure Set_Flags (Self : in out Object; Flags : Flag_Array) is
   begin
      Self.Flags := Flags;
   end Set_Flags;

   procedure Set_Unit_Kind (Self : in out Object; Kind : Character) is
   begin
      Self.Unit_Kind := Kind;
   end Set_Unit_Kind;

   procedure Set_Utype (Self : in out Object; Utype : Unit_Type) is
   begin
      Self.Utype := Utype;
   end Set_Utype;

end GPR2.ALI.Unit_Data;
