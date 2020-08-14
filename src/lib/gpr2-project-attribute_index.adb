------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Strings.Equal_Case_Insensitive;

package body GPR2.Project.Attribute_Index is

   use Ada.Strings;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Object) return Boolean is
      Is_Others : constant Boolean :=
                    Left.Is_Others = Right.Is_Others;
      L_Text    : constant String :=
                    (if Source_Reference.Value.Object (Left).Is_Defined
                     then Left.Text
                     else "");
      R_Text    : constant String :=
                    (if Source_Reference.Value.Object (Right).Is_Defined
                     then Right.Text
                     else "");
   begin
      if Left.Case_Sensitive then
         return Is_Others and then L_Text = R_Text;
      else
         return Is_Others
           and then Equal_Case_Insensitive (L_Text, R_Text);
      end if;
   end "=";

   --------------
   -- Set_Case --
   --------------

   procedure Set_Case
     (Self              : in out Object;
      Is_Case_Sensitive : Boolean) is
   begin
      Self.Case_Sensitive := Is_Case_Sensitive;
   end Set_Case;

end GPR2.Project.Attribute_Index;
