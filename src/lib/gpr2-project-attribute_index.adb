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
      if L_Text = "" and then R_Text = "" then
         --  Empty index should be different from not defined index

         return Source_Reference.Value.Object (Left).Is_Defined
           = Source_Reference.Value.Object (Right).Is_Defined;
      end if;
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
