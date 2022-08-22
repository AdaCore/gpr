--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
