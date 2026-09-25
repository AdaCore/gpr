with Ada.Text_IO; use Ada.Text_IO;

package body Pair_Body_Only_Prep is

   Body_Value : constant Integer := $DEF_C_BODY;

   procedure Show is
   begin
      Put_Line (Body_Value'Image);
   end Show;

end Pair_Body_Only_Prep;
