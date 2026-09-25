with Ada.Text_IO; use Ada.Text_IO;

package body Pair_Both_Prep is

   Body_Value : constant Integer := $DEF_B_BODY;

   procedure Show is
   begin
      Put_Line (Spec_Value'Image & " " & Body_Value'Image);
   end Show;

end Pair_Both_Prep;
