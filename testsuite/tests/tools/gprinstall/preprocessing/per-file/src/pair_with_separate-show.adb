with Ada.Text_IO; use Ada.Text_IO;

separate (Pair_With_Separate)
procedure Show is

   Sep_Value : constant Integer := $DEF_SEP_VALUE;

begin
   Put_Line (Sep_Value'Image);
end Show;
