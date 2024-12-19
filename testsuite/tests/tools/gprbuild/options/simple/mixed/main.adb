with Ada.Text_IO;

procedure Main is
   procedure In_C;
   pragma Import (C, In_C, "in_c");
begin
   Ada.Text_IO.Put_Line ("Hello there");
   In_C;
end Main;
