with Ada.Text_IO;
with Bundle;

procedure Main is
begin
   Ada.Text_IO.Put_Line (Integer'Image (Bundle.Value));
end Main;
