
with Ada.Text_IO;

with Pck1;
with Pck2;

procedure Main is
   use Ada;
begin
   Text_IO.Put_Line ("V1: " & Pck1.V);
   Text_IO.Put_Line ("V2: " & Pck2.V);
end Main;
