with Ada.Text_IO;
with Pkg2;
with Pkg3;

procedure P is
begin
   Ada.Text_IO.Put_Line (Pkg2.P'Image);
   Ada.Text_IO.Put_Line (Pkg3.P'Image);
end P;
