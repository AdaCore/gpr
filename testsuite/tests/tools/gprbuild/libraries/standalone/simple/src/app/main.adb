with Add;
with Sub;
with Ada.Text_IO;

procedure Main is
begin
   Ada.Text_IO.Put_Line ("1+1 = " & Add.Add(1,1)'Image);
   Ada.Text_IO.Put_Line ("1-1 = " & Sub.Sub (1,1)'Image);
end Main;