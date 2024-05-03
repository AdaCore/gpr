with Ada.Text_IO;
with Test1;
with Test2;
procedure Main is
begin
   Ada.Text_IO.Put_Line ("from Main");
   Test1.Msg;
   Test2.Msg;
end Main;
