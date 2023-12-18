with Test1;
with Test2;
with Ada.Text_IO;
procedure Main is
begin
   Ada.Text_IO.Put_Line (Test1.Msg & Test2.Msg);
end Main;
