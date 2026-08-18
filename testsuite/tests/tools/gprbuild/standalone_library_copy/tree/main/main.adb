with Ada.Text_IO;

with Fast;
with Greeter;

procedure Main is
begin
   Greeter.Greet ("world");
   Ada.Text_IO.Put_Line (Fast.Double (21)'Image);
end Main;
