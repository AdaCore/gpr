with Foo;
with Ada.Text_IO;

procedure Main is
   procedure Ioo;
   pragma Import (C, Ioo, "ioo");
begin
   Foo.Foo;
   Ioo;
end Main;
