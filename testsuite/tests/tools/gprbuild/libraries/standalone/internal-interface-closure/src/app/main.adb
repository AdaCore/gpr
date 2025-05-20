with Foo;
with Ada.Text_IO;

procedure Main is
   --  Goo should be there since it has been imported by Foo. Let's check
   procedure Goo;
   pragma Import (C, Goo, "goo");
begin
   Foo.Foo;
   Goo;
end Main;