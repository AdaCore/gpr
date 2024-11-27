procedure Main is
   procedure Foo;
   pragma Import (C, Foo, "foo");
begin
   Foo;
end Main;