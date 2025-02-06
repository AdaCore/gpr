with Foo;
with Internal;

procedure A is
begin
   Foo.Foo;
   Internal.Internal;
   --  Should not compile, because internal does not belong to the B Interfaces
end A;
