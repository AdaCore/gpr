package body Pkg is

   procedure P is
      procedure Foo;
      pragma Import (C, Foo, "foo");
   begin
      Foo;
   end P;
end Pkg;