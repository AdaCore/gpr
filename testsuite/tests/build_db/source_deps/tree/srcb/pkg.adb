with Foo;
with My_String;
--  Intentionally not existing

procedure Pkg (Put_Line : access procedure (Val : String))
  S : constant My_String.My_String := "Hello World!";
begin
   Put_Line (S);
end Pkg;
