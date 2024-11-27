procedure Main is
   procedure Hello (S : String);
   pragma Import (C, Hello, "hello");
begin
   Hello ("Hello there" & ASCII.NUL);
end Main;
