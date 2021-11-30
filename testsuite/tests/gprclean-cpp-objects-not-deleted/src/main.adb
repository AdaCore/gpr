with GNAT.IO;  use GNAT.IO;
procedure main is
   procedure Test;
   pragma Import (C, Test, "test");
begin
   Test;
   Put_Line ("Hello World!");
end main;
