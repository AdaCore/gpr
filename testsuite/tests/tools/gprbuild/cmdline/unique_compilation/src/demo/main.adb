with Ada.Text_IO; use Ada.Text_IO;
with Pkg, Pkg2;
procedure Main is
begin
   Put_Line ("I'm main");
   Put_Line (Pkg.Hello);
   Put_Line (Pkg2.Hello);
end Main;
