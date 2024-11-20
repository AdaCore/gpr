with Pkg1;
with Pkg2;
with Pkg3;
with Pkg4_1;
with Pkg4_2;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   Put_Line (Pkg1.Hello);
   Put_Line (Pkg2.Hello);
   Put_Line (Pkg3.Hello);
   Put_Line (Pkg4_1.Hello);
   Put_Line (Pkg4_2.Hello);
end Main;
