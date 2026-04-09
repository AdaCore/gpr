with Pkg;
with Pkg2;
with Pkg3;
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
begin
   Put_Line (Pkg2.name);
end;

package Pkg2 is
   Name : constant String := "Pkg2";
end Pkg2;

