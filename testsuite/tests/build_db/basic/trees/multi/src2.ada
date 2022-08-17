package Pkg2 is
   procedure Execute;
end Pkg2;

with Ada.Text_IO; use Ada.Text_IO;
package body Pkg2 is
   procedure Execute is
   begin
      Put_Line ("Pkg.Execute");
   end Execute;
end Pkg2;

with Pkg2;
procedure Main2 is
begin
   Pkg2.Execute;
end Main2;
