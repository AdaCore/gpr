with Ada.Text_IO; use Ada.Text_IO;
with Separates_Pkg;

procedure Main is
begin
   Put_Line ("Func_1 =" & Integer'Image (Separates_Pkg.Func_1));
   Put_Line ("Func_2 =" & Integer'Image (Separates_Pkg.Func_2));
end Main;
