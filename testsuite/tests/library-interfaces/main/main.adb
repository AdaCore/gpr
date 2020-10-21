with Ada.Text_IO; use Ada.Text_IO;

with Mylib;

procedure Main is
   function Get_I return Integer
      with Import, Convention => C, External_Name => "get_i";
begin
   Mylib.I := 20;
   Put_Line (Get_I'Image);
end Main;
