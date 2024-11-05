package Pkg is
   function Get_Message return String;
end Pkg;

with Ada.Text_IO;
with Pkg;

procedure Main is
   pragma Unexpected;
   --  Check that a warning in a multi-unit is properly
   --  replayed.
begin
   Ada.Text_IO.Put_Line (Pkg.Get_Message);
end Main;