package Pck is
   Var : constant Integer := 1;
end Pck;

with GNAT.IO;
with Pck;
procedure Call is
begin
   GNAT.IO.Put_Line ("V : " & Integer'Image (Pck.Var));
end Call;

with Call;
procedure Main is
begin
   Call;
end Main;
