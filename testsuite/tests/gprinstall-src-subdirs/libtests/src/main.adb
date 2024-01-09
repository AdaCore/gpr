with Ada.Text_IO; use Ada.Text_IO;
with Counters;    use Counters;

procedure Main is
   C : Counter;
begin
   pragma Assert (C.Value = 0);
   Bump (C);

   Put_Line ("Reaching end of test : " & C.Value'Img);
end Main;