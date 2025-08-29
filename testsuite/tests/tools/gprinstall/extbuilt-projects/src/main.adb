with Ada.Text_IO;
with Pck2;

procedure Main is
   function Mult (V : Integer) return Integer;
   pragma Import (C, Mult, "c_call");
begin
   Ada.Text_IO.Put_Line ("This is the main : " & Integer'Image (Mult (2)));
   Pck2.Call;
end Main;
