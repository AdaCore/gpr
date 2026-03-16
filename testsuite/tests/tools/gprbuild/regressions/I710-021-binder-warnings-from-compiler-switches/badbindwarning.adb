procedure Badbindwarning is
   A : Integer;
   B : Integer;
   pragma Unreferenced (B);
begin
   A := 1;
   B := A;
end Badbindwarning;
