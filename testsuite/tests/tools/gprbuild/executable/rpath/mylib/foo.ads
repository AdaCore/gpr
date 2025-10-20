package Foo is
   function Mult (S1, S2 : in Integer) return Integer;
   pragma Export (C, Mult);
end Foo;
