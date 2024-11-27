package Pkg is

   procedure P;
   pragma Export (C, P, "exported_to_c");

end Pkg;