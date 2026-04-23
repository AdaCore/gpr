package Pkg2 is
   procedure Execute;
   pragma Export (C, Execute, "pkg2_execute");
end Pkg2;

