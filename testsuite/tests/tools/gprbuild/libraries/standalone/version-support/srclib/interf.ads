package Interf is
   procedure Execute;
   pragma Export (C, Execute, "lib_execute");
end Interf;

