package Pkg is
   Name : constant String := "Original";
   procedure Execute;
   pragma Export (C, Execute, "pkg_execute");
end Pkg;

