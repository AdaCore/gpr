package Pkg is
   Name : constant String := "New";
   procedure Execute;
   pragma Export (C, Execute, "pkg_execute");
end Pkg;

