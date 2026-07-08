package Pkg is
	procedure Print (Toto : Integer);
	pragma Export (C, Print, "pkg__print");
end Pkg;
