package Pkg2 is
	procedure Print (Toto : Integer);
	pragma Export (C, Print, "pkg2__print");
end Pkg2;
