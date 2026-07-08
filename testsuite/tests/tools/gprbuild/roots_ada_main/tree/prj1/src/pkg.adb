with Ada.Text_IO;

package body Pkg is
	procedure Print (Toto : Integer) is
	begin
		Ada.Text_IO.Put_Line ("Number :" & Toto'Img);
	end Print;
end Pkg;
