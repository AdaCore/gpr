with Ada.Text_IO;

package body Pkg is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from mylib in Ada");
   end P;
   procedure F is
   begin
	   Ada.Text_IO.Put_Line ("Bye from mylib in Ada");
   end F;

end Pkg;
