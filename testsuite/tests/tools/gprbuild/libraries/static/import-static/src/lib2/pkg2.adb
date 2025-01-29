with Ada.Text_IO;
with Pkg3;

package body Pkg2 is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from lib2");
      Pkg3.P;
   end P;

end Pkg2;