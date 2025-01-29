with Ada.Text_IO;
with Pkg3;

package body Pkg2 is

   procedure P is
   begin
      Pkg3.P;
      Ada.Text_IO.Put_Line ("Hello from lib2");
   end P;

end Pkg2;