with Ada.Text_IO;
with Pkg2;

package body Pkg1 is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from lib1");
      Pkg2.P;
   end P;

end Pkg1;