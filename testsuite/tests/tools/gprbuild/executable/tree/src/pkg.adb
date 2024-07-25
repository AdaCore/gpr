with Ada.Text_IO;
with Dep_Two;

package body Pkg is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from Pkg");
      Dep_Two.P;
   end P;
end Pkg;
