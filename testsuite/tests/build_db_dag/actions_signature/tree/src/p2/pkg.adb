with Ada.Text_IO;
with Pkg2;

package body Pkg is

   procedure P2 is
   begin
     Ada.Text_IO.Put_Line ("Hey there!");
     Pkg2.Foo;
   end P2;

end Pkg;