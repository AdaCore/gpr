
with Ada.Text_IO;
with Foo;

package body Pkg is

   use Ada;

   procedure Call is
   begin
      Text_IO.Put_Line ("Pkg.Call");
      Foo.Call;
   end Call;

end Pkg;
