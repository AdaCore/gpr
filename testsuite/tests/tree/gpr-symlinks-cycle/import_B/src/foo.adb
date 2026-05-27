
with Ada.Text_IO;
with Bar;

package body Foo is

   use Ada;

   procedure Call is
   begin
      Bar.Call;
      Text_IO.Put_Line ("Foo.Call");
   end Call;

end Foo;
