
with Ada.Text_IO;

package body Foo is

   use Ada;

   procedure Call is
   begin
      Text_IO.Put_Line ("Foo.Call");
   end Call;

end Foo;
