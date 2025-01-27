with Ada.Text_IO;
with Internal;

package body Foo is
   procedure Foo is
   begin
      Ada.Text_IO.Put_Line ("Hello from Foo");
      Internal.Internal;
   end Foo;
end Foo;
