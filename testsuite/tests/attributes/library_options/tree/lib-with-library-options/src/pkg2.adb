with Ada.Text_IO;
package body Pkg2 is

   function Foo return Integer;
   pragma Import (C, Foo);

   Value : Integer := 0;

   procedure Execute is
   begin
      Value := Foo;
      Ada.Text_IO.Put_Line ("Value: " & Integer'Image (Value));
   end;

end;

