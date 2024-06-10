with Ada.Text_IO;
package body Foo is

   ---------
   -- Foo --
   ---------

   procedure Foo (Caller : String) is
   begin
      Ada.Text_IO.Put_Line ("Foo called from " & Caller);
   end Foo;

end Foo;
