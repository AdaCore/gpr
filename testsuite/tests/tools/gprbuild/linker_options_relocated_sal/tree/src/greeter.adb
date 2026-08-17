with Ada.Text_IO; use Ada.Text_IO;

package body Greeter is

   procedure Hello is
   begin
      Put_Line ("hello from relocated SAL");
   end Hello;

end Greeter;
