with Ada.Text_IO;
package body Test is
   procedure Msg is
   begin
      Ada.Text_IO.Put_Line ("from Test.Msg");
   end Msg;
end Test;