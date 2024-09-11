with Ada.Text_IO;
package body Ops is
   procedure Msg is
   begin
      Ada.Text_IO.Put_Line ("from Ops.Msg");
   end Msg;
end Ops;
