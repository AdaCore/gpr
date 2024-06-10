with Ada.Text_IO;
with ExtendedRoot;
package body Extended is
   procedure Msg is
   begin
      Ada.Text_IO.Put_Line ("From Extended");
      ExtendedRoot.Msg;
   end Msg;
end Extended;
