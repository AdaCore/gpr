with Impl;
with Ada.Text_IO;
package body Interf is
   procedure Execute is
     Version : constant String := Impl'Version;
   begin
      Ada.Text_IO.Put_Line ("impl version: " & Version);
   end Execute;
end Interf;
