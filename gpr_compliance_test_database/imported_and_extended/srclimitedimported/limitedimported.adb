with Ada.Text_IO;
with ImportedRoot;
package body LimitedImported is
   procedure Msg is
   begin
      Ada.Text_IO.Put_Line ("From LimitedImported");
      ImportedRoot.Msg;
   end Msg;
end LimitedImported;
