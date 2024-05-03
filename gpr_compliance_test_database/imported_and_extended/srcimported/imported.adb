with Ada.Text_IO;
with LimitedImportedRoot;
package body Imported is
   procedure Msg is
   begin
      Ada.Text_IO.Put_Line ("From Imported");
      LimitedImportedRoot.Msg;
   end Msg;
end Imported;
