with Ada.Text_IO;

package body Printer is
   procedure Show (X : Defs.Level) is
   begin
      Ada.Text_IO.Put_Line ("Printer: " & Defs.Level'Image (X));
   end Show;
end Printer;
