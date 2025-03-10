with Ada.Text_IO; use Ada.Text_IO;
package body Util is
   procedure Print (X : Integer) is
   begin
      Put_Line ("case1" & X'img);
   end Print;

end Util;
