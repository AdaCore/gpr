with Ada.Text_IO; use Ada.Text_IO;
package body Util is
   procedure Print2 is separate;
    procedure Print (X : Integer) is
   begin
      Put ("case2");
      Print2;
      Put_Line (X'img);
   end Print;
end Util;
