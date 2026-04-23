with Ada.Text_IO; use Ada.Text_IO;
with Pkga;
package body Pkgb is
   procedure Execute is
   begin
      Pkga.Execute;
      Put_Line ("testing Pkgb.Execute");
   end Execute;
end Pkgb;

