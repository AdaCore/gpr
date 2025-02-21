with Imp1;
with GNAT.IO; use GNAT.IO;
package body Imp2 is
   procedure Execute is
   begin
      Imp1.Execute;
      Put_Line ("Imp2.Execute extended");
   end Execute;
end;
