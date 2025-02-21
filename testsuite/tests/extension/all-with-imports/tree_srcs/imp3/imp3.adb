with Imp1;
with GNAT.IO; use GNAT.IO;
package body Imp3 is
   procedure Execute is
   begin
      Imp1.Execute;
      Put_Line ("Imp3.Execute");
   end Execute;
end;
