with Ada.Text_IO; use Ada.Text_IO;
with Pkg;
package body Pkg2 is
   procedure Execute is
   begin
      Put_Line ("Pkg2: " & Pkg.Name);
   end Execute;
end Pkg2;

