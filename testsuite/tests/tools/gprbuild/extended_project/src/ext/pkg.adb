with Ada.Text_IO;

package body Pkg is
   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hi, I'm from ext");
   end P;
end Pkg;