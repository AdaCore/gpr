with Ada.Text_IO;
package body Pck1 is
   procedure Call is
      procedure Ccall;
      pragma Import (C, Ccall);
   begin
      Ada.Text_IO.Put_Line ("Pck1.Call");
      Ccall;
   end Call;
end Pck1;
