with Ada.Text_IO;
package body Pck2 is
   procedure Call is
      procedure Ccall;
      pragma Import (C, Ccall);
   begin
      Ada.Text_IO.Put_Line ("Pck2.Call");
      Ccall;
   end Call;
end Pck2;
