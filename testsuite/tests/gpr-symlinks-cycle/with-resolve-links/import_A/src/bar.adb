with Ada.Text_IO;

package body Bar is

   procedure Call is
   begin
      Ada.Text_IO.Put_Line ("Bar.Call");
   end Call;

end Bar;
