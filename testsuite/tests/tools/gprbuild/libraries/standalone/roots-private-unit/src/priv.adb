with Ada.Text_IO;

package body Priv is

   procedure Secret is
   begin
      Ada.Text_IO.Put_Line ("secret");
   end Secret;

end Priv;
