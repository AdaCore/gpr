with Ada.Text_IO;

with Config;

package body Greeter is

   -----------
   -- Greet --
   -----------

   procedure Greet (Name : String) is
   begin
      Ada.Text_IO.Put_Line (Config.Prefix & Name);
   end Greet;

end Greeter;
