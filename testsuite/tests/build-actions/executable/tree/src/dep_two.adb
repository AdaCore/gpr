with Ada.Text_IO;

package body Dep_Two is

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from Dep_Two");
   end P;
end Dep_Two;
