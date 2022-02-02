with Ada.Text_IO;
package body Ada_Pkg1 is
   procedure Method1 (Name : String) is
   begin
      Ada.Text_IO.Put_Line ("Hello " & Name & "!");
   end Method1;
end Ada_Pkg1;
