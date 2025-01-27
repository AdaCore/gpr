with Ada.Text_IO;
with Pkg2;
pragma Elaborate_All (Ada.Text_IO);
package body Pkg1 is

   A : String := "I am KO";
   function Execute return String is
   begin
      Ada.Text_IO.Put_Line (Pkg2.Foo);
      return A;
   end;

begin
   A := "I am OK";
end Pkg1;

