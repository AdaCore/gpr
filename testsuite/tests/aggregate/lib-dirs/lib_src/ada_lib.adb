with Ada.Text_IO; use Ada.Text_IO;
with Ada_lib2; use ada_lib2;
package body Ada_Lib is
   procedure overflo (S : integer; X : out character) is
      C : constant := 1_000 * 1024 * 1024;
      V : String (1 .. C);
   begin
      V (S) := '1';
      X := V (S);
   end;

   procedure Do_It_In_Ada is
   begin
      Put_Line ("Done in Ada");
      Do_It2;
   exception
      when others =>
         Put_Line ("exception raised and handled");
   end Do_It_In_Ada;

end Ada_Lib;
