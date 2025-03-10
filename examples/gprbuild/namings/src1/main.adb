with Util;
with Ada.Text_IO; use Ada.TExt_IO;
procedure Main is
   A : aliased integer := 2;
   procedure C_Routine (x : in out Integer);
   pragma Import (C, C_Routine);

begin
   Put ("src1/");
   C_Routine (A);
   Util.Print (A);
end;
