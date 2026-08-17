with Ada.Text_IO; use Ada.Text_IO;
with Worker;

procedure Main_Encapsulated is
begin
   Worker.Guard.Signal;
   Worker.Guard.Wait;
   Put_Line ("hello from relocated SAL");
end Main_Encapsulated;
