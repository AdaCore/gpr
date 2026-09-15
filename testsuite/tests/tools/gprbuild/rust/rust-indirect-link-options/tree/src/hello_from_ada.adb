with Ada.Text_IO; use Ada.Text_IO;
with Wrapper;

procedure Hello_From_Ada is
begin
   Put_Line ("Hello from Ada!");
   Wrapper.Greet;
end Hello_From_Ada;
