with Ada.Text_IO; use Ada.Text_IO;

procedure Hello_From_Ada is
   procedure Greet_From_Rust with Import, Convention => C;
begin
   Put_Line ("Hello from Ada!");
   Greet_From_Rust;
end Hello_From_Ada;
