with GNAT.IO;  use GNAT.IO;
with Test1;
with Test;
procedure main is
begin
   Put_Line (Test1.Msg & Test.Msg);
end main;
