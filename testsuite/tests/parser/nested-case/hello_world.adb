#if MORE="Yes" then
with Hi;
#else
with Ada.Text_IO;
#end if;

procedure Hello_World is
begin
#if MORE="Yes" then
   Hi.Put;
#else
   Ada.Text_IO.Put_Line ("hi there");
#end if;
end Hello_World;
