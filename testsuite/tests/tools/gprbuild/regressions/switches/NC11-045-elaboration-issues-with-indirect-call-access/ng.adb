

with Ada.Command_Line;
package body Ng is
  N : Natural;
  function Image (Value : in Integer) return String is
  begin
    return Integer'Image (Value + N);
  end;
begin
  if Ada.Command_Line.Argument_Count < 1000 then
    N := 100;
  end if;
end Ng;
