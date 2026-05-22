
with Text_IO; use Text_IO;
package body Gen_G is

  procedure P is
  begin
    Put_Line ("p procedure " & Image.all (1));
  end;
begin
  Put_Line ("elaboration " & Image.all (2));
end;
