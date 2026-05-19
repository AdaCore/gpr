
with Ada.Text_IO;
package body Check is

  procedure O1 (X : Integer) is
  begin
    Ada.Text_IO.Put_Line ("Called Integer version " & X'img);
  end O1;
  procedure O1 (X : String) is
  begin
    Ada.Text_IO.Put_Line ("Called String version " & X);
  end O1;
  procedure O1 (X : Float) is
  begin
    raise Constraint_Error with "Float version";
  end O1;

  pragma No_Return (O1);

  procedure Do_Test is
  begin
    O1 (1);
    O1 ("Bla");
    O1 (2.0);
  end Do_Test;
end Check;
