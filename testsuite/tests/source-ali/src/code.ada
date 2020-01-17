package U1 is
   A : Integer := 1;
end U1;

package U2 is
   B : Integer := 2;
   procedure Increment;
   procedure Decrement with Inline_Always;
end U2;

package body U2 is
   procedure Increment is
   begin
      B := B + 1;
   end;
   procedure Decrement is
   begin
      B := B - 1;
   end;
end U2;

with U1;
procedure U3 is
begin
   U1.A := U1.A + 1;
end U3;
