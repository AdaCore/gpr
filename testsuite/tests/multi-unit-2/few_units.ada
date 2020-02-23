package U1 is
   procedure P;
end U1;

with U2;
package body U1 is
   procedure P is
   begin
      U2.P;
   end P;
end U1;

package U2 is
   procedure P;
end U2;

with U3;
package body U2 is
   procedure P is
   begin
      U3.P;
   end P;
end U2;

package U3 is
   procedure P;
end U3;

with GNAT.IO;
package body U3 is
   procedure P is
   begin
      GNAT.IO.Put_Line ("demo output");
   end P;
end U3;

