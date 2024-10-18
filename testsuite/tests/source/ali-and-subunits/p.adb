with P.C;
with Q;
package body P is
   B1 : constant Boolean := P.C.G;
   B2 : constant Boolean := Q.f;
   function F return Boolean is separate;
end P;
