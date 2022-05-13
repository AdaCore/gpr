package body Pkg is

   function "+" (T1, T2 : T) return T is
   begin
      return T (Integer (T1) + Integer (T2));
   end "+";

   procedure P2 is separate;

   procedure P is
   begin
      P2;
   end P;

end Pkg;
