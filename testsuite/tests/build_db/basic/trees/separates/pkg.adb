package body Pkg is

   package Pack is
      procedure P;
   end Pack;

   procedure P is separate;

   package body Pack is separate;

end Pkg;
