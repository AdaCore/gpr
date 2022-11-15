separate (Pkg)
package body Pack is

   package Sub is
      procedure P;
   end Sub;

   procedure P is
   begin
      Sub.P;
   end P;

   package body Sub is separate;

end Pack;
