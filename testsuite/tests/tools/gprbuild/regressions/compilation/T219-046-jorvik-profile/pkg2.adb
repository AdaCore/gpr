with Ada.Calendar; -- ok for Jorvik, not for Ravenscar
package body Pkg2 is
   procedure Proc is
   begin
      delay 1.0; -- ok for Jorvik, not for Ravenscar

      select -- not ok for either
         delay 1.0;
      then abort
         null;
      end select;
   end Proc;
end Pkg2;
