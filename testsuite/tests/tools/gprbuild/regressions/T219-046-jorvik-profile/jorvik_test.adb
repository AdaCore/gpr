pragma Profile (Jorvik);
with Ada.Calendar; -- ok for Jorvik, not for Ravenscar
procedure Jorvik_Test is
begin
   delay 1.0; -- ok for Jorvik, not for Ravenscar
   select  -- not ok for either
      delay 1.0;
   then abort
      null;
   end select;
end Jorvik_Test;
