with U4;
separate (Pkg1)
procedure Sep is
begin
   if U4.Dummy > 0 then
      raise Program_Error;
   end if;
end Sep;

with U1;

with Pkg;
separate (Pkg2)
procedure Sep is
begin
   U1.P;
   Pkg.Var := Pkg.Var + 1;
end Sep;
