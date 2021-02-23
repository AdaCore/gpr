separate (Pkg1)
procedure Sep is
begin
   null;
end Sep;

with U1;

separate (Pkg2)
procedure Sep is
begin
   U1.P;
end Sep;
