separate (Pkg)
procedure Child is

   procedure Child2;

   procedure Child2 is separate;
begin
   Child2;
end Child;