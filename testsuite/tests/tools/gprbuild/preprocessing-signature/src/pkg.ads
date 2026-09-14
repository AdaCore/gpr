package Pkg is

   #if LEVEL then
   Level : constant Integer := 2;
   #else
   Level : constant Integer := 1;
   #end if;

end Pkg;
