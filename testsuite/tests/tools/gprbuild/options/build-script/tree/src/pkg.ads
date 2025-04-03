package Pkg is

   procedure Hello;
   pragma Import (C, Hello, "hello");

end Pkg;