package Foo is

   procedure In_Ada;
   pragma Export (C, In_Ada, "in_ada");

end Foo;