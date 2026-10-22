with Interfaces.C;
package Pkg is
   function Value return Interfaces.C.int;
   pragma Export (C, Value, "library_value");
end Pkg;
