with Interfaces.C; use Interfaces.C;

package Math_Lib is
   function Compute (A, B : int) return int;
   pragma Export (C, Compute, "ada_compute");
end Math_Lib;
