with Interfaces.C; use Interfaces.C;

package Math_Lib is
   function Root (X : double) return double;
   pragma Export (C, Root, "ada_root");
end Math_Lib;
