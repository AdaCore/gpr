with Interfaces.C; use Interfaces.C;

package Math_Lib is
   function Add (A, B : int) return int;
   pragma Export (C, Add, "ada_add");
end Math_Lib;
