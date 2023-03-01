with Gpr_Parser_AdaSAT.Formulas;

--  Provide various helpers for using the Gpr_Parser_AdaSAT library

package Gpr_Parser_AdaSAT.Helpers is
   use Gpr_Parser_AdaSAT.Formulas;

   function DPLL_Solve
     (F        : Formula;
      M        : in out Model;
      Min_Vars : Variable_Or_Null := 0) return Boolean;
   --  Solve the given Formula and provide a model for its variables.
   --  It is assumed that this is a pure SAT problem. This simply invokes
   --  the `Solve` function on an instantiation of the `Gpr_Parser_AdaSAT.DPLL` package
   --  with a theory that always returns True.

end Gpr_Parser_AdaSAT.Helpers;
