package body Math_Lib is

   pragma Linker_Options ("-lm");
   --  libm is outside the project tree: only this pragma brings it into the
   --  link, and it gets there through the binder of this library.

   function C_Sqrt (X : double) return double
   with Import, Convention => C, External_Name => "sqrt";

   ----------
   -- Root --
   ----------

   function Root (X : double) return double is
   begin
      return C_Sqrt (X);
   end Root;

end Math_Lib;
