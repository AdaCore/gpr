package body Math_Lib is

   pragma Linker_Options ("extra_input.o");
   --  Not a library: an option like this one names an object file, a linker
   --  script or anything else the linker takes, and must be handed over as
   --  it stands.

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
