package body AAA is

   procedure AAAA;
   procedure AAA;
   procedure AA;

   procedure Proc is
   begin
      AA;
      AAA;
      AAAA;
   end Proc;

   procedure AAAA is separate;

   procedure AAA is separate;

   procedure AA is separate;

end AAA;
