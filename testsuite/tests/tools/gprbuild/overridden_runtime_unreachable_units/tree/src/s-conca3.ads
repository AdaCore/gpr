------------------------------------------------------------------------------
-- Copy of the genuine s-conca3.ads                                         --
------------------------------------------------------------------------------

package System.Concat_3 is

   procedure Str_Concat_3 (R : out String; S1, S2, S3 : String);
   --  Performs the operation R := S1 & S2 & S3. The bounds of R are known to
   --  be sufficient so no bound checks are required, and it is known that none
   --  of the input operands overlaps R. No assumptions can be made about the
   --  lower bounds of any of the operands.

end System.Concat_3;
