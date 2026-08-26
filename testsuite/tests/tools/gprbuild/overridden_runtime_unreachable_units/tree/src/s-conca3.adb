------------------------------------------------------------------------------
-- Copy of the genuine s-conca3.ads                                         --
------------------------------------------------------------------------------

with Ada.Text_IO;

package body System.Concat_3 is

   pragma Suppress (All_Checks);

   ------------------
   -- Str_Concat_3 --
   ------------------

   procedure Str_Concat_3 (R : out String; S1, S2, S3 : String) is
      F, L : Natural;

   begin
      Ada.Text_IO.Put_Line ("USING LOCAL OVERRIDE");
      F := R'First;
      L := F + S1'Length - 1;
      R (F .. L) := S1;

      F := L + 1;
      L := F + S2'Length - 1;
      R (F .. L) := S2;

      F := L + 1;
      L := F + S3'Length - 1;
      R (F .. L) := S3;
   end Str_Concat_3;

end System.Concat_3;
