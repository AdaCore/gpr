with p4_1; use p4_1;
with p5_0; use p5_0;
package body p4_0 is
   function p4_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_1_0 (Item - 1)) + Long_Long_Integer (p5_0_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_0_0;
   function p4_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_1_1 (Item - 1)) + Long_Long_Integer (p5_0_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_0_1;
   function p4_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_1_2 (Item - 1)) + Long_Long_Integer (p5_0_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_0_2;
end p4_0;
