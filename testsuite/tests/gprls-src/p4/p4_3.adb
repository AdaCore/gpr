with p4_0; use p4_0;
with p5_3; use p5_3;
package body p4_3 is
   function p4_3_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_0_0 (Item - 1)) + Long_Long_Integer (p5_3_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_3_0;
   function p4_3_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_0_1 (Item - 1)) + Long_Long_Integer (p5_3_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_3_1;
   function p4_3_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_0_2 (Item - 1)) + Long_Long_Integer (p5_3_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_3_2;
end p4_3;
