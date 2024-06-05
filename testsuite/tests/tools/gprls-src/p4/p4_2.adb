with p4_3; use p4_3;
with p5_2; use p5_2;
package body p4_2 is
   function p4_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_3_0 (Item - 1)) + Long_Long_Integer (p5_2_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_2_0;
   function p4_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_3_1 (Item - 1)) + Long_Long_Integer (p5_2_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_2_1;
   function p4_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p4_3_2 (Item - 1)) + Long_Long_Integer (p5_2_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p4_2_2;
end p4_2;
