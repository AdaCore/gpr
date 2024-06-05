with p5_3; use p5_3;
with p6_2; use p6_2;
package body p5_2 is
   function p5_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_3_0 (Item - 1)) + Long_Long_Integer (p6_2_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_2_0;
   function p5_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_3_1 (Item - 1)) + Long_Long_Integer (p6_2_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_2_1;
   function p5_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_3_2 (Item - 1)) + Long_Long_Integer (p6_2_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_2_2;
end p5_2;
