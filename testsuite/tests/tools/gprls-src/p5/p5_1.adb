with p5_2; use p5_2;
with p6_1; use p6_1;
package body p5_1 is
   function p5_1_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_2_0 (Item - 1)) + Long_Long_Integer (p6_1_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_1_0;
   function p5_1_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_2_1 (Item - 1)) + Long_Long_Integer (p6_1_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_1_1;
   function p5_1_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_2_2 (Item - 1)) + Long_Long_Integer (p6_1_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_1_2;
end p5_1;
