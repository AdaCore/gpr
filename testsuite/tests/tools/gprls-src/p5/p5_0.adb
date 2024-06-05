with p5_1; use p5_1;
with p6_0; use p6_0;
package body p5_0 is
   function p5_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_1_0 (Item - 1)) + Long_Long_Integer (p6_0_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_0_0;
   function p5_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_1_1 (Item - 1)) + Long_Long_Integer (p6_0_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_0_1;
   function p5_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p5_1_2 (Item - 1)) + Long_Long_Integer (p6_0_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p5_0_2;
end p5_0;
