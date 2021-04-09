with p0_3; use p0_3;
with p1_2; use p1_2;
package body p0_2 is
   function p0_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_3_0 (Item - 1)) + Long_Long_Integer (p1_2_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_2_0;
   function p0_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_3_1 (Item - 1)) + Long_Long_Integer (p1_2_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_2_1;
   function p0_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_3_2 (Item - 1)) + Long_Long_Integer (p1_2_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_2_2;
end p0_2;
