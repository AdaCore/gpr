with p0_0; use p0_0;
with p1_3; use p1_3;
package body p0_3 is
   function p0_3_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_0_0 (Item - 1)) + Long_Long_Integer (p1_3_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_3_0;
   function p0_3_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_0_1 (Item - 1)) + Long_Long_Integer (p1_3_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_3_1;
   function p0_3_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_0_2 (Item - 1)) + Long_Long_Integer (p1_3_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_3_2;
end p0_3;
