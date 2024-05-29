with p2_1; use p2_1;
with p3_0; use p3_0;
package body p2_0 is
   function p2_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_1_0 (Item - 1)) + Long_Long_Integer (p3_0_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_0_0;
   function p2_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_1_1 (Item - 1)) + Long_Long_Integer (p3_0_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_0_1;
   function p2_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_1_2 (Item - 1)) + Long_Long_Integer (p3_0_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_0_2;
end p2_0;
