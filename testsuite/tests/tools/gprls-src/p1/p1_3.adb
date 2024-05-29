with p1_0; use p1_0;
with p2_3; use p2_3;
package body p1_3 is
   function p1_3_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_0_0 (Item - 1)) + Long_Long_Integer (p2_3_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_3_0;
   function p1_3_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_0_1 (Item - 1)) + Long_Long_Integer (p2_3_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_3_1;
   function p1_3_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_0_2 (Item - 1)) + Long_Long_Integer (p2_3_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_3_2;
end p1_3;
