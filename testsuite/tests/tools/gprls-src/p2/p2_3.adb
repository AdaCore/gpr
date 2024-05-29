with p2_0; use p2_0;
with p3_3; use p3_3;
package body p2_3 is
   function p2_3_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_0_0 (Item - 1)) + Long_Long_Integer (p3_3_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_3_0;
   function p2_3_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_0_1 (Item - 1)) + Long_Long_Integer (p3_3_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_3_1;
   function p2_3_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_0_2 (Item - 1)) + Long_Long_Integer (p3_3_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_3_2;
end p2_3;
