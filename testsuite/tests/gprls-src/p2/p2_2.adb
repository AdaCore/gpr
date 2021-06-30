with p2_3; use p2_3;
with p3_2; use p3_2;
package body p2_2 is
   function p2_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_3_0 (Item - 1)) + Long_Long_Integer (p3_2_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_2_0;
   function p2_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_3_1 (Item - 1)) + Long_Long_Integer (p3_2_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_2_1;
   function p2_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p2_3_2 (Item - 1)) + Long_Long_Integer (p3_2_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p2_2_2;
end p2_2;
