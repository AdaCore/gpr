with p1_3; use p1_3;
with p2_2; use p2_2;
package body p1_2 is
   function p1_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_3_0 (Item - 1)) + Long_Long_Integer (p2_2_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_2_0;
   function p1_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_3_1 (Item - 1)) + Long_Long_Integer (p2_2_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_2_1;
   function p1_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_3_2 (Item - 1)) + Long_Long_Integer (p2_2_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_2_2;
end p1_2;
