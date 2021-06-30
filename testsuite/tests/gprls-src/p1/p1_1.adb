with p1_2; use p1_2;
with p2_1; use p2_1;
package body p1_1 is
   function p1_1_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_2_0 (Item - 1)) + Long_Long_Integer (p2_1_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_1_0;
   function p1_1_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_2_1 (Item - 1)) + Long_Long_Integer (p2_1_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_1_1;
   function p1_1_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_2_2 (Item - 1)) + Long_Long_Integer (p2_1_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_1_2;
end p1_1;
