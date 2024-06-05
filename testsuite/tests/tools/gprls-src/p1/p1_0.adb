with p1_1; use p1_1;
with p2_0; use p2_0;
package body p1_0 is
   function p1_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_1_0 (Item - 1)) + Long_Long_Integer (p2_0_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_0_0;
   function p1_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_1_1 (Item - 1)) + Long_Long_Integer (p2_0_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_0_1;
   function p1_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p1_1_2 (Item - 1)) + Long_Long_Integer (p2_0_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p1_0_2;
end p1_0;
