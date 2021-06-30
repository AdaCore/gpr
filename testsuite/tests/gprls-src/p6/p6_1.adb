with p6_2; use p6_2;
with p7_1; use p7_1;
package body p6_1 is
   function p6_1_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_2_0 (Item - 1)) + Long_Long_Integer (p7_1_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_1_0;
   function p6_1_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_2_1 (Item - 1)) + Long_Long_Integer (p7_1_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_1_1;
   function p6_1_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_2_2 (Item - 1)) + Long_Long_Integer (p7_1_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_1_2;
end p6_1;
