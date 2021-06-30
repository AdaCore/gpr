with p6_3; use p6_3;
with p7_2; use p7_2;
package body p6_2 is
   function p6_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_3_0 (Item - 1)) + Long_Long_Integer (p7_2_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_2_0;
   function p6_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_3_1 (Item - 1)) + Long_Long_Integer (p7_2_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_2_1;
   function p6_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_3_2 (Item - 1)) + Long_Long_Integer (p7_2_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_2_2;
end p6_2;
