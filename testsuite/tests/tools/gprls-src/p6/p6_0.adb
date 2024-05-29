with p6_1; use p6_1;
with p7_0; use p7_0;
package body p6_0 is
   function p6_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_1_0 (Item - 1)) + Long_Long_Integer (p7_0_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_0_0;
   function p6_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_1_1 (Item - 1)) + Long_Long_Integer (p7_0_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_0_1;
   function p6_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p6_1_2 (Item - 1)) + Long_Long_Integer (p7_0_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p6_0_2;
end p6_0;
