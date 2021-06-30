with p7_1; use p7_1;
package body p7_0 is
   function p7_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_1_0 (Item - 1)) + 700;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_0_0;
   function p7_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_1_1 (Item - 1)) + 701;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_0_1;
   function p7_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_1_2 (Item - 1)) + 702;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_0_2;
end p7_0;
