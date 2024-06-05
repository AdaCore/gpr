with p7_0; use p7_0;
package body p7_3 is
   function p7_3_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_0_0 (Item - 1)) + 730;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_3_0;
   function p7_3_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_0_1 (Item - 1)) + 731;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_3_1;
   function p7_3_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_0_2 (Item - 1)) + 732;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_3_2;
end p7_3;
