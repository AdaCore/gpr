with p8_2; use p8_2;
package body p8_1 is
   function p8_1_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_2_0 (Item - 1)) + 810;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_1_0;
   function p8_1_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_2_1 (Item - 1)) + 811;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_1_1;
   function p8_1_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_2_2 (Item - 1)) + 812;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_1_2;
end p8_1;
