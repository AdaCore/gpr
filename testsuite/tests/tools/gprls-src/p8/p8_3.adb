with p8_0; use p8_0;
package body p8_3 is
   function p8_3_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_0_0 (Item - 1)) + 830;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_3_0;
   function p8_3_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_0_1 (Item - 1)) + 831;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_3_1;
   function p8_3_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_0_2 (Item - 1)) + 832;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_3_2;
end p8_3;
