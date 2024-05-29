with p8_3; use p8_3;
package body p8_2 is
   function p8_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_3_0 (Item - 1)) + 820;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_2_0;
   function p8_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_3_1 (Item - 1)) + 821;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_2_1;
   function p8_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_3_2 (Item - 1)) + 822;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_2_2;
end p8_2;
