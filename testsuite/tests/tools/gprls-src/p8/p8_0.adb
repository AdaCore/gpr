with p8_1; use p8_1;
package body p8_0 is
   function p8_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_1_0 (Item - 1)) + 800;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_0_0;
   function p8_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_1_1 (Item - 1)) + 801;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_0_1;
   function p8_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p8_1_2 (Item - 1)) + 802;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p8_0_2;
end p8_0;
