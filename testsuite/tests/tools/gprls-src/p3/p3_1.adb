with p3_2; use p3_2;
package body p3_1 is
   function p3_1_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_2_0 (Item - 1)) + 310;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_1_0;
   function p3_1_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_2_1 (Item - 1)) + 311;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_1_1;
   function p3_1_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_2_2 (Item - 1)) + 312;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_1_2;
end p3_1;
