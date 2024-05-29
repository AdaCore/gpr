with p3_0; use p3_0;
package body p3_3 is
   function p3_3_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_0_0 (Item - 1)) + 330;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_3_0;
   function p3_3_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_0_1 (Item - 1)) + 331;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_3_1;
   function p3_3_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_0_2 (Item - 1)) + 332;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_3_2;
end p3_3;
