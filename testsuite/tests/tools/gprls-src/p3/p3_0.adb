with p3_1; use p3_1;
package body p3_0 is
   function p3_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_1_0 (Item - 1)) + 300;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_0_0;
   function p3_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_1_1 (Item - 1)) + 301;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_0_1;
   function p3_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_1_2 (Item - 1)) + 302;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_0_2;
end p3_0;
