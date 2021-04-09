with p3_3; use p3_3;
package body p3_2 is
   function p3_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_3_0 (Item - 1)) + 320;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_2_0;
   function p3_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_3_1 (Item - 1)) + 321;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_2_1;
   function p3_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p3_3_2 (Item - 1)) + 322;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p3_2_2;
end p3_2;
