with p7_3; use p7_3;
package body p7_2 is
   function p7_2_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_3_0 (Item - 1)) + 720;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_2_0;
   function p7_2_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_3_1 (Item - 1)) + 721;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_2_1;
   function p7_2_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_3_2 (Item - 1)) + 722;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_2_2;
end p7_2;
