with p7_2; use p7_2;
package body p7_1 is
   function p7_1_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_2_0 (Item - 1)) + 710;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_1_0;
   function p7_1_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_2_1 (Item - 1)) + 711;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_1_1;
   function p7_1_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p7_2_2 (Item - 1)) + 712;
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p7_1_2;
end p7_1;
