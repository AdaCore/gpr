with p0_1; use p0_1;
with p1_0; use p1_0;
package body p0_0 is
   function p0_0_0 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_1_0 (Item - 1)) + Long_Long_Integer (p1_0_0 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_0_0;
   function p0_0_1 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_1_1 (Item - 1)) + Long_Long_Integer (p1_0_1 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_0_1;
   function p0_0_2 (Item : Integer) return Integer is
      Result : Long_Long_Integer;
   begin
      if Item < 0 then
         return -Item;
      end if;
      Result := Long_Long_Integer (p0_1_2 (Item - 1)) + Long_Long_Integer (p1_0_2 (Item - 2));
      return Integer (Result rem Long_Long_Integer (Integer'Last));
   end p0_0_2;
end p0_0;
