with Helper;

package body Api is
   function Value return Integer is
   begin
      return Helper.Base + 1;
   end Value;
end Api;
