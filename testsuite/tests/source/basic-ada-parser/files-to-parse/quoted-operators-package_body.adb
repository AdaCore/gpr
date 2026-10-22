with Dependency;

package body Operators is
   function "and" (L, R : Natural) return Natural is
   begin
      return Natural'Min (L, R);
   end "and";

   function "+" (L, R : Natural) return Natural is (L - R);

   function "not" (Value : Natural) return Natural is (Natural'Last - Value);
end Operators;
