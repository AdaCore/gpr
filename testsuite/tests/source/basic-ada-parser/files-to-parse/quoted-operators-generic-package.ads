with Dependency;

generic
   type Element is private;
   with function "AnD" (L, R : Element) return Element is <>;
   with function "+" (L, R : Element) return Element is <>;
   with function "not" (Value : Element) return Element is <>;
package Generic_Operators is
   function Combine (L, R : Element) return Element;
end Generic_Operators;
