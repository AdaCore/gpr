with Dependency;

generic
   type Element is private;
   with function "and" (L, R : Element) return Element is <>;
   with function "+" (L, R : Element) return Element is <>;
   with function "not" (Value : Element) return Element is <>;
function Apply_Operators (L, R : Element) return Element;
