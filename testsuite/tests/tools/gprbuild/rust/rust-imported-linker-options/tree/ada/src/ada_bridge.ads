package Ada_Bridge is
   function Result return Integer;
   pragma Export (C, Result, "ada_result");
end Ada_Bridge;
