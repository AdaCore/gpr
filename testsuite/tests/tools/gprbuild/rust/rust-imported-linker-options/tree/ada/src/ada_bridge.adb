package body Ada_Bridge is
   function Native_Value return Integer
     with Import, Convention => C, External_Name => "linker_option_probe";

   function Result return Integer is (Native_Value + 1);
end Ada_Bridge;
