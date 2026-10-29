package body Pkg is
   function Foreign_Value return Interfaces.C.int;
   pragma Import (C, Foreign_Value, "foreign_value");
   function Value return Interfaces.C.int is (Foreign_Value);
end Pkg;
