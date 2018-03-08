--
--  Copyright (C) 2011-2012, AdaCore
--

package body Simple is

   function Inc (X : Integer) return Integer
   is
   begin
      return X + 1;
   end Inc;

--     function Dec (X : Integer) return Integer
--     is
--     begin
--        return X - 1;
--     end Dec;

--     procedure Interface is
--     begin
--        null;
--     end;

end Simple;
