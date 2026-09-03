with Ada.Calendar;
procedure Main is
   function Test return Boolean is (True);
   X : access function return Boolean := Test'Access;
begin
   null;
end Main;
