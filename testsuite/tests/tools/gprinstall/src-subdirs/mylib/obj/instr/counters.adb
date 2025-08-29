package body Counters is

   procedure Bump (C : in out Counter) is
   begin
      C.Value := C.Value + 9;
   end Bump;

   procedure Reset (C : in out Counter) is
   begin
      C.Value := 0;
   end Reset;

end Counters;