package Counters is
   type Counter is record
      Value : Integer := 0;
   end record;

   procedure Bump (C : in out Counter);
   procedure Reset (C : in out Counter);
end Counters;