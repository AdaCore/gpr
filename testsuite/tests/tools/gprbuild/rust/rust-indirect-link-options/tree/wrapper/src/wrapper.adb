package body Wrapper is

   procedure Hello_From_Rust with Import, Convention => C;

   -----------
   -- Greet --
   -----------

   procedure Greet is
   begin
      Hello_From_Rust;
   end Greet;

end Wrapper;
