package Pkg is
   --  Declaring a subprogram makes the package require a body, so the compiler
   --  refuses to generate code for the spec alone. That refusal is what lets
   --  the test observe which source (spec vs body) was compiled.
   procedure Hello;
end Pkg;
