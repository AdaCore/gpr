package Greeter is

   procedure Hello;
   --  Prints a fixed message. The point of this package is just to force
   --  a dependency on the Ada runtime (Ada.Text_IO), so that gnatbind emits
   --  the usual -lgnat/-lgnarl entries in the bind file's linker option
   --  list -- exactly the tokens whose handling this test is checking.

end Greeter;
