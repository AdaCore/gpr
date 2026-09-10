package Hello is

   procedure Say_Hello;
   pragma Export (C, Say_Hello, "say_hello");

   procedure Say_Bye;
   pragma Export (C, Say_Bye, "say_bye");

end Hello;
