package body Gen2 is

   I : Item;

   procedure Push (X : Item) is
   begin
      I := X;
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop return Item is
   begin
      return I;
   end Pop;

end Gen2;
