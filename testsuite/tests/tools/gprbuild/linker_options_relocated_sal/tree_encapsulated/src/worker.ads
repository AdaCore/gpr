package Worker is

   protected Guard is
      procedure Signal;
      entry Wait;
   private
      Flag : Boolean := False;
   end Guard;
   --  A protected object with a blocking entry is enough to pull in
   --  System.Tasking and force gnatbind to emit -lgnarl, unlike the plain
   --  Greeter fixture which has no tasking dependency at all.

end Worker;
