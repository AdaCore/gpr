package body Worker is

   protected body Guard is

      procedure Signal is
      begin
         Flag := True;
      end Signal;

      entry Wait when Flag is
      begin
         null;
      end Wait;

   end Guard;

end Worker;
