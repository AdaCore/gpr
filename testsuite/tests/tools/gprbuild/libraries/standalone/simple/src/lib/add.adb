with Foo;

package body Add is
   function Add (X, Y : Integer) return Integer is

      --  Empty task that creates a dependency toward libgnarl
      task T;
      task body T is
      begin
         null;
      end T;
   begin
      return X + Y;
   end Add;
end Add;

