with Foo;
package body B is
   procedure B (Caller : String) is
   begin
      Foo.Foo ("B called from " & Caller);
   end B;
end B;
