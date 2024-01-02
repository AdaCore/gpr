package Foo is
   function F return String;
end Foo;

package body Foo is
   function F return String is
   begin
      return "Hi all!";
   end F;
end Foo;