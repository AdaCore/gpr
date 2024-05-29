generic
   type Item is private;
package Gen2 is
   procedure Push (X : Item);
   function Pop return Item;
end Gen2;