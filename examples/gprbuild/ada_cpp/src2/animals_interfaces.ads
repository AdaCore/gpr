with Interfaces.C.Strings; use Interfaces.C.Strings;
package Animals_Interfaces is
   type Carnivore is limited interface;
   function Number_Of_Teeth (X : Carnivore) return Natural is abstract;
   pragma Convention (CPP, Number_Of_Teeth); --  Required by AI-430

   type Domestic is limited interface;
   procedure Set_Owner (X : in out Domestic; Name : Chars_Ptr) is abstract;
   pragma Convention (CPP, Set_Owner);
end;
