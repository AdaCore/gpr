with Interfaces.C.Strings; use Interfaces.C.Strings;
with Animals_Interfaces; use Animals_Interfaces;
package Animals is

   type Animal is tagged limited record
      Age : Natural;
   end record;
   pragma Import (CPP, Animal);
   --  Note that we are not allowed to initialize the record components
   --  since this is reponsibility of the constructor and it is imported
   --  from C++

   procedure Set_Age (X : in out Animal; Age : Natural);
   pragma Import (CPP, Set_Age);

   function Age (X : Animal) return Natural;
   pragma Import (CPP, Age);

   function New_Animal return Animal;
   pragma CPP_Constructor (New_Animal);
   pragma Import (CPP, New_Animal, "_ZN6AnimalC2Ev");
   --  We must import the constructor from C++ since all the primitives
   --  are defined in C++ (and hence the C++ constructor is responsible
   --  of building the dispatch tables).

   --  -----------------------------------------------------------------------
   type Dog is new Animal and Carnivore and Domestic with record
      Tooth_Count : Natural;
      Owner       : String (1 .. 30);
   end record;
   pragma Import (CPP, Dog);

   function Number_Of_Teeth (A : Dog) return Natural;
   pragma Import (CPP, Number_Of_Teeth);

   procedure Set_Owner (A : in out Dog; Name : Chars_Ptr);
   pragma Import (CPP, Set_Owner);

   function New_Dog return Dog'Class;
   pragma CPP_Constructor (New_Dog);
   pragma Import (CPP, New_Dog, "_ZN3DogC2Ev");

   --  -----------------------------------------------------------------------
   --  Example of a type derivation defined in the Ada side that inherites
   --  all the dispatching primitives of the ancestor from the C++ side.

   type Vaccinated_Dog is new Dog with null record;
   function Vaccination_Expired (A : Vaccinated_Dog) return Boolean;
   pragma Convention (CPP, Vaccination_Expired);
end Animals;
