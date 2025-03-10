with Ada.Text_IO;          use Ada.Text_IO;
with Animals;              use Animals;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Animals_Interfaces;   use Animals_Interfaces;

procedure Main is
   procedure Check_Carnivore (Obj : Carnivore'Class) is
      Aux : Natural;
   begin
      Put_Line ("Class wide calls to Carnivore ......");
      Aux := Obj.Number_Of_Teeth;
   end Check_Carnivore;

   procedure Check_Domestic (Obj : in out Domestic'Class) is
   begin
      Put_Line ("Class wide calls to Domestic ......");
      Obj.Set_Owner (Null_Ptr);
   end Check_Domestic;

   procedure Check_Vaccinated_Dog (Obj : in out Vaccinated_Dog'Class) is
      Aux_1 : Natural;
      Aux_2 : Boolean;

   begin
      Put_Line ("Class wide calls to Vaccinated_Dog ......");

      --  Call the inherited primitives (Age, Set_Age)

      Obj.Set_Age (10);
      if Obj.Age /= 10 then
         raise Program_Error;
      end if;

      --  Call the primitives that override abstract interfaces

      Aux_1 := Obj.Number_Of_Teeth;               --  Object.Opration notation (AI-252)
      Obj.Set_Owner (New_String ("Owner's name"));
      Aux_2 := Obj.Vaccination_Expired;
   end Check_Vaccinated_Dog;

   My_Pet : Vaccinated_Dog;  --  Constructor in the C++ side
begin
   Check_Carnivore      (My_Pet);  --  Check secondary DT
   Check_Domestic       (My_Pet);  --  Check secondary DT
   Check_Vaccinated_Dog (My_Pet);  --  Check primary DT
end Main;
