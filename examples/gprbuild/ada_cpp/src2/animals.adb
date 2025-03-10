with Ada.Text_IO; use Ada.Text_IO;
package body Animals is
   function Vaccination_Expired (A : Vaccinated_Dog) return Boolean is
   begin
      Put_Line (" In Ada: Vaccination_Expired");
      return False;
   end Vaccination_Expired;
end Animals;
