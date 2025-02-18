with Ada.Text_IO;
with Pkg2;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
package body Pkg1 is

   --  Ada.Text_IO already adds a libgnat dependency, but its symbol is also
   --  used by libgnat itself, so we can not produce an "undefined symbol"
   --  even if the link order of libs is wrong. So, we add another libgnat
   --  dependency that is not used by libgnat internally.

   function Function_To_Add_A_Libgnat_Dep (Date : Time) return Year_Number is
   begin
	   return Ada.Calendar.Formatting.Year (Date);
   end Function_To_Add_A_Libgnat_Dep;

   procedure P is
   begin
      Ada.Text_IO.Put_Line ("Hello from lib1");
      Pkg2.P;
   end P;

end Pkg1;