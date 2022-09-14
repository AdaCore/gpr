with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body InDash is

   -------------
   -- Display --
   -------------

   procedure Display (This : access Instrument) is
   begin
      New_Line;
      Put (Head (To_String (This.Name), 13));
      Put (" : ");
   end Display;

   ----------
   -- Name --
   ----------

   function Name (This : access Instrument) return String is
   begin
      return To_String (This.Name);
   end Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (This : access Instrument; To : String) is
   begin
      This.Name := To_Unbounded_String (To);
   end Set_Name;

end InDash;
