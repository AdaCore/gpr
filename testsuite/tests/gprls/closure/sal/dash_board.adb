
with Ada.Text_IO;

package body Dash_Board is

   --------------
   -- Register --
   --------------

   procedure Register (Device : InDash.Any_Instrument) is
   begin
      Registry.Append (Device);
   end Register;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister (Device : InDash.Any_Instrument) is
   begin
      Registry.Delete (Registry.Find_Index (Device));
   end Unregister;

   use Instruments;

   -------------
   -- Display --
   -------------

   procedure Display is
      C : Cursor := First (Registry);
   begin
      while C /= No_Element loop
         Element (C).Display; -- dispatches
         Next (C);
      end loop;
      Ada.Text_IO.New_Line;
   end Display;

   ------------
   -- Update --
   ------------

   procedure Update (Millisec : Integer) is
      C : Cursor := First (Registry);
   begin
      while C /= No_Element loop
         Element (C).Update (Millisec); -- dispatches
         Next (C);
      end loop;
   end Update;

end Dash_Board;
