with Ada.Strings.Unbounded;

package InDash is

   type Instrument is abstract tagged private;

   type Instrument_Reference is access all Instrument;

   type Any_Instrument is access all Instrument'Class;

   function Name (This : access Instrument) return String;

   procedure Set_Name (This : access Instrument; To : String);

   procedure Display (This : access Instrument);

   procedure Update  (This : access Instrument; Millisec : Integer) is abstract;
   --  Update the state of the instrument after millisec has lapsed

private

   use Ada.Strings.Unbounded;

   type Instrument is abstract tagged record
      Name : Unbounded_String;
   end record;

end InDash;
