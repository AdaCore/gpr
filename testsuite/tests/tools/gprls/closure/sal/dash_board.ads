
with Ada.Containers.Vectors;

with InDash;

package Dash_Board is

   procedure Display;

   procedure Register (Device : InDash.Any_Instrument);

   procedure Unregister (Device : InDash.Any_Instrument);

   procedure Update (Millisec : Integer);

private

   use InDash;

   package Instruments is
     new Ada.Containers.Vectors (Positive, Any_Instrument);

   Registry : Instruments.Vector;

end Dash_Board;
