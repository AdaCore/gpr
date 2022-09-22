
with InDash;

package Gauge is

   type Percent is delta 0.01 digits 5 range 0.0 .. 100.0;

   type Numeric_Gauge is new InDash.Instrument with private;

   type Numeric_Gauge_Reference is access all Numeric_Gauge;

   procedure Display (G : access Numeric_Gauge);

   procedure Update  (G : access Numeric_Gauge; Millisec : Integer);

   type Graphic_Gauge is new Numeric_Gauge with private;

   type Graphic_Gauge_Reference is access all Graphic_Gauge;

   procedure Display (G : access Graphic_Gauge);

private

   type Numeric_Gauge is new InDash.Instrument with record
      Value : Percent;
      Rate  : Float;
   end record;

   type Graphic_Gauge is new Numeric_Gauge with record
      Size  : Integer;
      Fill  : Character;
      Empty : Character;
   end record;

end Gauge;
