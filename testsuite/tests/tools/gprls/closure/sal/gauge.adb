
with Ada.Finalization; use Ada.Finalization;
with Ada.Text_IO;      use Ada.Text_IO;

with Dash_Board;

package body Gauge is

   use InDash;

   package Percent_IO is new Decimal_IO (Percent);
   use Percent_IO;

   type Life_Controller is new Limited_Controlled with null record;
   overriding procedure Initialize (LC : in out Life_Controller);
   overriding procedure Finalize (LC : in out Life_Controller);

   F, W : Any_Instrument;

   ------------------------
   -- Make_Numeric_Gauge --
   ------------------------

   function Make_Numeric_Gauge
     (Name : String;  Value : Percent;  Rate : Float) return Any_Instrument
   is
      Result : Numeric_Gauge_Reference;
   begin
      Result := new Numeric_Gauge;
      InDash.Any_Instrument(Result).Set_Name (Name);
      Result.Value := Value;
      Result.Rate  := Rate;
      return Any_Instrument (Result);
   end Make_Numeric_Gauge;

   ------------------------
   -- Make_Graphic_Gauge --
   ------------------------

   function Make_Graphic_Gauge
      (Name  : String;
       Value : Percent;
       Rate  : Float;
       Size  : Integer;
       Fill  : Character := '*';
       Empty : Character := ' ') return Any_Instrument
   is
      Result : Graphic_Gauge_Reference;
   begin
      Result := new Graphic_Gauge;
      InDash.Any_Instrument(Result).Set_Name (Name);
      Result.Value := Value;
      Result.Rate  := Rate;
      Result.Size  := Size;
      Result.Fill  := Fill;
      Result.Empty := Empty;
      return Any_Instrument (Result);
   end Make_Graphic_Gauge;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (LC : in out Life_Controller) is
   begin
      F := Make_Numeric_Gauge ("Fuel",  60.0, 1.0);
      Dash_Board.Register (F);

      W := Make_Graphic_Gauge ("Water", 80.0, 2.0, 20);
      Dash_Board.Register (W);

      --  O := Make_Graphic_Gauge ("Oil",   40.0, 2.0, 20);
      --  Dash_Board.Register (O);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (LC : in out Life_Controller) is
   begin
      Put_Line ("Unregister Gauge");
      Dash_Board.Unregister (F);
      Dash_Board.Unregister (W);
      --  Dash_Board.Unregister (O);
   end Finalize;

   -------------
   -- Display --
   -------------

   procedure Display (G : access Numeric_Gauge) is
   begin
      InDash.Instrument_Reference (G).Display;
      Put (G.Value, 3);
      Put (" %");
   end Display;

   ------------
   -- Update --
   ------------

   procedure Update (G : access Numeric_Gauge; Millisec : Integer) is
      Elapsed_Seconds : constant Float := Float(Millisec) / 1000.0;
      Elapsed_Minutes : constant Float := Elapsed_Seconds / 60.0;
   begin
      G.Value := G.Value - Percent(G.Rate * Elapsed_Minutes);
   end Update;

   -------------
   -- Display --
   -------------

   procedure Display (G : access Graphic_Gauge) is
      Lg : constant Integer := G.Size * Integer(G.Value) / 100;
      S1 : constant String (1 .. Lg) := (others => G.Fill);
      S2 : constant String (Lg + 1 .. G.Size) := (others => G.Empty);
   begin
      InDash.Instrument_Reference (G).Display;
      Put ('<');
      Put (S1);
      Put (S2);
      Put ('>');
   end Display;

   --  Declared at the end to ensure all routines are elaborated before
   --  calling them.

   LC : Life_Controller;

end Gauge;
