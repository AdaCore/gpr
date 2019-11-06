------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------


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
