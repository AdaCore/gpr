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
