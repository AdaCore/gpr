------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

--  Representation of Time Stamps

with Ada.Calendar.Time_Zones;

with GNAT.Calendar.Time_IO;

package body GPR2.Time_Stamp is

   use GNAT.Calendar.Time_IO;

   ---------------
   -- From_Time --
   ---------------

   function From_Time
     (Time : Calendar.Time) return Data is
   begin
      return Data (Image (Time, "%Y%m%d%H%M%S"));
   end From_Time;

   --------------
   -- UTC_Time --
   --------------

   function UTC_Time return Data is
      use type Ada.Calendar.Time;

      Now : constant Calendar.Time :=
              Calendar.Clock
                - Duration (Calendar.Time_Zones.UTC_Time_Offset) * 60;
      --  The UTC_Time_Offset is in minutes
   begin
      return Data (Image (Now, "%Y%m%d%H%M%S"));
   end UTC_Time;

end GPR2.Time_Stamp;
