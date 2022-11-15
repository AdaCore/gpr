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

with Ada.Calendar;

package GPR2.Time_Stamp is

   use Ada;

   --  All compiled units are marked with a time stamp which is derived from
   --  the source file (we assume that the host system has the concept of a
   --  file time stamp which is modified when a file is modified). These
   --  time stamps are used to ensure consistency of the set of units that
   --  constitutes a library. Time stamps are 14-character strings with
   --  with the following format:

   --     YYYYMMDDHHMMSS

   --       YYYY   year
   --       MM     month (2 digits 01-12)
   --       DD     day (2 digits 01-31)
   --       HH     hour (2 digits 00-23)
   --       MM     minutes (2 digits 00-59)
   --       SS     seconds (2 digits 00-59)

   --  In the case of Unix systems (and other systems which keep the time in
   --  GMT), the time stamp is the GMT time of the file, not the local time.
   --  This solves problems in using libraries across networks with clients
   --  spread across multiple time-zones.

   Length : constant := 14;
   --  Length of time stamp value

   subtype Index is Natural range 1 .. Length;
   type Data is new String (Index);
   --  Type used to represent time stamp

   Empty : constant Data := (others => ' ');
   --  Value representing an empty or missing time stamp. Looks less than
   --  any real time stamp if two time stamps are compared. Note that
   --  although this is not private, clients should not rely on the exact
   --  way in which this string is represented, and instead should use the
   --  subprograms below.

   Dummy : constant Data := (others => '0');
   --  This is used for dummy time stamp values used in the D lines for
   --  non-existent files, and is intended to be an impossible value.

   function From_Time (Time : Calendar.Time) return Data;
   --  Returns Time as a time stamp type

   function UTC_Time return Data;
   --  Returns the UTC time

end GPR2.Time_Stamp;
