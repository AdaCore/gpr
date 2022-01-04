------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;

package body GPR2.Source_Info.Parser is

   -------------
   -- To_Time --
   -------------

   function To_Time (S : Time_String) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Formatting.Time_Of
        (Year    => Year_Number'Value (S (1 .. 4)),
         Month   => Month_Number'Value (S (5 .. 6)),
         Day     => Day_Number'Value (S (7 .. 8)),
         Seconds => Day_Duration
           (3600 * Natural'Value (S (9 .. 10)) +
              60 * Natural'Value (S (11 .. 12)) +
              Natural'Value (S (13 .. 14))));
   end To_Time;

end GPR2.Source_Info.Parser;
