--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
