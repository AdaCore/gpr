------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

   function To_Time (S : String) return Ada.Calendar.Time is

      function T2 (Shift : Positive) return String is
        (S (S'First + Shift .. S'First + Shift + 1)) with Inline;

   begin
      if S'Length /= Time_Stamp_Length then
         raise Constraint_Error with
           "Wrong timestamp """ & S & """ length" & S'Length'Img;
      end if;

      return Ada.Calendar.Formatting.Value
        (S (S'First .. S'First + 3) & "-" & T2 (4) & "-" & T2 (6)
         & " " & T2 (8) & ":" & T2 (10) & ":" & T2 (12));
   end To_Time;

end GPR2.Source_Info.Parser;
