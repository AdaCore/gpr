--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This abstract object represents an object dependency parser.
--  Such parser can be defined for a specific language and kind (either parsed
--  from source or some artifact files created during the compilation like
--  ALI for GNAT or .d files for C/C++).
--
--  A source parser is created as a child package. It is then
--  registered into the source info parser registry child package.

private with Ada.Calendar;
private with Ada.Calendar.Formatting;

package GPR2.Build.Object_Info.Parser is

   type Object (Kind : Backend) is abstract tagged limited private;

   type Object_Ref is not null access all Object'Class;

   procedure Compute
     (Self     : Object;
      Data     : in out Object_Info.Object'Class;
      Messages : in out Log.Object) is abstract
     with Pre'Class => Data.Is_Defined;
   --  Set Data with the information for its compilation unit.

private

   type Object (Kind : Backend) is abstract tagged limited null record;

   subtype Time_String is String (1 .. 14);

   function To_Time (S : Time_String) return Ada.Calendar.Time is
     (Ada.Calendar.Formatting.Time_Of
        (Year    => Year_Number'Value (S (1 .. 4)),
         Month   => Month_Number'Value (S (5 .. 6)),
         Day     => Day_Number'Value (S (7 .. 8)),
         Seconds => Day_Duration
           (3600 * Natural'Value (S (9 .. 10)) +
              60 * Natural'Value (S (11 .. 12)) +
              Natural'Value (S (13 .. 14)))));
   --  Convert timestamp in YYYYMMDDHHMMSS format to Time

end GPR2.Build.Object_Info.Parser;
