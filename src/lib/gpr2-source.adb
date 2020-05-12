------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Calendar.Conversions;
with Ada.Characters.Handling;
with Ada.Directories;

with Interfaces.C;

pragma Warnings
  (Off, """System.OS_Constants"" is an internal GNAT unit");
pragma Warnings
  (Off, "use of this unit is non-portable and version-dependent");
with System.OS_Constants;
pragma Warnings (On);

package body GPR2.Source is

   One_Second : constant Duration :=
                  Calendar.Conversions.To_Duration
                    (tv_sec => 1, tv_nsec => 0);

   function Get_ALI_Timestamp
     (File : GPR2.Path_Name.Object) return Calendar.Time
     with Pre => File.Is_Defined;
   --  Return Timestamp used in ALI file. On windows use first greater time
   --  with an even number of second.

   function Key (Self : Object) return Value_Type
     with Inline, Pre => Self.Is_Defined;
   --  Returns the key for Self, this is used to compare a source object

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Object) return Boolean is
   begin
      return Key (Left) < Key (Right);
   end "<";

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Object) return Boolean is
   begin
      if not Left.Path_Name.Is_Defined
        and then not Right.Path_Name.Is_Defined
      then
         return True;
      else
         return Left.Path_Name.Is_Defined = Right.Path_Name.Is_Defined
           and then Key (Left) = Key (Right);
      end if;
   end "=";

   ---------------------
   -- Check_Timestamp --
   ---------------------

   function Check_Timestamp (Self : Object) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return Self.Timestamp = Get_ALI_Timestamp (Self.Path_Name);
   end Check_Timestamp;

   ------------
   -- Create --
   ------------

   function Create
     (Filename : GPR2.Path_Name.Object;
      Language : Name_Type;
      Kind     : Unit.Library_Unit_Type) return Object is
   begin
      return Result : Object  do
         Result.Path_Name := Filename;
         Result.Timestamp := Get_ALI_Timestamp (Filename);
         Result.Language  := +String (Language);

         Set (Result, Kind);
      end return;
   end Create;

   ----------------
   -- Create_Ada --
   ----------------

   function Create_Ada
     (Filename      : GPR2.Path_Name.Object;
      Units         : Unit.List.Object;
      Is_RTS_Source : Boolean;
      Is_Indexed    : Boolean) return Object
   is
      use all type Unit.Library_Unit_Type;

      Key          : Unbounded_String;
      Sorted_Units : Unit.List.Object :=
                       Unit.List.List.To_Vector (Unit.Undefined, Units.Length);
   begin
      for CU of Units loop
         if CU.Index > Positive (Units.Length) then
            raise Project_Error with "Unit index overflow";
         end if;

         if Sorted_Units (CU.Index).Is_Defined then
            raise Project_Error with "Unit index duplication";
         end if;

         Sorted_Units (CU.Index) := CU;
      end loop;

      for CU of Sorted_Units loop
         Append
           (Key,
            Characters.Handling.To_Lower (String (CU.Name))
            & (if CU.Kind = S_Spec then 'S' else 'B'));
      end loop;

      return Result : Object do
         Result.Path_Name := Filename;
         Result.Timestamp := Get_ALI_Timestamp (Filename);
         Result.Language  := +"Ada";
         Result.Ada_Key   := Key;

         Set_Ada (Result, Sorted_Units, Is_RTS_Source, Is_Indexed);
      end return;
   end Create_Ada;

   -----------------------
   -- Get_ALI_Timestamp --
   -----------------------

   function Get_ALI_Timestamp
     (File : GPR2.Path_Name.Object) return Ada.Calendar.Time
   is
      use type Calendar.Time;

      Timestamp : Calendar.Time :=
                    Directories.Modification_Time (File.Value);
      use System.OS_Constants;
   begin
      pragma Warnings (Off, "*this code can never be executed*");

      if Target_OS = Windows then
         declare
            use type Interfaces.C.int;

            Year   : Interfaces.C.int;
            Month  : Interfaces.C.int;
            Day    : Interfaces.C.int;
            Hour   : Interfaces.C.int;
            Minute : Interfaces.C.int;
            Second : Interfaces.C.int;
         begin
            Calendar.Conversions.To_Struct_Tm
              (T       => Timestamp,
               tm_year => Year,
               tm_mon  => Month,
               tm_day  => Day,
               tm_hour => Hour,
               tm_min  => Minute,
               tm_sec  => Second);

            if Second mod 2 > 0 then
               Timestamp := Timestamp + One_Second;
            end if;
         end;
      end if;

      pragma Warnings (On, "*this code can never be executed*");

      return Timestamp;
   end Get_ALI_Timestamp;

   ---------
   -- Key --
   ---------

   function Key (Self : Object) return Value_Type is
   begin
      if Self.Is_Ada then
         --  In this case, the relevant information is unit name + unit kind
         return To_String (Self.Ada_Key);

      else
         --  Not unit based: just use the full path
         return Self.Path_Name.Value;
      end if;
   end Key;

end GPR2.Source;
