------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2022, AdaCore                      --
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

with Ada.Calendar.Conversions;

with Interfaces.C;

package body GPR2.Source is

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

   ------------
   -- Create --
   ------------

   function Create
     (Filename : GPR2.Path_Name.Object;
      Language : Language_Id;
      Kind     : GPR2.Unit.Library_Unit_Type) return Object'Class is
   begin
      return Result : Object  do
         Result.Path_Name := Filename;
         Result.Timestamp := Filename.Modification_Time;

         Set_Non_Ada (Result, Language, Kind);
      end return;
   end Create;

   ----------------
   -- Create_Ada --
   ----------------

   function Create_Ada
     (Filename      : GPR2.Path_Name.Object;
      Units         : GPR2.Unit.List.Object) return Object'Class
   is
      use all type GPR2.Unit.Library_Unit_Type;

      Key          : Unbounded_String;
   begin
      for CU of Units loop
         Append
           (Key,
            To_Lower (CU.Name)
            & (if CU.Kind in GPR2.Unit.Spec_Kind then 'S' else 'B'));
      end loop;

      return Result : Object do
         Result.Path_Name := Filename;
         Result.Timestamp := Filename.Modification_Time;
         Result.Ada_Key   := Key;

         Set_Ada (Result, Units);
      end return;
   end Create_Ada;

   function Create_Ada
     (Filename      : GPR2.Path_Name.Object;
      Unit          : GPR2.Unit.Object;
      Is_RTS_Source : Boolean) return Object'Class  is
   begin
      pragma Assert (Unit.Index = No_Index);
      return Result : Object do
         Result.Path_Name := Filename;
         Result.Timestamp := Filename.Modification_Time;
         Result.Ada_Key   := +(To_Lower (Unit.Name)
                               & (if Unit.Kind in GPR2.Unit.Spec_Kind
                                  then 'S' else 'B'));

         Set_Ada (Result, Unit, Is_RTS_Source);
      end return;
   end Create_Ada;

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

   ----------------------
   -- To_ALI_Timestamp --
   ----------------------

   function To_ALI_Timestamp (Stamp : Calendar.Time) return Calendar.Time is
      use Interfaces;
      use Ada.Calendar;
      use type C.long;

      Timestamp : constant C.long := Conversions.To_Unix_Time (Stamp);

   begin
      return Conversions.To_Ada_Time
        (Timestamp
         + (if On_Windows and then Timestamp mod 2 > 0
            then 1
            else 0));
   end To_ALI_Timestamp;

end GPR2.Source;
