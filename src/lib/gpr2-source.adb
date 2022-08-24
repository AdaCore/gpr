--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
      --  Allows comparing with Undefined constant. Note: we can't call
      --  Left.Is_Defined or Right.Is_Defined here, because the Is_Defined
      --  function uses the "=" operator.

      if Left.Path_Name.Is_Defined /= Right.Path_Name.Is_Defined then
         return False;
      end if;

      if not Left.Path_Name.Is_Defined then
         --  Both undefined
         return True;
      end if;

      if Left.Is_Ada then
         return Right.Is_Ada and then Left.Ada_Key = Right.Ada_Key;
      else
         return not Right.Is_Ada
           and then Filename_Optional (Left.Path_Name.Value) =
                      Filename_Optional (Right.Path_Name.Value);
      end if;
   end "=";

   ------------
   -- Create --
   ------------

   function Create
     (Filename  : GPR2.Path_Name.Object;
      Language  : Language_Id;
      Kind      : GPR2.Unit.Library_Unit_Type;
      Timestamp : Ada.Calendar.Time) return Object'Class is
   begin
      return Result : Object  do
         Result.Path_Name := Filename;
         Result.Timestamp := Timestamp;

         Set_Non_Ada (Result, Language, Kind);
      end return;
   end Create;

   ----------------
   -- Create_Ada --
   ----------------

   function Create_Ada
     (Filename  : GPR2.Path_Name.Object;
      Units     : GPR2.Unit.List.Object;
      Timestamp : Ada.Calendar.Time) return Object'Class
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
         Result.Timestamp := Timestamp;
         Result.Ada_Key   := Key;

         Set_Ada (Result, Units);
      end return;
   end Create_Ada;

   function Create_Ada
     (Filename      : GPR2.Path_Name.Object;
      Unit          : GPR2.Unit.Object;
      Is_RTS_Source : Boolean;
      Timestamp     : Ada.Calendar.Time) return Object'Class  is
   begin
      pragma Assert (Unit.Index = No_Index);
      return Result : Object do
         Result.Path_Name := Filename;
         Result.Timestamp := Timestamp;
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
