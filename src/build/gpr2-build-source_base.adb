--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Interfaces.C;
with Ada.Calendar.Conversions;

with GPR2.Build.Tree_Db;

package body GPR2.Build.Source_Base is

   ------------
   -- Create --
   ------------

   function Create
     (Filename         : GPR2.Path_Name.Object;
      Language         : Language_Id;
      Kind             : Unit_Kind;
      Timestamp        : Ada.Calendar.Time;
      Tree_Db          : access GPR2.Build.Tree_Db.Object;
      Naming_Exception : Boolean;
      Source_Ref       : GPR2.Source_Reference.Value.Object;
      Is_Compilable    : Boolean := False)
      return Object'Class
   is
   begin
      return
        Object'(Db                => Tree_Db,
                Path_Name         => Filename,
                Modification_Time => Timestamp,
                Language          => Language,
                Kind              => Kind,
                CU_List           => Unit_Info.List.Empty_List,
                Inherited         => False,
                Naming_Exception  => Naming_Exception,
                Is_Compilable     => Is_Compilable,
                SR                => Source_Ref);
   end Create;

   -----------------
   -- Has_Unit_At --
   -----------------

   function Has_Unit_At (Self : Object; Index : Unit_Index) return Boolean is
   begin
      return Self.CU_List.Contains (Index);
   end Has_Unit_At;

   ----------
   -- Kind --
   ----------

   function Kind
     (Self : Object; Index : Unit_Index := No_Index)
      return Unit_Kind
   is
   begin
      if Self.Has_Units then
         return Self.CU_List.Element (Index).Kind;
      else
         return Self.Kind;
      end if;
   end Kind;

   -----------------
   -- Remove_Unit --
   -----------------

   procedure Remove_Unit
     (Self : in out Object;
      Index : Unit_Index) is
   begin
      Self.CU_List.Delete (Index);
   end Remove_Unit;

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
         + (if On_Windows
            then Timestamp mod 2
            else 0));
   end To_ALI_Timestamp;

   ------------------------------
   -- Update_Modification_Time --
   ------------------------------

   procedure Update_Modification_Time
     (Self : in out Object;
      Time : Ada.Calendar.Time)
   is
   begin
      Self.Modification_Time := Time;

      for U of Self.CU_List loop
         U.Set_Parsed_State (False);
      end loop;
   end Update_Modification_Time;

   -----------------
   -- Update_Unit --
   -----------------

   procedure Update_Unit
     (Self  : in out Object;
      Unit  : Unit_Info.Object) is
   begin
      Self.CU_List.Include (Unit);
   end Update_Unit;

end GPR2.Build.Source_Base;
