--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

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
      Source_Dir_Idx   : Natural)
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
                Naming_Exception  => Naming_Exception,
                Source_Dir_Idx    => Source_Dir_Idx);
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
