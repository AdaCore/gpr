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
      Tree_Db          : access GPR2.Build.Tree_Db.Object;
      Naming_Exception : Boolean;
      From_Src_Subdirs : Boolean)
      return Object'Class
   is
   begin
      return
        Object'(Db                => Tree_Db,
                Path_Name         => Filename,
                Language          => Language,
                Kind              => Kind,
                CU_List           => Unit_Info.List.Empty_List,
                Naming_Exception  => Naming_Exception,
                Src_Subdirs       => From_Src_Subdirs);
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
   -- Query_Units --
   -----------------

   procedure Query_Units
     (Self    : Object;
      Process : not null access procedure (Unit : Unit_Info.Object)) is
   begin
      for U of Self.CU_List loop
         Process (U);
      end loop;
   end Query_Units;

   -----------------
   -- Remove_Unit --
   -----------------

   procedure Remove_Unit
     (Self : in out Object;
      Index : Unit_Index) is
   begin
      Self.CU_List.Delete (Index);
   end Remove_Unit;

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
