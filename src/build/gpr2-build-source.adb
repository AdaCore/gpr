--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Interfaces.C;
with Ada.Calendar.Conversions;

with GPR2.Build.Tree_Db;

package body GPR2.Build.Source is

   type Unit_Iterator is new Unit_Iterators.Forward_Iterator with record
      List : access constant Unit_List;
   end record;

   overriding function First
     (Object : Unit_Iterator) return Cursor;

   overriding function Next
     (Object   : Unit_Iterator;
      Position : Cursor) return Cursor;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Unit_List;
      Position : Cursor) return Constant_Reference_Type
   is
      Ref : constant Unit_Map.Constant_Reference_Type :=
              Self.Units.Constant_Reference (Unit_Map.Cursor (Position));
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Constant_Reference;

   function Constant_Reference
     (Self     : aliased Unit_List;
      Position : Unit_Index) return Constant_Reference_Type
   is
   begin
      return Self.Constant_Reference (Cursor (Self.Units.Find (Position)));
   end Constant_Reference;

   ------------
   -- Create --
   ------------

   function Create
     (Filename         : GPR2.Path_Name.Object;
      Language         : Language_Id;
      Kind             : Unit_Kind;
      Timestamp        : Ada.Calendar.Time;
      Tree_Db          : access GPR2.Build.Tree_Db.Object;
      Naming_Exception : Naming_Exception_Kind;
      Source_Ref       : GPR2.Source_Reference.Value.Object;
      Is_Compilable    : Boolean := False)
      return Object
   is
   begin
      return
        (Db                => Tree_Db,
         Path_Name         => Filename,
         Modification_Time => Timestamp,
         Language          => Language,
         Kind              => Kind,
         CU_List           => Empty_List,
         Inherited         => False,
         Naming_Exception  => Naming_Exception,
         Is_Compilable     => Is_Compilable,
         SR                => Source_Ref);
   end Create;

   -----------
   -- First --
   -----------

   overriding function First
     (Object : Unit_Iterator) return Cursor is
   begin
      return Cursor (Object.List.Units.First);
   end First;

   -----------------
   -- Has_Unit_At --
   -----------------

   function Has_Unit_At (Self : Object; Index : Unit_Index) return Boolean is
   begin
      return Self.CU_List.Units.Contains (Index);
   end Has_Unit_At;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self    : in out Unit_List;
      Element : Unit_Part)
   is
   begin
      Self.Units.Insert (Element.Index, Element);

      if Element.Index /= No_Index then
         Self.Has_Index := True;
      end if;
   end Insert;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self : Unit_List) return Unit_Iterators.Forward_Iterator'Class
   is
   begin
      return Result : Unit_Iterator do
         Result := (List => Self'Unchecked_Access);
      end return;
   end Iterate;

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

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object   : Unit_Iterator;
      Position : Cursor) return Cursor
   is
      Result : Unit_Map.Cursor := Unit_Map.Cursor (Position);
   begin
      Unit_Map.Next (Result);
      return Cursor (Result);
   end Next;

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

      for U of Self.CU_List.Units loop
         U.Is_Parsed := False;
      end loop;
   end Update_Modification_Time;

   -----------------
   -- Update_Unit --
   -----------------

   procedure Update_Unit
     (Self  : in out Object;
      Unit  : Unit_Part) is
   begin
      if Self.CU_List.Is_Empty then
         Self.CU_List.Has_Index := Unit.Index /= No_Index;

      else
         pragma Assert (Unit.Index = No_Index or else Self.CU_List.Has_Index);
      end if;

      Self.CU_List.Units.Include (Unit.Index, Unit);
   end Update_Unit;

end GPR2.Build.Source;
