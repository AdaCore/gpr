--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Interfaces.C;
with Ada.Calendar.Conversions;

package body GPR2.Build.Source_Info is

   type Unit_Iterator is new Unit_Iterators.Forward_Iterator with record
      List : access constant Unit_List;
   end record;

   overriding function First
     (Object : Unit_Iterator) return Cursor;

   overriding function Next
     (Object   : Unit_Iterator;
      Position : Cursor) return Cursor;

   --------------
   -- Add_Unit --
   --------------

   procedure Add_Unit
     (Self  : in out Object;
      Unit  : Unit_Part)
   is
      Done : Boolean;
      C    : Unit_Map.Cursor;
   begin
      if Unit.Index = No_Index then
         pragma Assert (Self.CU_List.Units.Is_Empty);

         Self.CU_List.Units.Insert (Unit.Index, Unit);
         Self.CU_List.Has_Index := False;
         Self.Kind := Unit.Kind;

      else
         if not Self.CU_List.Has_Index then
            pragma Assert (Self.CU_List.Units.Is_Empty);

            Self.CU_List.Has_Index := True;
         end if;

         Self.CU_List.Units.Insert (Unit.Index, Unit, C, Done);

         pragma Assert (Done);
      end if;
   end Add_Unit;

   --  ----------------
   --  -- Check_Unit --
   --  ----------------
   --
   --  function Check_Unit
   --    (Self : Object;
   --     Name : Name_Type;
   --     Spec : Boolean;
   --     Unit : out GPR2.Unit.Object) return Boolean
   --  is
   --  begin
   --     for CU of Self.CU_List loop
   --        if (CU.Kind in GPR2.Unit.Spec_Kind) = Spec
   --          and then CU.Name = Name
   --        then
   --           Unit := CU;
   --           return True;
   --        end if;
   --     end loop;
   --
   --     return False;
   --  end Check_Unit;

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

   ----------
   -- Copy --
   ----------

   function Copy (Self : Object; To_View : Project.View.Object) return Object
   is
   begin
      return Result : Object := Self do
         Result.View := To_View;
         Result.Inherited := True;
      end return;
   end Copy;

   ------------
   -- Create --
   ------------

   function Create
     (Filename         : GPR2.Path_Name.Object;
      Language         : Language_Id;
      Kind             : Unit_Kind;
      Timestamp        : Ada.Calendar.Time;
      View             : GPR2.Project.View.Object;
      Naming_Exception : Naming_Exception_Kind;
      Source_Ref       : GPR2.Source_Reference.Value.Object;
      Aggregated       : Project.View.Object := Project.View.Undefined)
      return Object
   is
   begin
      return
        (View              => View,
         Path_Name         => Filename,
         Modification_Time => Timestamp,
         Language          => Language,
         Kind              => Kind,
         CU_List           => Empty_List,
         Aggregated        => Aggregated,
         Inherited         => False,
         Naming_Exception  => Naming_Exception,
         SR                => Source_Ref);
   end Create;

   ----------------
   -- Create_Ada --
   ----------------

   function Create_Ada
     (Filename         : GPR2.Path_Name.Object;
      Timestamp        : Ada.Calendar.Time;
      View             : GPR2.Project.View.Object;
      Naming_Exception : Naming_Exception_Kind;
      Source_Ref       : GPR2.Source_Reference.Value.Object;
      Units            : Unit_List'Class;
      Aggregated       : Project.View.Object := Project.View.Undefined)
      return Object
   is
   begin
      return
        (View              => View,
         Path_Name         => Filename,
         Modification_Time => Timestamp,
         Language          => Ada_Language,
         Kind              => <>,
         CU_List           => Unit_List (Units),
         Aggregated        => Aggregated,
         Inherited         => False,
         Naming_Exception  => Naming_Exception,
         SR                => Source_Ref);
   end Create_Ada;

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

   --------------------------------
   -- Is_Implementation_Required --
   --------------------------------

   --  function Is_Implementation_Required
   --    (Self : Object; Index : Unit_Index) return Boolean
   --  is
   --  begin
   --     return Self.CU_List
   --       (Index).Is_Flag_Set (GPR2.Unit.Body_Needed_For_SAL);
   --  end Is_Implementation_Required;

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

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Self     : aliased in out Unit_List;
      Position : Cursor) return Reference_Type
   is
      Ref : constant Unit_Map.Reference_Type :=
              Self.Units.Reference (Unit_Map.Cursor (Position));
   begin
      return (Element => Ref.Element.all'Unchecked_Access,
              Ref     => Ref);
   end Reference;

   function Reference
     (Self     : aliased in out Unit_List;
      Position : Unit_Index) return Reference_Type
   is
   begin
      return Self.Reference (Cursor (Self.Units.Find (Position)));
   end Reference;

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
   end Update_Modification_Time;

   -----------------
   -- Update_Unit --
   -----------------

   procedure Update_Unit
     (Self  : in out Object;
      Unit  : Unit_Part)
   is
      C : Unit_Map.Cursor;
   begin
      if Unit.Index = No_Index then
         pragma Assert (not Self.CU_List.Has_Index
                        and then Self.CU_List.Length = 1);

         Self.CU_List.Units.Include (Unit.Index, Unit);
      else
         pragma Assert (Self.CU_List.Has_Index);

         C := Self.CU_List.Units.Find (Unit.Index);

         pragma Assert (Unit_Map.Has_Element (C));

         Self.CU_List.Units.Replace_Element (C, Unit);
      end if;
   end Update_Unit;

end GPR2.Build.Source_Info;
