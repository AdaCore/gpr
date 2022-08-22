--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body GPR2.Source_Info is

   ---------------------
   -- Build_Timestamp --
   ---------------------

   function Build_Timestamp (Self  : Object;
                             Index : Unit_Index) return Ada.Calendar.Time is
   begin
      if Index = No_Index then
         return Self.LI_Timestamp;
      else
         return Self.CU_Info (Index).Build_Timestamp;
      end if;
   end Build_Timestamp;

   ----------------
   -- Check_Unit --
   ----------------

   function Check_Unit
     (Self : Object;
      Name : Name_Type;
      Spec : Boolean;
      Unit : out GPR2.Unit.Object) return Boolean is
   begin
      for CU of Self.CU_List loop
         if CU.Name = Name
           and then (CU.Kind in GPR2.Unit.Spec_Kind) = Spec
         then
            Unit := CU;
            return True;
         end if;
      end loop;

      return False;
   end Check_Unit;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Object) is
   begin
      Self.CU_List.Clear;
      Self.Dependencies.Clear;
   end Clear;

   ---------------------------------
   -- Context_Clause_Dependencies --
   ---------------------------------

   function Context_Clause_Dependencies
     (Self  : Object;
      Index : Unit_Index) return Source_Reference.Identifier.Set.Object is
   begin
      return Self.CU_List (Index).Dependencies;
   end Context_Clause_Dependencies;

   ------------------
   -- Dependencies --
   ------------------

   procedure Dependencies
     (Self   : Object;
      Index  : Unit_Index;
      Action : access procedure
                 (Unit_Name : Name_Type;
                  Sfile     : Simple_Name;
                  Kind      : GPR2.Unit.Library_Unit_Type;
                  Stamp     : Ada.Calendar.Time))
   is
      C_Idx : constant Unit_Dependencies.Cursor :=
                Self.Dependencies.Find (Index);
      U_Ref : Dependency_Vectors_Ref.Ref;

   begin
      if not Unit_Dependencies.Has_Element (C_Idx) then
         return;
      end if;

      U_Ref := Self.Dependencies (C_Idx);

      if U_Ref.Is_Null then
         return;
      end if;

      for C in U_Ref.Get.Iterate loop
         declare
            Ref   : constant Dependency_Vectors.Constant_Reference_Type :=
                      U_Ref.Get.Constant_Reference (C);
         begin
            Action
              (Name_Type (Ref.Unit_Name),
               Simple_Name (Ref.Sfile),
               Ref.Unit_Kind,
               Ref.Stamp);
         end;
      end loop;
   end Dependencies;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit (Self : Object; Unit : Name_Type) return Boolean is
   begin
      --  ??? CU_Map should be changed to be a map for unit name -> unit
      for CU of Self.CU_List loop
         if CU.Name = Unit then
            return True;
         end if;
      end loop;

      return False;
   end Has_Unit;

   -----------------
   -- Has_Unit_At --
   -----------------

   function Has_Unit_At
     (Self : Object; Index : Unit_Index) return Boolean is
   begin
      return Self.CU_List.Has_Element (Index);
   end Has_Unit_At;

   ----------------
   -- Is_Generic --
   ----------------

   function Is_Generic
     (Self : Object; Index : Unit_Index) return Boolean is
   begin
      return Self.Has_Units
        and then Self.CU_List (Index).Is_Generic;
   end Is_Generic;

   --------------------------------
   -- Is_Implementation_Required --
   --------------------------------

   function Is_Implementation_Required
     (Self : Object; Index : Unit_Index) return Boolean is
   begin
      return Self.CU_List
        (Index).Is_Flag_Set (GPR2.Unit.Body_Needed_For_SAL);
   end Is_Implementation_Required;

   ---------------
   -- Is_Parsed --
   ---------------

   function Is_Parsed (Self : Object) return Parse_State
   is
      Fully_Parsed : Boolean := True;
      Not_Parsed   : Boolean := True;
   begin
      if not Self.Has_Units then
         if Self.Is_Parsed (No_Index) then
            return Full;
         else
            return No;
         end if;
      else
         for CU of Self.Units loop
            if Self.Is_Parsed (CU.Index) then
               Not_Parsed := False;
            else
               Fully_Parsed := False;
            end if;
         end loop;

         if Fully_Parsed then
            return Full;
         elsif Not_Parsed then
            return No;
         else
            return Partial;
         end if;
      end if;
   end Is_Parsed;

   ----------
   -- Kind --
   ----------

   function Kind
     (Self  : Object;
      Index : Unit_Index := No_Index) return GPR2.Unit.Library_Unit_Type is
   begin
      if Self.Has_Units then
         return Self.CU_List (Index).Kind;
      else
         return Self.Kind;
      end if;
   end Kind;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Object) is
   begin
      Self := Undefined;
   end Reset;

   -------------
   -- Set_Ada --
   -------------

   procedure Set_Ada
     (Self  : in out Object;
      Units : GPR2.Unit.List.Object) is
   begin
      Self.Language      := Ada_Language;
      Self.Is_RTS_Source := False;
      Self.CU_List       := Units;
      Self.CU_Info       :=
        Unit_Info_Vectors.To_Vector (Unit_Info'(others => <>),
                                     Units.Length);
   end Set_Ada;

   procedure Set_Ada
     (Self          : in out Object;
      Unit          : GPR2.Unit.Object;
      Is_RTS_Source : Boolean) is
   begin
      pragma Assert (Unit.Index = No_Index);
      Self.Language      := Ada_Language;
      Self.Is_RTS_Source := Is_RTS_Source;
      Self.CU_List.Clear;
      Self.CU_List.Insert (Unit);
      Self.CU_Info       := Unit_Info_Vectors.Empty_Vector;
   end Set_Ada;

   -----------------
   -- Set_Non_Ada --
   -----------------

   procedure Set_Non_Ada
     (Self     : in out Object;
      Language : Language_Id;
      Kind     : GPR2.Unit.Library_Unit_Type) is
   begin
      pragma Assert (Language /= Ada_Language);
      Self.Language := Language;
      Self.Kind     := Kind;
   end Set_Non_Ada;

   ----------
   -- Unit --
   ----------

   function Unit
     (Self  : Object;
      Index : Unit_Index) return GPR2.Unit.Object is
   begin
      return Self.CU_List (Index);
   end Unit;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name
     (Self  : Object;
      Index : Unit_Index := No_Index) return Name_Type is
   begin
      return Self.CU_List (Index).Name;
   end Unit_Name;

   ----------------------------
   -- Update_Build_Timestamp --
   ----------------------------

   procedure Update_Build_Timestamp
     (Self : in out Object; Stamp : Ada.Calendar.Time) is
   begin
      Self.LI_Timestamp := Stamp;
   end Update_Build_Timestamp;

   -----------------
   -- Update_Kind --
   -----------------

   procedure Update_Kind
     (Self  : in out Object;
      Kind  : GPR2.Unit.Library_Unit_Type;
      Index : Unit_Index) is
   begin
      if not Self.Has_Units then
         Self.Kind := Kind;
      else
         Self.CU_List (Index).Update_Kind (Kind);
      end if;
   end Update_Kind;

end GPR2.Source_Info;
