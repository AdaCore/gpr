------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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

package body GPR2.Source_Info is

   ---------------------
   -- Build_Timestamp --
   ---------------------

   function Build_Timestamp (Self : Object) return Ada.Calendar.Time is
   begin
      pragma Assert (Self.LI_Timestamp /= No_Time);
      return Self.LI_Timestamp;
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
      Index : Unit_Index := 1) return Source_Reference.Identifier.Set.Object is
   begin
      return Self.CU_List (Positive (Index)).Dependencies;
   end Context_Clause_Dependencies;

   function Context_Clause_Dependencies
     (Self : Object;
      Unit : Name_Type) return Source_Reference.Identifier.Set.Object
   is
      Result : Source_Reference.Identifier.Set.Object;
   begin
      for CU of Self.CU_List loop
         if CU.Name = Unit then
            Result.Union (CU.Dependencies);
         end if;
      end loop;

      return Result;
   end Context_Clause_Dependencies;

   -----------------------
   -- File_Dependencies --
   -----------------------

   function Dependencies
     (Self  : Object;
      Index : Unit_Index := 1) return Containers.Filename_List
   is
      Result : Containers.Filename_List;
      C_Idx  : constant Unit_Dependencies.Cursor :=
                 Self.Dependencies.Find (Index);
   begin
      if Unit_Dependencies.Has_Element (C_Idx) then
         for D of Self.Dependencies (C_Idx) loop
            Result.Append (Filename_Type (D.Sfile));
         end loop;
      end if;

      return Result;
   end Dependencies;

   procedure Dependencies
     (Self   : Object;
      Action : access procedure
                 (Sfile : Simple_Name;
                  Unit  : Name_Type;
                  Kink  : GPR2.Unit.Library_Unit_Type);
      Index  : Unit_Index := 1)
   is
      C_Idx  : constant Unit_Dependencies.Cursor :=
                 Self.Dependencies.Find (Index);
   begin
      if Unit_Dependencies.Has_Element (C_Idx) then
         for C in Unit_Dependencies.Element (C_Idx).Iterate loop
            declare
               Key : constant Dependency_Key := Dependency_Maps.Key (C);
            begin
               Action
                 (Simple_Name (Dependency_Maps.Element (C).Sfile),
                  Name_Type (Key.Unit_Name),
                  Key.Unit_Kind);
            end;
         end loop;
      end if;
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
      return Self.CU_List.Length >= Containers.Count_Type (Index);
   end Has_Unit_At;

   ----------------
   -- Is_Generic --
   ----------------

   function Is_Generic
     (Self : Object; Index : Unit_Index := 1) return Boolean is
   begin
      return Self.Has_Units
        and then Self.CU_List (Positive (Index)).Is_Generic;
   end Is_Generic;

   --------------------------------
   -- Is_Implementation_Required --
   --------------------------------

   function Is_Implementation_Required
     (Self : Object; Index : Unit_Index := 1) return Boolean is
   begin
      return Self.CU_List
        (Positive (Index)).Is_Flag_Set (Unit.Body_Needed_For_SAL);
   end Is_Implementation_Required;

   ----------
   -- Kind --
   ----------

   function Kind
     (Self  : Object;
      Index : Unit_Index := 1) return Unit.Library_Unit_Type is
   begin
      if Self.Is_Ada then
         return Self.CU_List (Positive (Index)).Kind;
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

   ---------
   -- Set --
   ---------

   procedure Set
     (Self : in out Object;
      Kind : Unit.Library_Unit_Type) is
   begin
      Self.Is_Ada := False;
      Self.Kind   := Kind;
   end Set;

   -------------
   -- Set_Ada --
   -------------

   procedure Set_Ada
     (Self          : in out Object;
      Units         : Unit.List.Object;
      Is_RTS_Source : Boolean;
      Is_Indexed    : Boolean) is
   begin
      Self.Is_Ada        := True;
      Self.CU_List       := Units;
      Self.Is_RTS_Source := Is_RTS_Source;
      Self.Is_Indexed    := Is_Indexed;

      if Self.CU_List.Length > 0 then
         Self.Kind := Self.CU_List (1).Kind;
      end if;
   end Set_Ada;

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name
     (Self : Object; Index : Unit_Index := 1) return Name_Type is
   begin
      return Self.CU_List (Positive (Index)).Name;
   end Unit_Name;

   -----------------------
   -- Units --
   -----------------------

   function Units
     (Self : Object) return Unit.List.Object is
   begin
      return Self.CU_List;
   end Units;

   ------------
   -- Update --
   ------------

   procedure Update (Self : in out Object) is
   begin
      null;
   end Update;

   -----------------
   -- Update_Kind --
   -----------------

   procedure Update_Kind
     (Self  : in out Object;
      Kind  : Unit.Library_Unit_Type;
      Index : Unit_Index := 1) is
   begin
      if Index = 1 then
         Self.Kind := Kind;
      end if;

      Self.CU_List (Positive (Index)).Update_Kind (Kind);
   end Update_Kind;

end GPR2.Source_Info;
