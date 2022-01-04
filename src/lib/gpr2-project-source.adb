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

with Ada.Containers.Ordered_Sets;
with Ada.Strings.Fixed;

with GPR2.Message;
with GPR2.Project.Source.Part_Set;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info.Set;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference.Identifier.Set;

package body GPR2.Project.Source is

   package PRA renames GPR2.Project.Registry.Attribute;
   package PRP renames GPR2.Project.Registry.Pack;

   procedure Context_Clause_Dependencies
     (Self     : Object;
      For_Each : not null access procedure
                   (Source : GPR2.Project.Source.Object;
                    Unit   : GPR2.Unit.Object);
      Closure  : Boolean := False;
      Index    : Unit_Index := No_Index)
     with Pre => Self.Is_Defined and then Self.Has_Units;
   --  Returns the source files on which the current source file depends
   --  (potentially transitively). The dependence built on top of Ada "with"
   --  statements.

   function Change_Actual_View
     (Self : Object; View : Project.View.Object) return Object
     with Pre => Self.Is_Defined and then View.Is_Defined;
   --  Returns Object with changed actual view for the case when source was
   --  derived from extended project.

   ----------------
   -- Aggregated --
   ----------------

   function Aggregated (Self : Object) return Project.View.Object is
   begin
      return (if Self.Is_Aggregated
              then Definition.Strong (Self.Aggregated)
              else Project.View.Undefined);
   end Aggregated;

   -----------------------
   -- Aggregating_Views --
   -----------------------

   function Aggregating_Views (Self : Object) return Project.View.Set.Object is
   begin
      return Definition.Strong (Self.View).Aggregate_Libraries;
   end Aggregating_Views;

   ---------------
   -- Artifacts --
   ---------------

   function Artifacts
     (Self : Object; Force_Spec : Boolean := False) return Artifact.Object is
   begin
      return Artifact.Create (Self, Force_Spec => Force_Spec);
   end Artifacts;

   ------------------------
   -- Change_Actual_View --
   ------------------------

   function Change_Actual_View
     (Self : Object; View : Project.View.Object) return Object is
   begin
      return Result : Object := Self do
         Result.View      := Definition.Weak (View);
         Result.Inherited := True;
      end return;
   end Change_Actual_View;

   ---------------------------------
   -- Context_Clause_Dependencies --
   ---------------------------------

   procedure Context_Clause_Dependencies
     (Self     : Object;
      For_Each : not null access procedure
                  (Source : GPR2.Project.Source.Object;
                   Unit   : GPR2.Unit.Object);
      Closure  : Boolean := False;
      Index    : Unit_Index := No_Index)
   is
      Tree   : constant not null access Project.Tree.Object :=
                 View (Self).Tree;
      U_Done : Containers.Name_Set;

      package Source_Unit_Id_Sets is new Ada.Containers.Ordered_Sets
        (GPR2.Unit.Source_Unit_Identifier, GPR2.Unit."<", GPR2.Unit."=");
      S_Done : Source_Unit_Id_Sets.Set;
      --  Fast look-up tables to avoid analysing the same unit/file multiple
      --  time and more specifically avoid circularities.

      procedure Output (Unit : Unit_Info.Object)
        with Inline, Pre => Unit.Is_Defined;
      --  Call For_Each for each part of the Unit

      procedure To_Analyze (Src : Source_Part);
      --  Record Src's withed units to be analysed (insert into Buf)

      function Get
        (Source : GPR2.Path_Name.Object) return Project.Source.Object
      is
        (Tree.Get_View (Source).Source (Source));

      ------------
      -- Output --
      ------------

      procedure Output (Unit : Unit_Info.Object) is

         procedure Outp (Item : GPR2.Unit.Source_Unit_Identifier);

         ----------
         -- Outp --
         ----------

         procedure Outp (Item : GPR2.Unit.Source_Unit_Identifier) is
            Position : Source_Unit_Id_Sets.Cursor;
            Inserted : Boolean;
            Src      : GPR2.Project.Source.Object;
         begin
            S_Done.Insert (Item, Position, Inserted);
            if Inserted then
               Src := Get (Item.Source);
               For_Each (Src, Src.Unit (Item.Index));
            end if;
         end Outp;

      begin
         if Unit.Has_Spec then
            --  function and procedure compilation units allowed to do not have
            --  a spec.
            Outp (Unit.Spec);
         end if;

         if Unit.Has_Body then
            Outp (Unit.Main_Body);
         end if;

         for Sep of Unit.Separates loop
            Outp (Sep);
         end loop;
      end Output;

      Buf  : Source_Reference.Identifier.Set.Object;
      --  Buf contains units to be checked, this list is extended when looking
      --  for the full-closure. Using this list we avoid a recursive call.

      ----------------
      -- To_Analyze --
      ----------------

      procedure To_Analyze (Src : Source_Part) is
         Done_Pos : Containers.Name_Type_Set.Cursor;
         Position : Source_Reference.Identifier.Set.Cursor;
         Inserted : Boolean;
         CU       : GPR2.Unit.Object renames Src.Source.Unit (Src.Index);
      begin
         for W of CU.Dependencies loop
            U_Done.Insert (W.Text, Done_Pos, Inserted);

            if Inserted then
               Buf.Insert (W, Position, Inserted);
            end if;
         end loop;
      end To_Analyze;

   begin
      if Self.Has_Other_Part (Index) then
         To_Analyze (Self.Other_Part (Index));
      end if;

      To_Analyze ((Self, Index));

      For_Every_Unit : while not Buf.Is_Empty loop
         declare
            W    : constant Source_Reference.Identifier.Object'Class :=
                     Buf.First_Element;
            View : constant Project.View.Object :=
                     Tree.Get_View (Unit => W.Text);
         begin
            --  Remove the unit just taken from the list

            Buf.Delete_First;

            if not View.Is_Defined then
               Tree.Log_Messages.Append
                 (Message.Create
                    (Message.Warning,
                     "withed unit " & String (W.Text) & " not found",
                     W));

            else
               declare
                  Data : constant Definition.Const_Ref :=
                           Definition.Get_RO (View);
                  --  The view information for the unit Identifier
                  CU   : constant Unit_Info.Set.Cursor :=
                           Data.Units.Find (W.Text);
                  SU   : Unit_Info.Object;
               begin
                  if Unit_Info.Set.Set.Has_Element (CU) then
                     SU := Unit_Info.Set.Set.Element (CU);

                     --  At least the dependencies are the spec and body of
                     --  the withed unit.

                     Output (SU);

                     --  Finally, for the Closure mode add the dependencies
                     --  of withed unit from the direct withed spec and
                     --  bodies.

                     if Closure then
                        if SU.Spec.Source.Is_Defined then
                           To_Analyze ((Get (SU.Spec.Source), SU.Spec.Index));
                        end if;

                        if SU.Main_Body.Source.Is_Defined then
                           To_Analyze
                             ((Get (SU.Main_Body.Source), SU.Main_Body.Index));
                        end if;

                        for Sep of SU.Separates loop
                           To_Analyze ((Get (Sep.Source), Sep.Index));
                        end loop;
                     end if;

                  else
                     --  This should never happen, if the unit has been
                     --  found to be in the View, it should be there.

                     raise Project_Error
                       with "internal error in dependencies";
                  end if;
               end;
            end if;
         end;
      end loop For_Every_Unit;
   end Context_Clause_Dependencies;

   ------------
   -- Create --
   ------------

   function Create
     (Source           : GPR2.Source.Object;
      View             : Project.View.Object;
      Is_Interface     : Boolean;
      Naming_Exception : Naming_Exception_Kind;
      Is_Compilable    : Boolean;
      Aggregated       : Project.View.Object := Project.View.Undefined)
      return Object is
   begin
      return Result : Object :=
        Object'
          (Source with
           Definition.Weak (View),
           Is_Interface     => Is_Interface,
           Naming_Exception => Naming_Exception,
           Is_Compilable    => Is_Compilable,
           others           => <>)
      do
         if Aggregated.Is_Defined then
            Result.Aggregated := Definition.Weak (Aggregated);
         end if;
      end return;
   end Create;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self    : Object;
      Index   : Unit_Index := No_Index;
      Closure : Boolean    := False) return Part_Set.Object
   is
      Deps : Part_Set.Object;

      procedure Insert (Source : Object; Unit : GPR2.Unit.Object);

      ------------
      -- Insert --
      ------------

      procedure Insert (Source : Object; Unit : GPR2.Unit.Object) is
      begin
         if Unit.Is_Defined then
            Deps.Insert ((Source, Unit.Index));
         else
            Deps.Insert ((Source, No_Index));
         end if;

      end Insert;

   begin
      Self.Dependencies (Index, Insert'Access, Closure);
      return Deps;
   end Dependencies;

   procedure Dependencies
     (Self     : Object;
      Index    : Unit_Index;
      For_Each : not null access procedure
                   (Source : Object; Unit : GPR2.Unit.Object);
      Closure  : Boolean := False)
   is
      Done      : Part_Set.Object;

      procedure Action
        (Unit_Name : Name_Type;
         Sfile     : Simple_Name;
         Kind      : GPR2.Unit.Library_Unit_Type;
         Stamp     : Ada.Calendar.Time);
      --  Callback to list dependencies from GPR2.Source_Info

      ------------
      -- Action --
      ------------

      procedure Action
        (Unit_Name : Name_Type;
         Sfile     : Simple_Name;
         Kind      : GPR2.Unit.Library_Unit_Type;
         Stamp     : Ada.Calendar.Time)
      is
         S        : Project.Source.Object;
         Position : Part_Set.Cursor;
         Inserted : Boolean;
         CU       : GPR2.Unit.Object;

      begin
         if not View (Self).Check_Source (Sfile, S) then
            return;
         end if;

         if S.Has_Units
           and then S.Check_Unit
             (Unit_Name, Spec => Kind in GPR2.Unit.Spec_Kind, Unit => CU)
         then
            Done.Insert ((S, CU.Index), Position, Inserted);
         else
            Done.Insert ((S, No_Index), Position, Inserted);
         end if;

         if Inserted then
            if not S.Is_Ada
              and then not S.Is_Parsed (No_Index)
              and then S.Kind (No_Index) in GPR2.Unit.Spec_Kind
            then
               --  Non-Ada spec build timestamp can be taken only from
               --  dependencies.

               S.Update_Build_Timestamp (Stamp);
            end if;

            if Closure then
               for CU of S.Units loop
                  S.Dependencies (CU.Index, Action'Access);
               end loop;
            end if;

            For_Each (S, CU);
         end if;
      end Action;

   begin
      Self.Dependencies (Index, Action'Access);

      if Done.Is_Empty and then Self.Has_Units then
         --  It mean that we do not have ALI file parsed, try to get "with"
         --  dependencies from Ada parser.

         Self.Context_Clause_Dependencies
           (For_Each, Closure => Closure, Index => Index);
      end if;
   end Dependencies;

   procedure Dependencies
     (Self     : Object;
      Index    : Unit_Index;
      For_Each : not null access procedure
                   (Source : Object; Index : Unit_Index);
      Closure  : Boolean := False)
   is
      procedure Action (Source : Object; Unit : GPR2.Unit.Object);

      procedure Action (Source : Object; Unit : GPR2.Unit.Object) is
      begin
         if Unit.Is_Defined then
            For_Each (Source, Unit.Index);
         else
            For_Each (Source, No_Index);
         end if;
      end Action;

   begin
      Self.Dependencies (Index, Action'Access);
   end Dependencies;

   --------------------------
   -- Has_Aggregating_View --
   --------------------------

   function Has_Aggregating_View (Self : Object) return Boolean is
   begin
      return Definition.Strong (Self.View).Is_Aggregated_In_Library;
   end Has_Aggregating_View;

   --------------------
   -- Has_Other_Part --
   --------------------

   function Has_Other_Part
     (Self  : Object;
      Index : Unit_Index := No_Index) return Boolean
   is
      View   : constant Project.View.Object  :=
                 Definition.Strong (Self.View);
      Data   : constant Definition.Const_Ref := Definition.Get_RO (View);

   begin
      if Self.Has_Units
        and then Self.Units.Length >= Containers.Count_Type (Index)
      then
         declare
            use GPR2.Unit;
            CU     : constant GPR2.Unit.Object := Self.Unit (Index);
            Kind   : constant GPR2.Unit.Library_Unit_Type := CU.Kind;
            C_Unit : constant Unit_Info.Set.Cursor :=
                       Data.Units.Find
                         (if Kind = S_Separate
                          then CU.Separate_From
                          else CU.Name);
         begin
            if Unit_Info.Set.Set.Has_Element (C_Unit) then
               declare
                  Unit : constant Unit_Info.Set.Set.Constant_Reference_Type :=
                           Data.Units (C_Unit);
               begin
                  case Kind is
                     when Body_Kind =>
                        return Unit.Spec.Source.Is_Defined;

                     when Spec_Kind =>
                        return Unit.Main_Body.Source.Is_Defined;

                     when S_Separate =>
                        return Unit.Spec.Source.Is_Defined
                          or else Unit.Main_Body.Source.Is_Defined;
                  end case;
               end;
            end if;
         end;
      end if;

      return False;
   end Has_Other_Part;

   -------------------
   -- Is_Compilable --
   -------------------

   function Is_Compilable (Self : Object) return Boolean is
   begin
      return Self.Is_Compilable;
   end Is_Compilable;

   -------------
   -- Is_Main --
   -------------

   function Is_Main (Self : Object) return Boolean is
   begin
      return Definition.Strong (Self.View).Is_Main (Self);
   end Is_Main;

   ------------------
   -- Is_Overriden --
   ------------------

   function Is_Overriden (Self : Object) return Boolean is
      use type Project.View.Object;
      Try       : Object;
      Self_View : constant Project.View.Object := View (Self);
   begin
      if Self_View.Check_Source (Self.Path_Name.Simple_Name, Try)
        and then View (Try) /= Self_View
        and then View (Try).Is_Extending (Parent => Self_View)
      then
         return True;
      end if;

      if not Self.Is_Ada then
         return False;
      end if;

      for CU of Self.Units loop
         if Definition.Check_Source_Unit (Self_View, CU, Try)
           and then View (Try) /= Self_View
           and then View (Try).Is_Extending (Parent => Self_View)
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Overriden;

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part
     (Self  : Object;
      Index : Unit_Index := No_Index) return Source_Part
   is
      use GPR2.Unit;
      View      : constant Project.View.Object :=
                    Definition.Strong (Self.View);
      CU        : constant GPR2.Unit.Object := Self.Unit (Index);
      Kind      : constant GPR2.Unit.Library_Unit_Type := CU.Kind;
      Unit_Name : constant Name_Type :=
                          (if Kind = S_Separate
                           then CU.Separate_From
                           else CU.Name);
      Unit : constant Unit_Info.Object := View.Unit (Unit_Name);
   begin
      case Kind is
         when GPR2.Unit.Body_Kind =>
            return (View.Source (Unit.Spec.Source), Unit.Spec.Index);

         when GPR2.Unit.Spec_Kind =>
            return (View.Source (Unit.Main_Body.Source),
                    Unit.Main_Body.Index);

         when S_Separate =>
            if Unit.Spec.Source.Is_Defined then
               return (View.Source (Unit.Spec.Source), Unit.Spec.Index);
            else
               return (View.Source (Unit.Main_Body.Source),
                       Unit.Main_Body.Index);
            end if;
      end case;
   end Other_Part;

   --------------------------
   -- Other_Part_Unchecked --
   --------------------------

   function Other_Part_Unchecked
     (Self  : Object;
      Index : Unit_Index) return Source_Part
   is
      View   : constant Project.View.Object  :=
                 Definition.Strong (Self.View);
      Data   : constant Definition.Const_Ref := Definition.Get_RO (View);
   begin
      if Self.Has_Units
        and then Self.Units.Length >= Containers.Count_Type (Index)
      then
         declare
            use GPR2.Unit;
            CU     : constant GPR2.Unit.Object := Self.Unit (Index);
            Kind   : constant GPR2.Unit.Library_Unit_Type := CU.Kind;
            C_Unit : constant Unit_Info.Set.Cursor :=
                       Data.Units.Find
                         (if Kind = S_Separate
                          then CU.Separate_From
                          else CU.Name);
         begin
            if Unit_Info.Set.Set.Has_Element (C_Unit) then
               declare
                  Unit : constant Unit_Info.Set.Set.Constant_Reference_Type :=
                           Data.Units (C_Unit);
               begin
                  case Kind is
                     when GPR2.Unit.Body_Kind =>
                        if Unit.Spec.Source.Is_Defined then
                           return (View.Source (Unit.Spec.Source),
                                   Unit.Spec.Index);
                        end if;

                     when GPR2.Unit.Spec_Kind =>
                        if Unit.Main_Body.Source.Is_Defined then
                           return (View.Source (Unit.Main_Body.Source),
                                   Unit.Main_Body.Index);
                        end if;

                     when S_Separate =>
                        if Unit.Main_Body.Source.Is_Defined then
                           return (View.Source (Unit.Main_Body.Source),
                                   Unit.Main_Body.Index);
                        elsif Unit.Spec.Source.Is_Defined then
                           return (View.Source (Unit.Spec.Source),
                                   Unit.Spec.Index);
                        end if;
                  end case;
               end;
            end if;
         end;
      end if;

      return (Undefined, No_Index);
   end Other_Part_Unchecked;

   -------------------
   -- Separate_From --
   -------------------

   function Separate_From
     (Self  : Object;
      Index : Unit_Index) return Source_Part
   is
      View      : constant Project.View.Object  :=
                    Definition.Strong (Self.View);
      CU        : constant GPR2.Unit.Object := Self.Unit (Index);
      Unit_Name : constant Name_Type := CU.Separate_From;
      Unit      : constant Unit_Info.Object := View.Unit (Unit_Name);
   begin
      if Unit.Has_Spec then
         return (View.Source (Unit.Spec.Source), Unit.Spec.Index);
      else
         return (View.Source (Unit.Main_Body.Source), Unit.Main_Body.Index);
      end if;
   end Separate_From;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : in out Object;
      Backends : Source_Info.Backend_Set := Source_Info.All_Backends)
   is
      use type GPR2.Source_Info.Parse_State;
      Language : constant Language_Id := Self.Language;

      procedure Clarify_Unit_Type;
      --  Set Kind to Spec_Only for the units without body and
      --  set Kind to S_Body_Only for the units without specification.

      -----------------------
      -- Clarify_Unit_Type --
      -----------------------

      procedure Clarify_Unit_Type is
         use Definition;
         Def : constant Ref := Get (Strong (Self.View));
      begin
         for U of Self.Units loop
            declare
               use GPR2.Unit;
               package US renames Unit_Info.Set.Set;

               CU : constant Project.Unit_Info.Set.Cursor :=
                       Def.Units.Find (U.Name);

            begin
               if not US.Has_Element (CU) then
                  if Self.Is_Runtime then
                     return;
                  end if;

                  if Is_Runtime_Unit_Name (U.Name) then
                     --  Try to find possible runtime unit name and fix unit
                     --  name.

                     declare
                        DR : constant String :=
                               View (Self).Attribute
                                 (PRA.Dot_Replacement, PRP.Naming).Value.Text;
                        SN : constant String :=
                               String (Self.Path_Name.Simple_Name);
                        CU : Project.Unit_Info.Set.Cursor;
                     begin
                        if SN (SN'First + 1 .. SN'First + DR'Length) = DR then
                           CU := Def.Units.Find
                             (Name_Type
                                (Ada.Strings.Fixed.Replace_Slice
                                   (SN, SN'First + 1, SN'First + DR'Length,
                                    ".")));

                           if US.Has_Element (CU) then
                              Def.Units (CU).Update_Name (U.Name);
                              Def.Units.Insert (U.Name, US.Element (CU));
                              Def.Units.Delete (CU);
                           end if;
                        end if;
                     end;
                  end if;

                  return;
               end if;

               if U.Kind = S_Spec and then not US.Element (CU).Has_Body then
                  Self.Update_Kind (S_Spec_Only, U.Index);

               elsif U.Kind = S_Body and then not US.Element (CU).Has_Spec then
                  Self.Update_Kind (S_Body_Only, U.Index);
               end if;
            end;
         end loop;
      end Clarify_Unit_Type;

   begin
      if Self.Is_Parsed = Source_Info.Full then
         return;
      end if;

      for BK in Backends'Range loop
         if Backends (BK)
           and then Source_Info.Parser.Registry.Exists (Language, BK)
         then
            declare
               use type Source_Info.Implemented_Backend;

               Backend : constant not null access
                 Source_Info.Parser.Object'Class :=
                   Source_Info.Parser.Registry.Get (Language, BK);
            begin
               if BK = Source_Info.LI and then Self.Has_Units then
                  --  Need to clarify unit type before call to ALI parser to
                  --  detect when spec only Ada source has related ALI file.

                  Clarify_Unit_Type;
               end if;

               --  Ada source parser is not compatible with multi-unit
               --  sources.

               if not (BK = Source_Info.Source
                       and then Self.Has_Units
                       and then Self.Has_Index)
               then
                  Source_Info.Parser.Compute
                    (Self   => Backend,
                     Data   => Self,
                     Source => Self);
               end if;

               exit when Self.Is_Parsed = Source_Info.Full;
            end;
         end if;
      end loop;

      if Self.Has_Units then
         Clarify_Unit_Type;
      end if;
   end Update;

   ----------
   -- View --
   ----------

   function View (Self : Object) return Project.View.Object is
   begin
      return Definition.Strong (Self.View);
   end View;

begin
   Definition.Change_Actual_View := Change_Actual_View'Access;
end GPR2.Project.Source;
