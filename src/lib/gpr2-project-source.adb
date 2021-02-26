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

with GPR2.Message;
with GPR2.Project.Definition;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info.Set;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference.Identifier.Set;

package body GPR2.Project.Source is

   procedure Context_Clause_Dependencies
     (Self     : Object;
      For_Each : not null access procedure
                   (Source : GPR2.Project.Source.Object);
      Closure  : Boolean := False)
     with Pre => Self.Is_Defined and then Self.Source.Has_Units;
   --  Returns the source files on which the current source file depends
   --  (potentially transitively). The dependence built on top of Ada "with"
   --  statements.

   function Change_Actual_View
     (Self : Object; View : Project.View.Object) return Object
     with Pre => Self.Is_Defined and then View.Is_Defined;
   --  Returns Object with changed actual view for the case when source was
   --  derived from extended project.

   -----------------------
   -- Aggregating_Views --
   -----------------------

   function Aggregating_Views (Self : Object) return Project.View.Set.Object
   is
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
                   (Source : GPR2.Project.Source.Object);
      Closure  : Boolean := False)
   is
      Tree   : constant not null access Project.Tree.Object :=
                 View (Self).Tree;
      U_Done : Containers.Name_Set;
      S_Done : Containers.Filename_Set;
      --  Fast look-up tables to avoid analysing the same unit/file multiple
      --  time and more specifically avoid circularities.

      procedure Output (Unit : Unit_Info.Object)
        with Inline, Pre => Unit.Is_Defined;
      --  Call For_Each for each part of the Unit

      procedure To_Analyze (Src : GPR2.Project.Source.Object);
      --  Record Src's withed units to be analysed (insert into Buf)

      function Get
        (Source : GPR2.Path_Name.Object) return Project.Source.Object
      is
        (Tree.Get_View (Source).Source (Source));

      ------------
      -- Output --
      ------------

      procedure Output (Unit : Unit_Info.Object) is

         procedure Outp (Item : GPR2.Path_Name.Object);

         ----------
         -- Outp --
         ----------

         procedure Outp (Item : GPR2.Path_Name.Object) is
            Position : Containers.Filename_Type_Set.Cursor;
            Inserted : Boolean;
         begin
            S_Done.Insert (Item.Simple_Name, Position, Inserted);
            if Inserted then
               For_Each (Get (Item));
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

      Buf  : Source_Reference.Identifier.Set.Object :=
               Self.Source.Context_Clause_Dependencies;
      --  Buf contains units to be checked, this list is extended when looking
      --  for the full-closure. Using this list we avoid a recursive call.

      ----------------
      -- To_Analyze --
      ----------------

      procedure To_Analyze (Src : GPR2.Project.Source.Object) is
         Done_Pos : Containers.Name_Type_Set.Cursor;
         Position : Source_Reference.Identifier.Set.Cursor;
         Inserted : Boolean;
      begin
         for CU of Src.Source.Units loop
            for W of CU.Dependencies loop
               U_Done.Insert (W.Text, Done_Pos, Inserted);

               if Inserted then
                  Buf.Insert (W, Position, Inserted);
               end if;
            end loop;
         end loop;
      end To_Analyze;

   begin
      if Self.Has_Other_Part then
         To_Analyze (Self.Other_Part);
      end if;

      To_Analyze (Self);

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
                     SU := Data.Units (CU);

                     --  At least the dependencies are the spec and body of
                     --  the withed unit.

                     Output (SU);

                     --  Finaly, for the Closure mode add the dependencies
                     --  of withed unit from the direct withed spec and
                     --  bodies.

                     if Closure then
                        if SU.Spec.Is_Defined then
                           To_Analyze (Get (SU.Spec));
                        end if;

                        if SU.Main_Body.Is_Defined then
                           To_Analyze (Get (SU.Main_Body));
                        end if;

                        for Sep of SU.Separates loop
                           To_Analyze (Get (Sep));
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
      Aggregated       : Boolean := False) return Object is
   begin
      return Object'
        (Source,
         Definition.Weak (View),
         Is_Interface, Naming_Exception, Is_Compilable, Aggregated, False);
   end Create;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self    : Object;
      Closure : Boolean := False) return GPR2.Project.Source.Set.Object
   is
      Deps : GPR2.Project.Source.Set.Object;

      procedure Insert (Source : GPR2.Project.Source.Object);

      ------------
      -- Insert --
      ------------

      procedure Insert (Source : GPR2.Project.Source.Object) is
      begin
         Deps.Insert (Source);
      end Insert;

   begin
      Self.Dependencies (Insert'Access, Closure);
      return Deps;
   end Dependencies;

   procedure Dependencies
     (Self     : Object;
      For_Each : not null access procedure
                   (Source : GPR2.Project.Source.Object);
      Closure  : Boolean := False)
   is
      Done : Containers.Filename_Set;

      procedure Collect_Source (Source : Source_Info.Object'Class);

      --------------------
      -- Collect_Source --
      --------------------

      procedure Collect_Source (Source : Source_Info.Object'Class) is
         S        : Project.Source.Object;
         Position : Containers.Filename_Type_Set.Cursor;
         Inserted : Boolean;
      begin
         for File of Source.Dependencies loop
            Done.Insert (File, Position, Inserted);

            if Inserted
              and then View (Self).Check_Source (File, S)
            then
               For_Each (S);

               if Closure then
                  Collect_Source (S.Source);
               end if;
            end if;
         end loop;
      end Collect_Source;

   begin
      Collect_Source (Self.Source);

      if Done.Is_Empty then
         --  It mean that we do not have ALI file parsed, try to get "with"
         --  dependencies from Ada parser.
         Self.Context_Clause_Dependencies (For_Each, Closure);
      end if;
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
      Index : Source_Info.Unit_Index := 1) return Boolean
   is
      use all type GPR2.Unit.Library_Unit_Type;

      View   : constant Project.View.Object  :=
                 Definition.Strong (Self.View);
      Data   : constant Definition.Const_Ref := Definition.Get_RO (View);
      Source : constant GPR2.Source.Object   := Self.Source;
   begin
      if Source.Is_Defined
        and then Source.Has_Units
        and then Source.Units.Length >= Containers.Count_Type (Index)
      then
         declare
            CU     : constant GPR2.Unit.Object :=
                       Source.Units.Element (Positive (Index));
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
                        return Unit.Spec.Is_Defined
                          or else Unit.Separates.Length > 0;

                     when GPR2.Unit.Spec_Kind =>
                        return Unit.Main_Body.Is_Defined
                          or else Unit.Separates.Length > 0;

                     when S_Separate =>
                        return Unit.Spec.Is_Defined
                          or else Unit.Main_Body.Is_Defined;
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
      Try : Object;
   begin
      if not View (Self).Check_Source (Self.Path_Name.Simple_Name, Try)
        or else View (Try) /= View (Self)
      then
         return True;

      elsif not Self.Source.Has_Units then
         return False;
      end if;

      for U of Self.Source.Units loop
         if Definition.Check_Source_Unit (View (Self), U, Try)
           and then View (Try) = View (Self)
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_Overriden;

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part
     (Self  : Object;
      Index : Source_Info.Unit_Index := 1) return Object
   is
      use all type GPR2.Unit.Library_Unit_Type;

      View      : constant Project.View.Object :=
                    Definition.Strong (Self.View);
      CU        : constant GPR2.Unit.Object :=
                    Source (Self).Units.Element (Positive (Index));
      Kind      : constant GPR2.Unit.Library_Unit_Type := CU.Kind;
      Unit_Name : constant Name_Type :=
                          (if Kind = S_Separate
                           then CU.Separate_From
                           else CU.Name);
      Unit      : constant Unit_Info.Object := View.Unit (Unit_Name);
   begin
      case Kind is
         when GPR2.Unit.Body_Kind =>
            if Unit.Spec.Is_Defined then
               return View.Source (Unit.Spec);
            else
               --  ??? returning first separate
               return View.Source (Unit.Separates.First_Element);
            end if;

         when GPR2.Unit.Spec_Kind =>
            if Unit.Main_Body.Is_Defined then
               return View.Source (Unit.Main_Body);
            else
               --  ??? returning first separate
               return View.Source (Unit.Separates.First_Element);
            end if;

         when S_Separate =>
            if Unit.Spec.Is_Defined then
               return View.Source (Unit.Spec);
            else
               return View.Source (Unit.Main_Body);
            end if;
      end case;
   end Other_Part;

   -------------------
   -- Separate_From --
   -------------------

   function Separate_From
     (Self  : Object;
      Index : Source_Info.Unit_Index := 1) return Object
   is
      View      : constant Project.View.Object  :=
                    Definition.Strong (Self.View);
      CU        : constant GPR2.Unit.Object :=
                    Source (Self).Units.Element (Positive (Index));
      Unit_Name : constant Name_Type := CU.Separate_From;
      Unit      : constant Unit_Info.Object := View.Unit (Unit_Name);
   begin
      if Unit.Has_Spec then
         return View.Source (Unit.Spec);
      else
         return View.Source (Unit.Main_Body);
      end if;
   end Separate_From;

   ------------
   -- Source --
   ------------

   function Source (Self : Object) return GPR2.Source.Object is
   begin
      return Self.Source;
   end Source;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : in out Object;
      Backends : Source_Info.Backend_Set := Source_Info.All_Backends)
   is
      Language : constant Name_Type := Self.Source.Language;

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
         for U of Self.Source.Units loop
            declare
               use GPR2.Unit;
               package US renames Unit_Info.Set.Set;

               CU : constant Project.Unit_Info.Set.Cursor :=
                       Def.Units.Find (U.Name);

               procedure Update_Kind (Kind : Library_Unit_Type);
               --  Update U unit kind and Source kind if necessary

               -----------------
               -- Update_Kind --
               -----------------

               procedure Update_Kind (Kind : Library_Unit_Type) is
               begin
                  Self.Source.Update_Kind
                    (Kind, Source_Info.Unit_Index (U.Index));
               end Update_Kind;

            begin
               if not US.Has_Element (CU) then
                  if Self.Source.Is_Runtime then
                     return;
                  end if;

                  pragma Assert
                    (U.Kind not in S_Spec | S_Body,
                     "can't find """ & String (U.Name) & """ for "
                     & U.Kind'Img & " in """ & Self.Path_Name.Value & '"');
               end if;

               if U.Kind = S_Spec and then not US.Element (CU).Has_Body then
                  Update_Kind (S_Spec_Only);

               elsif U.Kind = S_Body and then not US.Element (CU).Has_Spec then
                  Update_Kind (S_Body_Only);
               end if;
            end;
         end loop;
      end Clarify_Unit_Type;

   begin
      if not Self.Source.Has_Units or else Self.Source.Is_Parsed then
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
               if BK = Source_Info.LI then
                  --  Need to clarify unit type before call to ALI parser to
                  --  detect when spec only Ada source has related ALI file.

                  Clarify_Unit_Type;
               end if;

               Source_Info.Parser.Compute
                 (Self   => Backend,
                  Data   => Self.Source,
                  Source => Self);

               exit when Self.Source.Is_Parsed;
            end;
         end if;
      end loop;

      Clarify_Unit_Type;
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
