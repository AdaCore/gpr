--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Fixed;

with GPR2.Message;
with GPR2.Project.Source.Part_Set;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info.Set;
with GPR2.Source_Info.Parser.Registry;

package body GPR2.Project.Source is

   package PRA renames GPR2.Project.Registry.Attribute;
   use type Ada.Containers.Hash_Type;

   function Hash
     (Id : GPR2.Unit.Source_Unit_Identifier) return Ada.Containers.Hash_Type
   is (Ada.Strings.Hash (Id.Source.Value) +
         Ada.Containers.Hash_Type (Id.Index));

   package Source_Unit_Id_Sets is new Ada.Containers.Hashed_Sets
     (GPR2.Unit.Source_Unit_Identifier, Hash, GPR2.Unit."=", GPR2.Unit."=");

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

   ------------
   -- Create --
   ------------

   function Create
     (Source           : GPR2.Source.Object;
      View             : Project.View.Object;
      Naming_Exception : Naming_Exception_Kind;
      Is_Compilable    : Boolean;
      Aggregated       : Project.View.Object := Project.View.Undefined)
      return Object is
   begin
      return Result : Object :=
        Object'
          (Source with
           Definition.Weak (View),
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
      Closure : Boolean    := False;
      Sorted  : Boolean    := True) return Part_Set.Object
   is
      Deps : Part_Set.Object (Sorted => Sorted);

      procedure Insert
        (Source    : Object;
         Unit      : GPR2.Unit.Object;
         Timestamp : Ada.Calendar.Time);

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Source    : Object;
         Unit      : GPR2.Unit.Object;
         Timestamp : Ada.Calendar.Time)
      is
         pragma Unreferenced (Timestamp);
      begin
         if Unit.Is_Defined then
            Deps.Insert ((Source, Unit.Index));
         else
            Deps.Insert ((Source, No_Index));
         end if;

      end Insert;

   begin
      Self.Dependencies (Index, Insert'Access, Closure, Sorted);
      return Deps;
   end Dependencies;

   procedure Dependencies
     (Self     : Object;
      Index    : Unit_Index;
      For_Each : not null access procedure
                   (Source    : Object;
                    Unit      : GPR2.Unit.Object;
                    Timestamp : Ada.Calendar.Time);
      Closure  : Boolean := False;
      Sorted   : Boolean := True)
   is
      use type GPR2.Source_Info.Backend;

      Tree   : constant not null access Project.Tree.Object :=
                    View (Self).Tree;
      Done   : Source_Unit_Id_Sets.Set;
      Added  : Part_Set.Object (Sorted => Sorted);

      procedure Analyze_With_Clauses (Src : Source_Part);
      --  Retrieve the dependencies from with clauses

      procedure Handle_Dependency
        (Source    : Object;
         Index     : Unit_Index;
         Timestamp : Ada.Calendar.Time);
      --  Add the dependency to the result

      procedure On_Dependency
        (Unit_Name : Name_Type;
         Sfile     : Simple_Name;
         Kind      : GPR2.Unit.Library_Unit_Type;
         Stamp     : Ada.Calendar.Time);
      --  Callback when iterating on Source_Info.Dependencies

      --------------------------
      -- Analyze_With_Clauses --
      --------------------------

      procedure Analyze_With_Clauses (Src : Source_Part) is
         CU : GPR2.Unit.Object renames Src.Source.Unit (Src.Index);
      begin
         for W of CU.Dependencies loop
            declare
               V  : constant Project.View.Object :=
                        Tree.Get_View (W.Text);
               UI : Unit_Info.Object;
            begin
               if not V.Is_Defined then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Warning,
                        "withed unit " & String (W.Text) & " not found",
                        W));

               else
                  UI := Definition.Get_RO (V).Units (W.Text);

                  if not Closure then
                     if UI.Has_Spec then
                        Handle_Dependency
                          (V.Source (UI.Spec.Source),
                           UI.Spec.Index,
                           No_Time);

                     elsif UI.Has_Body then
                        Handle_Dependency
                          (V.Source (UI.Main_Body.Source),
                           UI.Main_Body.Index,
                           No_Time);
                     end if;

                  else
                     if UI.Has_Spec then
                        Handle_Dependency
                          (V.Source (UI.Spec.Source),
                           UI.Spec.Index,
                           No_Time);
                     end if;

                     if UI.Has_Body then
                        Handle_Dependency
                          (V.Source (UI.Main_Body.Source),
                           UI.Main_Body.Index,
                           No_Time);
                     end if;

                     for Sep of UI.Separates loop
                        Handle_Dependency
                          (V.Source (Sep.Source),
                           Sep.Index,
                           No_Time);
                     end loop;
                  end if;
               end if;
            end;
         end loop;
      end Analyze_With_Clauses;

      -----------------------
      -- Handle_Dependency --
      -----------------------

      procedure Handle_Dependency
        (Source    : Object;
         Index     : Unit_Index;
         Timestamp : Ada.Calendar.Time)
      is
         Position : Source_Unit_Id_Sets.Cursor;
         Inserted : Boolean;
      begin
         Done.Insert ((Source.Path_Name, Index), Position, Inserted);

         if Inserted then
            if Closure then
               Added.Insert ((Source, Index));
            end if;

            if Source.Has_Units then
               For_Each (Source, Source.Unit (Index), Timestamp);
            else
               For_Each (Source, GPR2.Unit.Undefined, Timestamp);
            end if;
         end if;
      end Handle_Dependency;

      -------------------
      -- On_Dependency --
      -------------------

      procedure On_Dependency
        (Unit_Name : Name_Type;
         Sfile     : Simple_Name;
         Kind      : GPR2.Unit.Library_Unit_Type;
         Stamp     : Ada.Calendar.Time)
      is
         S  : Project.Source.Constant_Access;
         CU : GPR2.Unit.Object;

      begin
         if not View (Self).Check_Source (Sfile, S) then
            return;
         end if;

         if S.Has_Units
           and then S.Check_Unit
             (Unit_Name, Spec => Kind in GPR2.Unit.Spec_Kind, Unit => CU)
         then
            Handle_Dependency (S.all, CU.Index, Stamp);
         else
            Handle_Dependency (S.all, No_Index, Stamp);
         end if;
      end On_Dependency;

   begin
      if not Self.Is_Parsed (Index) then
         return;
      end if;

      if Self.Used_Backend (Index) = Source_Info.LI then
         --  Dependencies will be retrieved from LI information for the
         --  source file.
         Added.Insert ((Self, Index));
      else
         --  Gather the withed units from the compilation unit: spec + body
         --  + separates.
         declare
            V    : constant Project.View.Object := View (Self);
            Def  : constant Definition.Const_Ref :=
                     Definition.Get_RO (View (Self));
            Unit : constant GPR2.Unit.Object := Self.Unit (Index);
            UI   : constant Unit_Info.Object :=
                     Def.Units.Element (if Unit.Kind = GPR2.Unit.S_Separate
                                        then Unit.Separate_From
                                        else Unit.Name);
         begin
            if UI.Has_Spec then
               if not Closure then
                  --  In closure mode, Handle_Directory will add the source
                  --  there.
                  Added.Insert ((V.Source (UI.Spec.Source), UI.Spec.Index));
               end if;

               --  Add explicitly the compilation unit, as the with clauses
               --  won't list them (but they're still dependencies).
               Handle_Dependency
                 (V.Source (UI.Spec.Source), UI.Spec.Index, No_Time);
            end if;

            if UI.Has_Body then
               if not Closure then
                  Added.Insert
                    ((V.Source (UI.Main_Body.Source), UI.Main_Body.Index));
               end if;

               Handle_Dependency
                 (V.Source (UI.Main_Body.Source), UI.Main_Body.Index, No_Time);
            end if;

            for Sep of UI.Separates loop
               if not Closure then
                  Added.Insert ((V.Source (Sep.Source), Sep.Index));
               end if;
               Handle_Dependency
                 (V.Source (Sep.Source), Sep.Index, No_Time);
            end loop;
         end;
      end if;

      while not Added.Is_Empty loop
         declare
            Set : constant Part_Set.Object := Added;
         begin
            Added.Clear;

            for Src_Part of Set loop
               if Src_Part.Source.Is_Parsed (Src_Part.Index) then
                  if Src_Part.Source.Used_Backend (Src_Part.Index) =
                    GPR2.Source_Info.LI
                  then
                     --  Get the dependencies directly from the dependency
                     --  file.

                     Src_Part.Source.Dependencies
                       (Src_Part.Index,
                        On_Dependency'Access);

                  elsif Src_Part.Source.Has_Units then
                     --  No dependency file: use the source parser to gather
                     --  the list of withed units.

                     Analyze_With_Clauses (Src_Part);
                  end if;
               end if;
            end loop;
         end;
      end loop;
   end Dependencies;

   procedure Dependencies
     (Self     : Object;
      Index    : Unit_Index;
      For_Each : not null access procedure
                   (Source    : Object;
                    Index     : Unit_Index;
                    Timestamp : Ada.Calendar.Time);
      Closure  : Boolean := False;
      Sorted   : Boolean := True)
   is
      procedure Action (Source    : Object;
                        Unit      : GPR2.Unit.Object;
                        Timestamp : Ada.Calendar.Time);

      procedure Action (Source    : Object;
                        Unit      : GPR2.Unit.Object;
                        Timestamp : Ada.Calendar.Time) is
      begin
         if Unit.Is_Defined then
            For_Each (Source, Unit.Index, Timestamp);
         else
            For_Each (Source, No_Index, Timestamp);
         end if;
      end Action;

   begin
      Self.Dependencies
        (Index, Action'Access, Closure => Closure, Sorted => Sorted);
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

   ------------------
   -- Is_Interface --
   ------------------

   function Is_Interface (Self : Object) return Boolean is
      Def : constant Definition.Const_Ref :=
              Definition.Get_RO (View (Self));
   begin
      if Def.Interface_Sources.Contains (Self.Path_Name.Simple_Name) then

         return True;

      elsif Self.Has_Units then
         for CU of Self.Units loop
            if Def.Interface_Units.Contains (CU.Name) then

               return True;

            end if;
         end loop;
      end if;

      return False;
   end Is_Interface;

   -------------
   -- Is_Main --
   -------------

   function Is_Main (Self : Object) return Boolean is
   begin
      return Definition.Strong (Self.View).Is_Main (Self);
   end Is_Main;

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
      Def : constant Definition.Ref :=
              Definition.Get (Definition.Strong (Self.View));
      C   : constant Project.Source.Set.Cursor :=
              Def.Sources.Find (Self);
   begin
      pragma Assert (Project.Source.Set.Has_Element (C));
      Self.Update (C, Backends);
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : in out Object;
      C        : Project.Source.Set.Cursor;
      Backends : Source_Info.Backend_Set := Source_Info.All_Backends)
   is
      use type GPR2.Source_Info.Parse_State;
      Language : constant Language_Id := Self.Language;
      Def      : constant Definition.Ref :=
                   Definition.Get (Definition.Strong (Self.View));

      procedure Clarify_Unit_Type;
      --  Set Kind to Spec_Only for the units without body and
      --  set Kind to S_Body_Only for the units without specification.

      -----------------------
      -- Clarify_Unit_Type --
      -----------------------

      procedure Clarify_Unit_Type is
         use Definition;
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
                                 (PRA.Naming.Dot_Replacement).Value.Text;
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

         declare
            CUnits   : GPR2.Project.Unit_Info.Set.Cursor;
            Inserted : Boolean;
         begin
            --  Check newly found separates and update Unit_Info
            for Unit of Self.Units loop
               declare
                  Unit_Name : constant Name_Type :=
                                (if Unit.Kind = GPR2.Unit.S_Separate
                                 then Unit.Separate_From
                                 else Unit.Name);
               begin
                  Def.Units.Insert
                    (Unit_Name,
                     Unit_Info.Create (Unit_Name),
                     CUnits,
                     Inserted);
               end;

               declare
                  SUI : constant GPR2.Unit.Source_Unit_Identifier :=
                          (Self.Path_Name, Unit.Index);
                  Ref : constant Unit_Info.Set.Set.Reference_Type :=
                          Def.Units.Reference (CUnits);
               begin
                  if Unit.Kind in GPR2.Unit.Spec_Kind
                    and then not Ref.Has_Spec
                  then
                     Ref.Update_Spec (SUI);

                  elsif Unit.Kind in GPR2.Unit.Body_Kind
                    and then not Ref.Has_Body
                  then
                     Ref.Update_Body (SUI);

                  elsif Unit.Kind = GPR2.Unit.S_Separate then
                     Ref.Update_Separates (SUI);
                  end if;
               end;

               Def.Units_Map.Include (Definition.Key (Unit), C);
            end loop;
         end;
      end if;

      Def.Sources.Replace (C, Self);
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
