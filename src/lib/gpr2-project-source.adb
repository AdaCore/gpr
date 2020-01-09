------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Definition;
with GPR2.Project.Pack;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.Unit_Info;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Identifier.Set;

package body GPR2.Project.Source is

   ----------------------
   -- Aggregating_View --
   ----------------------

   function Aggregating_View (Self : Object) return Project.View.Object is
   begin
      return Definition.Strong (Self.View).Aggregate;
   end Aggregating_View;

   ---------------
   -- Artifacts --
   ---------------

   function Artifacts (Self : Object) return Artifact.Object is
   begin
      return Artifact.Create (Self);
   end Artifacts;

   ------------
   -- Create --
   ------------

   function Create
     (Source               : GPR2.Source.Object;
      View                 : Project.View.Object;
      Is_Interface         : Boolean;
      Has_Naming_Exception : Boolean;
      Is_Compilable        : Boolean;
      Aggregated           : Boolean := False) return Object is
   begin
      return Object'
        (Source,
         Definition.Weak (View),
         Is_Interface, Has_Naming_Exception, Is_Compilable, Aggregated);
   end Create;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self : Object;
      Mode : Dependency := Direct) return GPR2.Project.Source.Set.Object
   is
      View : constant Project.View.Object  := Definition.Strong (Self.View);
      Tree : constant not null access Project.Tree.Object := View.Tree;

      procedure Insert
        (Deps : in out GPR2.Project.Source.Set.Object;
         Unit : Unit_Info.Object)
        with Inline, Pre => Unit.Is_Defined;
      --  Insert Unit into Deps (result of this routine)

      procedure To_Analyze (Src : GPR2.Project.Source.Object);
      --  Record Src's withed units to be analysed (insert into Buf)

      function Get
        (Source : GPR2.Path_Name.Object) return Project.Source.Object is
        (Tree.Get_View (Source).Source (Source));

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Deps : in out GPR2.Project.Source.Set.Object;
         Unit : Unit_Info.Object)
      is
         procedure Insert
           (Deps : in out GPR2.Project.Source.Set.Object;
            Src  : GPR2.Project.Source.Object) with Inline;
         --  Insert Src into the Deps if it does not exists

         ------------
         -- Insert --
         ------------

         procedure Insert
           (Deps : in out GPR2.Project.Source.Set.Object;
            Src  : GPR2.Project.Source.Object) is
         begin
            if not Deps.Contains (Src) then
               Deps.Insert (Src);
            end if;
         end Insert;

      begin
         if Unit.Has_Spec then
            --  function and procedure compilation units allowed to do not have
            --  a spec.
            Insert (Deps, Get (Unit.Spec));
         end if;

         if Unit.Has_Body then
            Insert (Deps, Get (Unit.Main_Body));
         end if;

         for Sep of Unit.Separates loop
            Insert (Deps, Get (Sep));
         end loop;
      end Insert;

      Data : constant Definition.Const_Ref := Definition.Get_RO (View);

      Buf  : Source_Reference.Identifier.Set.Object :=
               Source (Self).Dependencies;
      --  Buf contains units to be checked, this list is extended when looking
      --  for the full-closure. Using this list we avoid a recursive call.

      Done : Source_Reference.Identifier.Set.Object;
      --  Fast look-up table to avoid analysing the same unit multiple time and
      --  more specifically avoid circularities.

      Deps : GPR2.Project.Source.Set.Object;
      --  The resulting source set

      ----------------
      -- To_Analyze --
      ----------------

      procedure To_Analyze (Src : GPR2.Project.Source.Object) is
      begin
         for CU of Source (Src).Units loop
            for W of CU.Dependencies loop
               if not Done.Contains (W) and then not Buf.Contains (W) then
                  Buf.Include (W);
               end if;
            end loop;
         end loop;
      end To_Analyze;

   begin
      --  First we need to ensure that all views are up-to-date regarding the
      --  sources/unit. The sources are recomputed only if required.

      Data.Tree.Update_Sources;

      --  For Unit or Closure add dependencies from the other part

      if Mode in Unit | Closure then
         if Source (Self).Has_Single_Unit and then Self.Has_Other_Part then
            To_Analyze (View.Source (Self.Other_Part.Path_Name));
         end if;
      end if;

      For_Every_Unit : loop
         exit For_Every_Unit when Buf.Is_Empty;

         declare
            W    : constant Source_Reference.Identifier.Object :=
                     Source_Reference.Identifier.Object (Buf.First_Element);
            View : Project.View.Object;
         begin
            --  Remove the unit just taken from the list

            Buf.Delete_First;

            if not Done.Contains (W) then
               Done.Include (W);

               View := Data.Tree.Get_View (Unit => W.Text);

               if not View.Is_Defined then
                  Data.Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Warning,
                        "withed unit " & String (W.Text) & " not found",
                        W));

               else
                  declare
                     Data : constant Definition.Const_Ref :=
                              Definition.Get_RO (View);
                     --  The view information for the unit Identifier
                     SU   : Unit_Info.Object;
                  begin
                     if Data.Units.Contains (W.Text) then
                        SU := Data.Units.Element (W.Text);

                        --  At least the dependencies are the spec and body of
                        --  the withed unit.

                        Insert (Deps, SU);

                        --  Finaly, for the Closure mode add the dependencies
                        --  of withed unit from the direct withed spec and
                        --  bodies.

                        if Mode = Closure then
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
            end if;
         end;
      end loop For_Every_Unit;

      return Deps;
   end Dependencies;

   --------------------
   -- Extending_View --
   --------------------

   function Extending_View (Self : Object) return Project.View.Object is
   begin
      return Definition.Strong (Self.View).Extending;
   end Extending_View;

   --------------------------
   -- Has_Aggregating_View --
   --------------------------

   function Has_Aggregating_View (Self : Object) return Boolean is
   begin
      return Definition.Strong (Self.View).Is_Aggregated_In_Library;
   end Has_Aggregating_View;

   ------------------------
   -- Has_Extending_View --
   ------------------------

   function Has_Extending_View (Self : Object) return Boolean is
   begin
      return Definition.Strong (Self.View).Is_Extended;
   end Has_Extending_View;

   --------------------
   -- Has_Other_Part --
   --------------------

   function Has_Other_Part
     (Self  : Object;
      Index : Source_Info.Unit_Index := 1) return Boolean
   is
      use all type GPR2.Unit.Kind_Type;

      View   : constant Project.View.Object  := Definition.Strong (Self.View);
      Data   : constant Definition.Const_Ref := Definition.Get_RO (View);
      Source : constant GPR2.Source.Object := Self.Source;
   begin
      if Source.Is_Defined
        and then Source.Has_Units
        and then Source.Units.Length >= Containers.Count_Type (Index)
      then
         declare
            CU        : constant GPR2.Unit.Object :=
                          Source.Units.Element (Positive (Index));
            Kind      : constant GPR2.Unit.Kind_Type := CU.Kind;
            Unit_Name : constant Name_Type :=
                          (if Kind = S_Separate
                           then CU.Separate_From
                           else CU.Name);
         begin
            if Data.Units.Contains (Unit_Name) then
               declare
                  Unit : constant Unit_Info.Object :=
                           View.Unit (Unit_Name);
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

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part
     (Self  : Object;
      Index : Source_Info.Unit_Index := 1) return Object
   is
      use all type GPR2.Unit.Kind_Type;

      View      : constant Project.View.Object :=
                    Definition.Strong (Self.View);
      CU        : constant GPR2.Unit.Object :=
                    Source (Self).Units.Element (Positive (Index));
      Kind      : constant GPR2.Unit.Kind_Type := CU.Kind;
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

   procedure Update (Self : in out Object) is
      Language : constant Name_Type := Self.Source.Language;
   begin
      --  If we have a LI parser for this language, use it

      if Source_Info.Parser.Registry.Exists (Language, Source_Info.LI)
        and then not Self.Source.Is_Parsed
      then
         declare
            Art : constant Artifact.Object := Self.Artifacts;
            LI  : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;
         begin
            --  Let's check if we have a dependency for this file

            if Art.Has_Dependency and then Art.Dependency.Exists then
               LI := Art.Dependency;
            end if;

            --  If LI is present then we can parse it to get the source
            --  information.

            if LI.Is_Defined then
               --  ??? check if LI file is more recent than source
               declare
                  Backend : constant not null access
                              Source_Info.Parser.Object'Class :=
                                Source_Info.Parser.Registry.Get
                                  (Language, Source_Info.LI);
               begin
                  Source_Info.Object (Self.Source).Reset;

                  Source_Info.Parser.Compute
                    (Self   => Backend,
                     Data   => Source_Info.Object'Class (Self.Source),
                     LI     => LI,
                     Source => Self.Source);

                  if Source_Info.Object'Class (Self.Source).Is_Parsed then
                     return;
                  end if;
               end;
            end if;
         end;
      end if;

      --  If no LI file (source may not be compiled yet) we default to
      --  parsing with a source parser.

      declare
         use type GPR2.Source_Info.Implemented_Backend;
         use all type GPR2.Unit.Kind_Type;
         --  At this point, all sources have been loaded into the
         --  view and so we know the relation between unit and
         --  spec/body/separate. We can then update the kind to
         --  S_Spec_Only or S_Body_Only accordingly.
         --
         --  We do that here only after a source parser as a LI based parser
         --  does that already.
      begin
         Self.Source.Update;

         if Self.Source.Is_Parsed
           and then Self.Source.Used_Backend = Source_Info.Source
           and then Self.Source.Has_Units
           and then Self.Source.Has_Single_Unit
           and then View (Self).Units.Contains (Self.Source.Unit_Name)
         then
            declare
               Unit : constant Unit_Info.Object :=
                        View (Self).Unit (Self.Source.Unit_Name);
            begin
               --  If a runtime source the unit is not defined

               if Self.Source.Kind = S_Spec
                 and then not Unit.Has_Body
               then
                  Self.Source.Update_Kind (S_Spec_Only);

               elsif Self.Source.Kind = S_Body
                 and then not Unit.Has_Spec
               then
                  Self.Source.Update_Kind (S_Body_Only);
               end if;
            end;
         end if;
      end;
   end Update;

   ----------
   -- View --
   ----------

   function View (Self : Object) return Project.View.Object is
   begin
      return Definition.Strong (Self.View);
   end View;

end GPR2.Project.Source;
