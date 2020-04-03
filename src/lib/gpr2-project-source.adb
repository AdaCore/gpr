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
with GPR2.Project.Unit_Info.Set;

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

   ---------------------------------
   -- Context_Clause_Dependencies --
   ---------------------------------

   procedure Context_Clause_Dependencies
     (Self     : Object;
      For_Each : not null access procedure
                   (Source : GPR2.Project.Source.Object);
      Closure  : Boolean := False)
   is
      View : constant Project.View.Object := Definition.Strong (Self.View);
      Tree : constant not null access Project.Tree.Object := View.Tree;

      Done : Containers.Name_Set;
      --  Fast look-up table to avoid analysing the same unit multiple time and
      --  more specifically avoid circularities.

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
            Position : Containers.Name_Type_Set.Cursor;
            Inserted : Boolean;
         begin
            Done.Insert (Item.Simple_Name, Position, Inserted);
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
               Done.Insert (W.Text, Done_Pos, Inserted);

               if Inserted then
                  Buf.Insert (W, Position, Inserted);
               end if;
            end loop;
         end loop;
      end To_Analyze;

      Data : constant Definition.Const_Ref := Definition.Get_RO (View);

   begin
      if Self.Has_Other_Part then
         To_Analyze (Self.Other_Part);
      end if;

      To_Analyze (Self);

      For_Every_Unit : while not Buf.Is_Empty loop
         declare
            W    : constant Source_Reference.Identifier.Object'Class :=
                     Buf.First_Element;
            View : Project.View.Object;
         begin
            --  Remove the unit just taken from the list

            Buf.Delete_First;

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
      Tree : constant not null access Project.Tree.Object :=
               Definition.Strong (Self.View).Tree;
      Done : Containers.Name_Set;

      procedure Collect_Source (Source : Source_Info.Object'Class);

      --------------------
      -- Collect_Source --
      --------------------

      procedure Collect_Source (Source : Source_Info.Object'Class) is
         S        : Project.Source.Object;
         Position : Containers.Name_Type_Set.Cursor;
         Inserted : Boolean;
         Src_File : GPR2.Path_Name.Object;
         View     : Project.View.Object;
      begin
         for File of Source.Dependencies loop
            Done.Insert (File, Position, Inserted);

            if Inserted then
               Src_File := GPR2.Path_Name.Create_File
                 (File, GPR2.Path_Name.No_Resolution);
               View := Tree.Get_View (Src_File);

               if View.Is_Defined then
                  S := View.Source (Src_File);

                  pragma Assert (S.Is_Defined, "Can't find " & String (File));

                  For_Each (S);

                  if Closure then
                     Collect_Source (S.Source);
                  end if;
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
      use all type GPR2.Unit.Library_Unit_Type;

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
            Kind      : constant GPR2.Unit.Library_Unit_Type := CU.Kind;
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

   procedure Update (Self : in out Object) is
      Language : constant Name_Type := Self.Source.Language;
   begin
      --  If we have a LI parser for this language, use it

      if Source_Info.Parser.Registry.Exists (Language, Source_Info.LI)
        and then not Self.Source.Is_Parsed
      then
         declare
            Art : constant Artifact.Object := Self.Artifacts;
            LI  : GPR2.Path_Name.Object;
         begin
            --  Let's check if we have a dependency for this file

            if Art.Has_Dependency then
               LI := Art.Dependency;
            end if;

            --  If LI is present then we can parse it to get the source
            --  information.

            if LI.Is_Defined and then LI.Exists then
               --  ??? check if LI file is more recent than source
               declare
                  Backend : constant not null access
                              Source_Info.Parser.Object'Class :=
                                Source_Info.Parser.Registry.Get
                                  (Language, Source_Info.LI);
               begin
                  Source_Info.Parser.Compute
                    (Self   => Backend,
                     Data   => Self.Source,
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
         use all type GPR2.Unit.Library_Unit_Type;
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
           and then View (Self).Units (Need_Update => False).Contains
                      (Self.Source.Unit_Name)
         then
            declare
               Unit : constant Unit_Info.Object :=
                        View (Self).Unit
                          (Self.Source.Unit_Name, Need_Update => False);
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
