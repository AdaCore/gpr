------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GPR2.Compilation_Unit;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Definition;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Source_Reference;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Identifier.Set;
with GPR2.Unit;

package body GPR2.Project.Source is

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
      Extending_View       : Project.View.Object := Project.View.Undefined)
      return Object is
   begin
      return Object'
        (Source, Definition.Weak (View), Definition.Weak (Extending_View),
         Is_Interface, Has_Naming_Exception);
   end Create;

   ------------------
   -- Dependencies --
   ------------------

   function Dependencies
     (Self : Object;
      Mode : Dependency := Direct) return GPR2.Project.Source.Set.Object
   is
      procedure Insert
        (Deps : in out GPR2.Project.Source.Set.Object;
         Unit : GPR2.Unit.Object)
        with Inline, Pre => Unit.Is_Defined;
      --  Insert Unit into Deps (result of this routine)

      procedure To_Analyze (Src : GPR2.Project.Source.Object);
      --  Record Src's withed units to be analysed (insert into Buf)

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Deps : in out GPR2.Project.Source.Set.Object;
         Unit : GPR2.Unit.Object)
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
            Insert (Deps, Unit.Spec);
         end if;

         if Unit.Has_Body then
            Insert (Deps, Unit.Main_Body);
         end if;

         for Sep of Unit.Separates loop
            Insert (Deps, Sep);
         end loop;
      end Insert;

      View : constant Project.View.Object  := Definition.Strong (Self.View);
      Data : constant Definition.Const_Ref := Definition.Get_RO (View);

      Buf : Source_Reference.Identifier.Set.Object :=
              Self.Source.With_Clauses;
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
         if Src.Is_Defined then
            for CU of Src.Source.Compilation_Units loop
               for W of CU.Withed_Units loop
                  if not Done.Contains (W) and then not Buf.Contains (W) then
                     Buf.Include (W);
                  end if;
               end loop;
            end loop;
         end if;
      end To_Analyze;

   begin
      --  First we need to ensure that all views are up-to-date regarding the
      --  sources/unit. The sources are recomputed only if required.

      Data.Tree.Update_Sources;

      --  For Unit or Closure add dependencies from the other part

      if Mode in Unit | Closure then
         if Self.Source.Has_Single_Unit and then Self.Has_Other_Part then
            To_Analyze (Self.Other_Part);
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
                     SU   : GPR2.Unit.Object;
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
                           To_Analyze (SU.Spec);
                           To_Analyze (SU.Main_Body);

                           for Sep of SU.Separates loop
                              To_Analyze (Sep);
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
      return Definition.Strong (Self.Extending_View);
   end Extending_View;

   ------------------------
   -- Has_Extending_View --
   ------------------------

   function Has_Extending_View (Self : Object) return Boolean is
   begin
      return Definition.Strong (Self.Extending_View).Is_Defined;
   end Has_Extending_View;

   --------------------
   -- Has_Other_Part --
   --------------------

   function Has_Other_Part (Self : Object) return Boolean is
   begin
      return Self.Source.Has_Units
        and then Self.Source.Has_Single_Unit
        and then Self.Source.Other_Part.Is_Defined;
   end Has_Other_Part;

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

   function Other_Part (Self : Object) return Object is
      Source : constant GPR2.Source.Object := Self.Source.Other_Part;
   begin
      if Source.Is_Defined then
         return Definition.Strong (Self.View).Source (Source.Path_Name);
      else
         return Undefined;
      end if;
   end Other_Part;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Object) is
   begin
      Self.Source.Release;
   end Release;

   ------------
   -- Source --
   ------------

   function Source (Self : Object) return GPR2.Source.Object is
   begin
      return Self.Source;
   end Source;

   ----------
   -- View --
   ----------

   function View (Self : Object) return Project.View.Object is
   begin
      return Definition.Strong (Self.View);
   end View;

end GPR2.Project.Source;
