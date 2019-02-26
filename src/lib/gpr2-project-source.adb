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

with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Source.Artifact;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Source_Reference.Set;
with GPR2.Source_Reference.Identifier;
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
      Has_Naming_Exception : Boolean) return Object is
   begin
      return Object'
        (Source, Definition.Weak (View), Is_Interface,
         Has_Naming_Exception);
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

      procedure To_Analyse (Src : GPR2.Project.Source.Object);
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

         for B of Unit.Bodies loop
            Insert (Deps, B);
         end loop;
      end Insert;

      View : constant Project.View.Object  := Definition.Strong (Self.View);
      Data : constant Definition.Const_Ref := Definition.Get_RO (View);

      Buf  : Source_Reference.Set.Object := Self.Source.Withed_Units;
      --  Buf contains units to be checked, this list is extended when looking
      --  for the full-closure. Using this list we avoid a recursive call.

      Done : Source_Reference.Set.Object;
      --  Fast look-up table to avoid analysing the same unit multiple time and
      --  more specifically avoid circularities.

      Deps : GPR2.Project.Source.Set.Object;
      --  The resulting source set

      ----------------
      -- To_Analyse --
      ----------------

      procedure To_Analyse (Src : GPR2.Project.Source.Object) is
      begin
         if Src.Is_Defined then
            for W of Src.Source.Withed_Units loop
               if not Done.Contains (W) and then not Buf.Contains (W) then
                  Buf.Insert (W);
               end if;
            end loop;
         end if;
      end To_Analyse;

   begin
      --  First we need to ensure that all views are up-to-date regarding the
      --  sources/unit. The sources are recomputed only if required.

      Data.Tree.Update_Sources;

      --  For Unit or Closure add dependencies from the other part

      if Mode in Unit | Closure then
         To_Analyse
           (Object'
              (Self.Source.Other_Part, Self.View,
               False, Self.Has_Naming_Exception));
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
                        SU := Data.Units (W.Text);

                        --  At least the dependencies are the spec and body of
                        --  the withed unit.

                        Insert (Deps, SU);

                        --  Finaly, for the Closure mode add the dependencies
                        --  of withed unit from the direct withed spec and
                        --  bodies.

                        if Mode = Closure then
                           To_Analyse (SU.Spec);

                           for B of SU.Bodies loop
                              To_Analyse (B);
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
   -- Has_Other_Part --
   --------------------

   function Has_Other_Part (Self : Object) return Boolean is
   begin
      return Self.Source.Other_Part.Is_Defined;
   end Has_Other_Part;

   -------------
   -- Is_Main --
   -------------

   function Is_Main (Self : Object) return Boolean is
      Path  : constant Path_Name.Object := Self.Source.Path_Name;
      View  : constant Project.View.Object := Definition.Strong (Self.View);
      Mains : constant Attribute.Object :=
                View.Attribute (Registry.Attribute.Main);
   begin
      return Mains.Has_Value (Value_Type (Path.Base_Name))
        or else Mains.Has_Value (Value_Type (Path.Simple_Name));
   end Is_Main;

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part (Self : Object) return Object is
   begin
      return Definition.Strong (Self.View).Source
               (Self.Source.Other_Part.Path_Name);
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
