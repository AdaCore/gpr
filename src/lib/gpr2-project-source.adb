------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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
with GPR2.Source_Reference.Set;
with GPR2.Source_Reference.Identifier;
with GPR2.Unit;

package body GPR2.Project.Source is

   ---------------
   -- Artifacts --
   ---------------

   function Artifacts (Self : Object) return Artifact.Object is

      Src  : constant Name_Type := Self.Source.Path_Name.Base_Name;
      Lang : constant Name_Type := Self.Source.Language;
      View : constant GPR2.Project.View.Object := Self.View;

      function Artifact_Dir return GPR2.Path_Name.Object is
        (if View.Kind = K_Library
         then View.Library_Directory
         else View.Object_Directory);
      --  The directory where artifacts are written for this source

      O_Suffix   : constant Optional_Name_Type :=
                     (if View.Tree.Has_Configuration
                      then View.Tree.Configuration.Object_File_Suffix (Lang)
                      else ".o");

      D_Suffix   : constant Optional_Name_Type :=
                     (if View.Tree.Has_Configuration
                      then View.Tree.
                             Configuration.Dependency_File_Suffix (Lang)
                      elsif Lang = "ada" then ".ali" else ".d");

      P_Suffix   : constant Optional_Name_Type := ".prep";

      Object       : constant Path_Name.Object :=
                       Path_Name.Create_File
                         (Src & O_Suffix,
                          Optional_Name_Type (Artifact_Dir.Value));

      Dependency   : constant Path_Name.Object :=
                       Path_Name.Create_File
                         (Src & D_Suffix,
                          Optional_Name_Type (Artifact_Dir.Value));

      Preprocessed : constant Path_Name.Object :=
                       Path_Name.Create_File
                         (Src & P_Suffix,
                          Optional_Name_Type (View.Object_Directory.Value));
   begin
      return Artifact.Create (Self, Object, Dependency, Preprocessed);
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
      return Object'(Source, View, Is_Interface, Has_Naming_Exception);
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
         Unit : GPR2.Unit.Object) with Inline;
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
         if Unit.Spec /= Undefined then
            --  function and procedure compilation units allowed to do not have
            --  a spec.
            Insert (Deps, Unit.Spec);
         end if;

         for B of Unit.Bodies loop
            Insert (Deps, B);
         end loop;
      end Insert;

      Data : constant Definition.Data := Definition.Get (Self.View);

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
         if Src /= GPR2.Project.Source.Undefined then
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
            SU   : GPR2.Unit.Object;
         begin
            --  Remove the unit just taken from the list

            Buf.Delete_First;

            if not Done.Contains (W) then
               Done.Include (W);

               View := Data.Tree.Get_View (Unit => W.Identifier);

               if View = Project.View.Undefined then
                  Data.Tree.Log_Messages.Append
                    (Message.Create
                       (Message.Warning,
                        "withed unit " & String (W.Identifier) & " not found",
                        W));

               else
                  declare
                     Data : constant Definition.Data := Definition.Get (View);
                     --  The view information for the unit Identifier
                  begin
                     if Data.Units.Contains (W.Identifier) then
                        SU := Data.Units (W.Identifier);

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
      return Self.Source.Other_Part /= GPR2.Source.Undefined;
   end Has_Other_Part;

   ----------------
   -- Other_Part --
   ----------------

   function Other_Part (Self : Object) return Object is
   begin
      return Self.View.Source (Self.Source.Other_Part.Path_Name);
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
      return Self.View;
   end View;

end GPR2.Project.Source;
