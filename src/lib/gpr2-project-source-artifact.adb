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

with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Definition;
with GPR2.Source;

package body GPR2.Project.Source.Artifact is

   package PRA renames GPR2.Project.Registry.Attribute;

   function "&" (Left, Right : Name_Type) return Name_Type renames GPR2."&";
   --  ??? work around a strange visibility issue

   function At_Suffix (At_Num : Positive) return Name_Type;
   --  Returns 'at' index from attribute value or index prefixed with '~'
   --  character to use in filenames.

   ---------------
   -- At_Suffix --
   ---------------

   function At_Suffix (At_Num : Positive) return Name_Type is
      Result : String :=  At_Num'Img;
   begin
      Result (Result'First) := '~';
      return Name_Type (Result);
   end At_Suffix;

   ---------------
   -- Callgraph --
   ---------------

   function Callgraph (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Callgraph;
   end Callgraph;

   --------------
   -- Coverage --
   --------------

   function Coverage (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Coverage;
   end Coverage;

   ------------
   -- Create --
   ------------

   function Create
     (Source : Project.Source.Object) return Artifact.Object
   is
      Src  : constant GPR2.Source.Object := Source.Source;
      BN   : constant Name_Type := Src.Path_Name.Base_Name;
      Lang : constant Name_Type := Src.Language;
      View : constant Project.View.Object :=
               Definition.Strong (Source.View);

      O_Suffix   : constant Name_Type := View.Tree.Object_Suffix (Lang);
      D_Suffix   : constant Name_Type := View.Tree.Dependency_Suffix (Lang);
      C_Suffix   : constant Name_Type := ".ci";
      P_Suffix   : constant Name_Type := ".prep";
      S_Suffix   : constant Name_Type := ".cswi";
      Cov_Suffix : constant Name_Type := ".sid";

      Object_Files : Index_Path_Name_Map.Map;
      Deps_Lib     : Index_Path_Name_Map.Map;
      Deps_Obj     : Index_Path_Name_Map.Map;

      function From_Hierarchy
        (Filename, Dir_Attr : Name_Type) return GPR2.Path_Name.Object;
      --  Find Filename in directory defined in attribute Dir_Attr in this
      --  source view and in the extended views if the Source is inherited.

      Preprocessed : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (Src.Path_Name.Simple_Name & P_Suffix,
                          Optional_Name_Type (View.Object_Directory.Value));

      Callgraph    : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (BN & C_Suffix,
                          Optional_Name_Type (View.Object_Directory.Value));

      Coverage     : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (BN & Cov_Suffix,
                          Optional_Name_Type (View.Object_Directory.Value));

      Switches     : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (BN & S_Suffix,
                          Optional_Name_Type (View.Object_Directory.Value));

      -------------------
      -- From_Hierarhy --
      -------------------

      function From_Hierarchy
        (Filename, Dir_Attr : Name_Type) return GPR2.Path_Name.Object
      is
         View      : Project.View.Object   := Create.View;
         Source    : Project.Source.Object := Create.Source;
         Candidate : GPR2.Path_Name.Object;
      begin
         loop
            Candidate :=
              GPR2.Path_Name.Create_File
                (Filename,
                 Optional_Name_Type
                   (Definition.Apply_Root_And_Subdirs
                      (View, Dir_Attr).Value));

            exit when not Source.Inherited or else Candidate.Exists;
            View := View.Extended;

            exit when not View.Has_Attributes (Dir_Attr);
            Source := View.Source (Source.Path_Name, Need_Update => False);
         end loop;

         return Candidate;
      end From_Hierarchy;

   begin
      if Src.Has_Units and then Src.Has_Index then
         for CU of Src.Units loop
            if CU.Kind in GPR2.Unit.Body_Kind | GPR2.Unit.S_Spec_Only then
               declare
                  Base : constant Name_Type := BN & At_Suffix (CU.Index);
               begin
                  if Source.Aggregated then
                     Deps_Lib.Insert
                       (CU.Index,
                        GPR2.Path_Name.Create_File
                          (Base & D_Suffix,
                           Optional_Name_Type
                             (Source.Aggregating_View.Library_Ali_Directory
                              .Value)));

                  else
                     Object_Files.Insert
                       (CU.Index,
                        From_Hierarchy (Base & O_Suffix, PRA.Object_Dir));

                     if View.Is_Library and then Lang = "Ada" then
                        Deps_Lib.Insert
                          (CU.Index,
                           From_Hierarchy
                             (Base & D_Suffix, PRA.Library_Ali_Dir));
                     end if;

                     Deps_Obj.Insert
                       (CU.Index,
                        From_Hierarchy (Base & D_Suffix, PRA.Object_Dir));
                  end if;
               end;
            end if;
         end loop;

      elsif Source.Aggregated then
         --  For aggregated library the .ali is also copied into the
         --  aggregate library directory.

         Deps_Lib.Insert
           (1,
            GPR2.Path_Name.Create_File
              (BN & D_Suffix,
               Optional_Name_Type
                 (Source.Aggregating_View.Library_Ali_Directory.Value)));

      else
         Object_Files.Insert
           (1, From_Hierarchy (BN & O_Suffix, PRA.Object_Dir));

         if View.Is_Library and then Lang = "Ada" then
            Deps_Lib.Insert
              (1, From_Hierarchy (BN & D_Suffix, PRA.Library_Ali_Dir));
         end if;

         Deps_Obj.Insert
           (1, From_Hierarchy (BN & D_Suffix, PRA.Object_Dir));
      end if;

      return Artifact.Object'
        (Source           => Source,
         Object_Files     => Object_Files,
         Deps_Lib_Files   => Deps_Lib,
         Deps_Obj_Files   => Deps_Obj,
         Switches         => Switches,
         Preprocessed_Src => Preprocessed,
         Callgraph        => Callgraph,
         Coverage         => Coverage);
   end Create;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self     : Artifact.Object;
      Index    : Natural;
      Location : Dependency_Location := In_Both)
      return GPR2.Path_Name.Object is
   begin
      if Index = 0 then
         case Location is
            when In_Library =>
               return Self.Deps_Lib_Files.First_Element;

            when In_Objects =>
               return Self.Deps_Obj_Files.First_Element;

            when In_Both    =>
               return
                 (if Self.Deps_Lib_Files.Is_Empty
                  then Self.Deps_Obj_Files.First_Element
                  elsif Self.Deps_Obj_Files.Is_Empty
                  then Self.Deps_Lib_Files.First_Element
                  elsif Self.Deps_Lib_Files.First_Element.Exists
                  then Self.Deps_Lib_Files.First_Element
                  elsif Self.Deps_Obj_Files.First_Element.Exists
                  then Self.Deps_Obj_Files.First_Element
                  else Self.Deps_Lib_Files.First_Element);
         end case;

      else
         case Location is
            when In_Library =>
               return Self.Deps_Lib_Files (Index);

            when In_Objects =>
               return Self.Deps_Obj_Files (Index);

            when In_Both    =>
               return
                 (if not Self.Deps_Lib_Files.Contains (Index)
                  then Self.Deps_Obj_Files (Index)
                  elsif not Self.Deps_Obj_Files.Contains (Index)
                  then Self.Deps_Lib_Files (Index)
                  elsif Self.Deps_Lib_Files (Index).Exists
                  then Self.Deps_Lib_Files (Index)
                  elsif Self.Deps_Obj_Files (Index).Exists
                  then Self.Deps_Obj_Files (Index)
                  else Self.Deps_Lib_Files (Index));
         end case;
      end if;
   end Dependency;

   --------------------
   -- Has_Dependency --
   --------------------

   function Has_Dependency
     (Self     : Object;
      Index    : Natural             := 0;
      Location : Dependency_Location := In_Both) return Boolean is
   begin
      if Index = 0 then
         return
           (case Location is
               when In_Objects => not Self.Deps_Obj_Files.Is_Empty,
               when In_Library => not Self.Deps_Lib_Files.Is_Empty,
               when In_Both    => not (Self.Deps_Lib_Files.Is_Empty
                                       and then Self.Deps_Obj_Files.Is_Empty));
      else
         return
           (case Location is
               when In_Objects => Self.Deps_Obj_Files.Contains (Index),
               when In_Library => Self.Deps_Lib_Files.Contains (Index),
               when In_Both    => Self.Deps_Lib_Files.Contains (Index)
                                  or else Self.Deps_Obj_Files.Contains
                                            (Index));
      end if;
   end Has_Dependency;

   ----------
   -- List --
   ----------

   function List (Self : Object) return GPR2.Path_Name.Set.Object is
      P_Source : constant GPR2.Project.Source.Object := Self.Source;
      Source   : constant GPR2.Source.Object := P_Source.Source;
      Lang     : constant Name_Type := Source.Language;
      View     : constant Project.View.Object :=
                   Definition.Strong (P_Source.View);
      Result   : GPR2.Path_Name.Set.Object;
      Name     : constant Name_Type := Source.Path_Name.Simple_Name;
      O_Dir    : constant Optional_Name_Type :=
                   Optional_Name_Type (View.Object_Directory.Value);

      procedure Append_File (Name : Name_Type);
      --  Append full filename constructed from Name and Object_Dir to result

      -----------------
      -- Append_File --
      -----------------

      procedure Append_File (Name : Name_Type) is
      begin
         Result.Append (GPR2.Path_Name.Create_File (Name, O_Dir));
      end Append_File;

   begin
      for E of View.Source_Artifact_Extensions (Lang) loop
         Append_File (Name & Name_Type (E));
      end loop;

      if not Self.Object_Files.Is_Empty then
         --  Object themselves

         for O of Self.Object_Files loop
            Result.Append (O);
         end loop;

         --  The generated artefacts

         Append_File (Name & ".stdout");
         Append_File (Name & ".stderr");

         Append_File (Source.Path_Name.Base_Name & ".adt");

         for E of View.Object_Artifact_Extensions (Lang) loop
            Append_File (Source.Path_Name.Base_Name & Name_Type (E));
         end loop;
      end if;

      --  Append the dependencies

      for D of Self.Deps_Lib_Files loop
         Result.Append (D);
      end loop;

      for D of Self.Deps_Obj_Files loop
         Result.Append (D);
      end loop;

      --  Append preprocessed sources

      if Self.Has_Preprocessed_Source then
         Result.Append (Self.Preprocessed_Source);
      end if;

      if Self.Switches.Is_Defined and then Self.Switches.Exists then
         Result.Append (Self.Switches);
      end if;

      if Self.Has_Callgraph then
         Result.Append (Self.Callgraph);
      end if;

      if Self.Has_Coverage then
         Result.Append (Self.Coverage);
      end if;

      return Result;
   end List;

   -----------------
   -- Object_Code --
   -----------------

   function Object_Code
     (Self  : Artifact.Object;
      Index : Natural) return GPR2.Path_Name.Object is
   begin
      return (if Index = 0
              then Self.Object_Files.First_Element
              else Self.Object_Files (Index));
   end Object_Code;

   -------------------------
   -- Preprocessed_Source --
   -------------------------

   function Preprocessed_Source
     (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Preprocessed_Src;
   end Preprocessed_Source;

   ------------
   -- Source --
   ------------

   function Source (Self : Object) return GPR2.Project.Source.Object is
   begin
      return Self.Source;
   end Source;

end GPR2.Project.Source.Artifact;
