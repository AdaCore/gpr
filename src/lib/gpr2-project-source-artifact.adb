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

with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Definition;
with GPR2.Source;

package body GPR2.Project.Source.Artifact is

   package PRA renames GPR2.Project.Registry.Attribute;

   function At_Suffix (At_Pos : Positive) return Filename_Type;
   --  Returns 'at' index from attribute value or index prefixed with '~'
   --  character to use in filenames.

   ---------------
   -- At_Suffix --
   ---------------

   function At_Suffix (At_Pos : Positive) return Filename_Type is
      Result : String :=  At_Pos'Img;
   begin
      Result (Result'First) := '~';
      return Filename_Type (Result);
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
     (Source     : Project.Source.Object;
      Force_Spec : Boolean := False) return Artifact.Object
   is
      Src  : constant GPR2.Source.Object := Source.Source;
      Main : constant GPR2.Project.Source.Object :=
               (if Source.Has_Other_Part
                  and then Source.Naming_Exception in Naming_Exception_Value
                  and then Src.Has_Single_Unit
                  and then Src.Kind = Unit.S_Spec
                  and then not Force_Spec
                then Source.Other_Part
                else Source);
      BN   : constant Filename_Type := Main.Path_Name.Base_Filename;
      Lang : constant Name_Type := Src.Language;
      View : constant Project.View.Object :=
               Definition.Strong (Source.View);

      O_Suffix   : constant Filename_Type := View.Tree.Object_Suffix (Lang);
      D_Suffix   : constant Filename_Type :=
                     View.Tree.Dependency_Suffix (Lang);
      C_Suffix   : constant Filename_Type := ".ci";
      P_Suffix   : constant Filename_Type := ".prep";
      S_Suffix   : constant Filename_Type := ".cswi";
      Cov_Suffix : constant Filename_Type := ".sid";

      Object_Files : Index_Path_Name_Map.Map;
      Deps_Lib     : Index_Path_Name_Map.Map;
      Deps_Obj     : Index_Path_Name_Map.Map;

      function From_Hierarchy
        (View         : Project.View.Object;
         Filename     : Filename_Type;
         Dir_Attr     : Name_Type;
         Full_Closure : Boolean := False) return GPR2.Path_Name.Object;
      --  Find Filename in directory defined in attribute Dir_Attr in this
      --  source view and in the extended views if the Source is inherited.
      --  If Full_Closure is set, then the full closure of the extended
      --  projects (in case of extends all) will be searched.

      procedure Insert_If_Defined
        (Map : in out Index_Path_Name_Map.Map;
         Index : Natural;
         Path  : GPR2.Path_Name.Object);
      --  If Path is defined, include it in Map with index Index, else do
      --  nothing.

      Preprocessed : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (Src.Path_Name.Simple_Name & P_Suffix,
                          Filename_Optional (View.Object_Directory.Value));

      Callgraph    : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (BN & C_Suffix,
                          Filename_Type (View.Object_Directory.Value));

      Coverage     : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (BN & Cov_Suffix,
                          Filename_Type (View.Object_Directory.Value));

      Switches     : constant GPR2.Path_Name.Object :=
                       GPR2.Path_Name.Create_File
                         (BN & S_Suffix,
                          Filename_Type (View.Object_Directory.Value));

      -------------------
      -- From_Hierarhy --
      -------------------

      function From_Hierarchy
        (View         : Project.View.Object;
         Filename     : Filename_Type;
         Dir_Attr     : Name_Type;
         Full_Closure : Boolean := False) return GPR2.Path_Name.Object is

         function Get_Candidate
           (View : Project.View.Object) return GPR2.Path_Name.Object;
         --  If View has Dir_Attr defined, then returns the candidate file
         --  within this directory. Returns Undefined otherwise.

         -------------------
         -- Get_Candidate --
         -------------------

         function Get_Candidate
           (View : Project.View.Object) return GPR2.Path_Name.Object is
         begin
            if View.Has_Attributes (Dir_Attr) then
               return GPR2.Path_Name.Create_File
                 (Filename,
                  Filename_Type
                    (Definition.Apply_Root_And_Subdirs
                         (View, Dir_Attr).Value));
            else
               return GPR2.Path_Name.Undefined;
            end if;
         end Get_Candidate;

         Source        : constant Project.Source.Object :=
                           View.Source
                             (Create.Source.Path_Name, Need_Update => False);
         Candidate     : GPR2.Path_Name.Object;
         New_Candidate : GPR2.Path_Name.Object;

      begin
         Candidate := Get_Candidate (View);

         if not Source.Inherited
           or else (Candidate.Is_Defined and then Candidate.Exists)
         then
            return Candidate;
         end if;

         if View.Is_Extending then
            if Full_Closure then
               for Ext of View.Extended loop
                  New_Candidate := From_Hierarchy
                    (Ext, Filename, Dir_Attr, True);

                  exit when New_Candidate.Is_Defined
                    and then New_Candidate.Exists;
               end loop;
            else
               New_Candidate := From_Hierarchy
                 (View.Extended_Root, Filename, Dir_Attr);
            end if;
         end if;

         if New_Candidate.Is_Defined and then New_Candidate.Exists then
            --  Found from hierarchy
            return New_Candidate;
         else
            --  Project's own candidate
            return Candidate;
         end if;
      end From_Hierarchy;

      -----------------------
      -- Insert_If_Defined --
      -----------------------

      procedure Insert_If_Defined
        (Map : in out Index_Path_Name_Map.Map;
         Index : Natural;
         Path  : GPR2.Path_Name.Object) is
      begin
         if Path.Is_Defined then
            Map.Insert (Index, Path);
         end if;
      end Insert_If_Defined;

   begin
      if Src.Has_Units and then Src.Has_Index then
         for CU of Src.Units loop
            if CU.Kind in GPR2.Unit.Body_Kind | GPR2.Unit.S_Spec_Only then
               declare
                  Base : constant Filename_Type := BN & At_Suffix (CU.Index);
               begin
                  if Source.Aggregated then
                     for View of Source.Aggregating_Views loop
                        Deps_Lib.Insert
                          (CU.Index,
                           GPR2.Path_Name.Create_File
                             (Base & D_Suffix,
                              Filename_Type
                                (View.Library_Ali_Directory.Value)));
                     end loop;
                  else
                     Insert_If_Defined
                       (Object_Files,
                        CU.Index,
                        From_Hierarchy
                          (View, Base & O_Suffix, PRA.Object_Dir, True));
                     Insert_If_Defined
                       (Deps_Lib,
                        CU.Index,
                        From_Hierarchy
                          (View,
                           Base & D_Suffix,
                           PRA.Library_Ali_Dir, True));
                     Insert_If_Defined
                       (Deps_Obj,
                        CU.Index,
                        From_Hierarchy
                          (View, Base & D_Suffix, PRA.Object_Dir, True));
                  end if;
               end;
            end if;
         end loop;

      elsif Source.Aggregated then
         --  For aggregated library the .ali is also copied into the
         --  aggregate library directory.
         for Agg_Lib of Source.Aggregating_Views loop
            Deps_Lib.Insert
              (1,
               GPR2.Path_Name.Create_File
                 (BN & D_Suffix,
                  Filename_Type (Agg_Lib.Library_Ali_Directory.Value)));
         end loop;

      else
         Insert_If_Defined
           (Object_Files,
            1,
            From_Hierarchy (View, BN & O_Suffix, PRA.Object_Dir, True));
         Insert_If_Defined
           (Deps_Lib,
            1,
            From_Hierarchy (View, BN & D_Suffix, PRA.Library_Ali_Dir, True));
         Insert_If_Defined
           (Deps_Obj,
            1,
            From_Hierarchy (View, BN & D_Suffix, PRA.Object_Dir, True));
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
      Result : GPR2.Path_Name.Set.Object := Self.List_To_Clean;
   begin
      if Self.Has_Coverage then
         Result.Append (Self.Coverage);
      end if;

      return Result;
   end List;

   -------------------
   -- List_To_Clean --
   -------------------

   function List_To_Clean (Self : Object) return GPR2.Path_Name.Set.Object is
      P_Source : constant GPR2.Project.Source.Object := Self.Source;
      Source   : constant GPR2.Source.Object := P_Source.Source;
      Lang     : constant Name_Type := Source.Language;
      View     : constant Project.View.Object :=
                   Definition.Strong (P_Source.View);
      Result   : GPR2.Path_Name.Set.Object;
      Name     : constant Filename_Type := Source.Path_Name.Simple_Name;
      O_Dir    : constant Filename_Type :=
                   Filename_Type (View.Object_Directory.Value);

      procedure Append_File (Name : Filename_Type);
      --  Append full filename constructed from Name and Object_Dir to result

      -----------------
      -- Append_File --
      -----------------

      procedure Append_File (Name : Filename_Type) is
      begin
         Result.Append (GPR2.Path_Name.Create_File (Name, O_Dir));
      end Append_File;

   begin
      for E of View.Source_Artifact_Extensions (Lang) loop
         Append_File (Name & Filename_Type (E));
      end loop;

      if not Self.Object_Files.Is_Empty then
         --  Object themselves

         for O of Self.Object_Files loop
            Result.Append (O);
         end loop;

         --  The generated artefacts

         Append_File (Name & ".stdout");
         Append_File (Name & ".stderr");

         Append_File (Source.Path_Name.Base_Filename & ".adt");

         for E of View.Object_Artifact_Extensions (Lang) loop
            Append_File (Source.Path_Name.Base_Filename & Filename_Type (E));
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

      return Result;
   end List_To_Clean;

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
