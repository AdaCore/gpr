--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Project.Tree;
with GPR2.Project.Definition;

package body GPR2.Project.Source.Artifact is

   type Artifact_Dir is (Object_Dir, Library_ALI_Dir);

   function At_Suffix (At_Pos : Unit_Index) return Filename_Type;
   --  Returns 'at' index from attribute value or index prefixed with '~'
   --  character to use in filenames.

   function From_Hierarchy
     (View         : Project.View.Object;
      Source       : Project.Source.Object;
      Filename     : Filename_Type;
      Dir_Attr     : Artifact_Dir;
      Full_Closure : Boolean := False) return GPR2.Path_Name.Object
     with Pre => View.Is_Defined and then Source.Is_Defined;
   --  Find Filename in directory defined in attribute Dir_Attr in this
   --  source view and in the extended views if the Source is inherited.
   --  If Full_Closure is set, then the full closure of the extended
   --  projects (in case of extends all) will be searched.

   procedure Insert_If_Defined
     (Map   : in out Index_Path_Name_Map.Map;
      Index : Unit_Index;
      Path  : GPR2.Path_Name.Object);
   --  If Path is defined, include it in Map with index Index, else do
   --  nothing.

   ---------------
   -- At_Suffix --
   ---------------

   function At_Suffix (At_Pos : Unit_Index) return Filename_Type is
      Result : String :=  At_Pos'Img;
   begin
      Result (Result'First) := '~';
      return Filename_Type (Result);
   end At_Suffix;

   ---------------
   -- Callgraph --
   ---------------

   function Callgraph
     (Self  : Object;
      Index : Unit_Index := No_Index) return GPR2.Path_Name.Object is
   begin
      return Self.Callgraph (Index);
   end Callgraph;

   --------------
   -- Coverage --
   --------------

   function Coverage
     (Self  : Object;
      Index : Unit_Index := No_Index) return GPR2.Path_Name.Object is
   begin
      return Self.Coverage (Index);
   end Coverage;

   ------------
   -- Create --
   ------------

   function Create
     (Source     : Project.Source.Object;
      Force_Spec : Boolean := False;
      Filter     : Artifact.Filter := All_Artifacts) return Artifact.Object
   is
      procedure Get_Object_Artifacts
        (BN      : Filename_Type;
         Index   : Unit_Index);
      --  Retrieve object and ali files for a given unit, if any

      procedure Get_Source_Artifacts;
      --  Retrieve preprocessed source, callgraph, coverage and switches
      --  artifacts

      Lang         : constant Language_Id := Source.Language;
      View         : constant Project.View.Object :=
                       Definition.Strong (Source.View);

      O_Suffix     : constant Filename_Type := View.Tree.Object_Suffix (Lang);
      D_Suffix     : constant Filename_Type :=
                       View.Tree.Dependency_Suffix (Lang);
      C_Suffix     : constant Filename_Type := ".ci";
      P_Suffix     : constant Filename_Type := ".prep";
      S_Suffix     : constant Filename_Type := ".cswi";
      Cov_Suffix   : constant Filename_Type := ".sid";

      Object_Files : Index_Path_Name_Map.Map;
      Deps_Lib     : Index_Path_Name_Map.Map;
      Deps_Obj     : Index_Path_Name_Map.Map;

      Preprocessed : GPR2.Path_Name.Object;
      Callgraph    : Index_Path_Name_Map.Map;
      Coverage     : Index_Path_Name_Map.Map;
      Switches     : Index_Path_Name_Map.Map;

      --------------------------
      -- Get_Object_Artifacts --
      --------------------------

      procedure Get_Object_Artifacts
        (BN    : Filename_Type;
         Index : Unit_Index) is
      begin
         if Filter (Dependency_File) then
            if Force_Spec then
               Deps_Lib.Include
                 (Index, GPR2.Path_Name.Create_File (BN & D_Suffix));
            else
               Insert_If_Defined
                 (Deps_Lib,
                  Index,
                  From_Hierarchy
                    (View, Source, BN & D_Suffix, Library_ALI_Dir, True));
            end if;
         end if;

         if View.Kind /= K_Aggregate_Library then
            if Filter (Object_File) then
               if Force_Spec then
                  Object_Files.Include
                    (Index, GPR2.Path_Name.Create_File (BN & O_Suffix));
               else
                  Insert_If_Defined
                    (Object_Files,
                     Index,
                     From_Hierarchy
                       (View, Source, BN & O_Suffix, Object_Dir, True));
               end if;
            end if;

            if Filter (Dependency_File) then
               if Force_Spec then
                  Deps_Obj.Include
                    (Index, GPR2.Path_Name.Create_File (BN & D_Suffix));
               else
                  Insert_If_Defined
                    (Deps_Obj,
                     Index,
                     From_Hierarchy
                       (View, Source, BN & D_Suffix, Object_Dir, True));
               end if;
            end if;

            if Filter (Artifact.Callgraph) then
               Callgraph.Include
                 (Index,
                  GPR2.Path_Name.Create_File
                    (BN & C_Suffix,
                     Filename_Type (View.Object_Directory.Value)));
            end if;

            if Filter (Artifact.Coverage) then
               Coverage.Include
                 (Index,
                  GPR2.Path_Name.Create_File
                    (BN & Cov_Suffix,
                     Filename_Type (View.Object_Directory.Value)));
            end if;

            if Filter (Artifact.Switches) then
               Switches.Include
                 (Index,
                  GPR2.Path_Name.Create_File
                    (BN & S_Suffix,
                     Filename_Type (View.Object_Directory.Value)));
            end if;
         end if;
      end Get_Object_Artifacts;

      --------------------------
      -- Get_Source_Artifacts --
      --------------------------

      procedure Get_Source_Artifacts is
      begin
         if Filter (Preprocessed_Source) then
            Preprocessed := GPR2.Path_Name.Create_File
              (Source.Path_Name.Simple_Name & P_Suffix,
               Filename_Optional (View.Object_Directory.Value));
         end if;
      end Get_Source_Artifacts;

      BN : constant Filename_Type := Source.Path_Name.Base_Filename;

   begin
      if Source.Has_Units then
         for CU of Source.Units loop
            --  Only consider object artifacts for the body part or if the
            --  spec has no body, except when Force_Spec is set (in which case
            --  don't check anything and just return the simple name)..
            if (CU.Kind in
                  GPR2.Unit.Body_Kind | GPR2.Unit.S_Spec_Only)
              or else (Force_Spec and then CU.Kind = GPR2.Unit.S_Spec)
            then
               declare
                  Base : constant Filename_Type :=
                           (if Source.Has_Index
                            then BN & At_Suffix (CU.Index)
                            else BN);
               begin
                  Get_Object_Artifacts (Base, CU.Index);
               end;
            end if;
         end loop;

      else
         Get_Object_Artifacts (BN, No_Index);
      end if;

      Get_Source_Artifacts;

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
     (Source        : Project.Source.Object;
      Index         : Unit_Index          := No_Index;
      Location      : Dependency_Location := In_Both;
      Actual_File   : Boolean             := False;
      Maybe_No_Body : Boolean             := False)
      return GPR2.Path_Name.Object
   is
      function Get_Dep (F : Filename_Type) return GPR2.Path_Name.Object;

      BN       : constant Filename_Type :=
                   Source.Path_Name.Base_Filename;
      Lang     : constant Language_Id := Source.Language;
      View     : constant Project.View.Object :=
                   Definition.Strong (Source.View);
      D_Suffix : constant Filename_Type :=
                   View.Tree.Dependency_Suffix (Lang);

      -------------
      -- Get_Dep --
      -------------

      function Get_Dep (F : Filename_Type) return GPR2.Path_Name.Object is
         Candidate, Candidate2 : GPR2.Path_Name.Object;
      begin
         if Location in In_Both | In_Library then
            Candidate := From_Hierarchy
              (View,
               Source,
               F & D_Suffix,
               Library_ALI_Dir, True);
         end if;

         if Location = In_Library then
            if not Actual_File then
               return Candidate;
            elsif Candidate.Is_Defined
              and then Candidate.Exists
            then
               return Candidate;
            else
               return GPR2.Path_Name.Undefined;
            end if;
         end if;

         Candidate2 := From_Hierarchy
           (View,
            Source,
            F & D_Suffix,
            Object_Dir, True);

         --  Priorities:
         --  1- return the location, if set to a unique one
         --  1b- if Location is Both, always favor the Library_ALI_Dir one
         --  2- if Actual_File is set, return immediately if one is found, else
         --     return Undefined.
         --  3- if one of the location is undefined, return the other
         --  4- if both are defined, return the one that exists
         --  5- return the Library_Ali_Dir path
         if Location = In_Objects then
            if not Actual_File then
               return Candidate2;
            elsif Candidate2.Is_Defined and then Candidate2.Exists then
               return Candidate2;
            else
               return GPR2.Path_Name.Undefined;
            end if;
         --  At this point, Location is In_Both, and no if Actual_File is set
         --  then no LI file exists.

         elsif Actual_File then
            if Candidate.Is_Defined and then Candidate.Exists then
               return Candidate;
            elsif Candidate2.Is_Defined and then Candidate2.Exists then
               return Candidate2;
            else
               return GPR2.Path_Name.Undefined;
            end if;

         elsif not Candidate2.Is_Defined then
            return Candidate;
         elsif not Candidate.Is_Defined then
            return Candidate2;
         elsif Candidate.Exists then
            return Candidate;
         elsif Candidate2.Exists then
            return Candidate2;
         else
            return Candidate;
         end if;
      end Get_Dep;

   begin
      if Source.Has_Units and then Source.Has_Index then
         declare
            CU : GPR2.Unit.Object renames Source.Unit (Index);
            Other : GPR2.Project.Source.Source_Part;
         begin
            if CU.Kind in GPR2.Unit.Body_Kind | GPR2.Unit.S_Spec_Only
              or else (Maybe_No_Body and then CU.Kind = GPR2.Unit.S_Spec)
            then
               declare
                  Base : constant Filename_Type :=
                           BN & At_Suffix (Index);
               begin
                  return Get_Dep (Base);
               end;
            elsif Source.Has_Other_Part (Index) then
               Other := Source.Other_Part (Index);
               return Dependency
                 (Other.Source,
                  Other.Index,
                  Location,
                  Actual_File,
                  Maybe_No_Body);
            else
               return GPR2.Path_Name.Undefined;
            end if;
         end;
      else
         return Get_Dep (BN);
      end if;
   end Dependency;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self     : Object;
      Index    : Unit_Index          := No_Index;
      Location : Dependency_Location := In_Both)
      return GPR2.Path_Name.Object is
   begin
      if Index <= 1 then
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

   -------------------
   -- From_Hierarhy --
   -------------------

   function From_Hierarchy
     (View         : Project.View.Object;
      Source       : Project.Source.Object;
      Filename     : Filename_Type;
      Dir_Attr     : Artifact_Dir;
      Full_Closure : Boolean := False) return GPR2.Path_Name.Object
   is
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
         case Dir_Attr is
            when Object_Dir =>
               if View.Kind not in K_Configuration | K_Abstract then
                  return View.Object_Directory.Compose (Filename);
               else
                  return GPR2.Path_Name.Undefined;
               end if;

            when Library_ALI_Dir =>
               if View.Is_Library then
                  return View.Library_Ali_Directory.Compose (Filename);
               else
                  return GPR2.Path_Name.Undefined;
               end if;
         end case;
      end Get_Candidate;

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
               declare
                  Ext_Source : Project.Source.Object renames
                                 Ext.Source (Source.Path_Name);
               begin
                  if Ext_Source.Is_Defined then
                     New_Candidate := From_Hierarchy
                       (Ext, Ext_Source, Filename, Dir_Attr, True);

                     exit when New_Candidate.Is_Defined
                       and then New_Candidate.Exists;
                  end if;
               end;
            end loop;
         else
            New_Candidate := From_Hierarchy
              (View.Extended_Root,
               View.Extended_Root.Source (Source.Path_Name),
               Filename, Dir_Attr);
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

   --------------------
   -- Has_Dependency --
   --------------------

   function Has_Dependency
     (Self     : Object;
      Index    : Unit_Index          := No_Index;
      Location : Dependency_Location := In_Both) return Boolean is
   begin
      if Index = No_Index then
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

   -----------------------
   -- Insert_If_Defined --
   -----------------------

   procedure Insert_If_Defined
     (Map   : in out Index_Path_Name_Map.Map;
      Index : Unit_Index;
      Path  : GPR2.Path_Name.Object) is
   begin
      if Path.Is_Defined then
         Map.Insert (Index, Path);
      end if;
   end Insert_If_Defined;

   ----------
   -- List --
   ----------

   function List (Self : Object) return GPR2.Path_Name.Set.Object is
      Result : GPR2.Path_Name.Set.Object := Self.List_To_Clean;
   begin
      if Self.Has_Coverage then
         for C of Self.Coverage loop
            Result.Append (C);
         end loop;
      end if;

      return Result;
   end List;

   -------------------
   -- List_To_Clean --
   -------------------

   function List_To_Clean (Self : Object) return GPR2.Path_Name.Set.Object is
      Source : constant GPR2.Project.Source.Object := Self.Source;
      Lang   : constant Language_Id := Source.Language;
      View   : constant Project.View.Object :=
                 Definition.Strong (Source.View);
      Name   : constant Filename_Type := Source.Path_Name.Simple_Name;
      O_Dir  : constant Filename_Type :=
                 Filename_Type (View.Object_Directory.Value);
      Result : GPR2.Path_Name.Set.Object;

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

      for S of Self.Switches loop
         if S.Exists then
            Result.Append (S);
         end if;
      end loop;

      for C of Self.Callgraph loop
         if C.Exists then
            Result.Append (C);
         end if;
      end loop;

      return Result;
   end List_To_Clean;

   -----------------
   -- Object_Code --
   -----------------

   function Object_Code
     (Self  : Object;
      Index : Unit_Index := No_Index) return GPR2.Path_Name.Object is
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
