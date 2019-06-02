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

with Ada.Strings.Fixed;

with GPR2.Project.Tree;
with GPR2.Project.Definition;

package body GPR2.Project.Source.Artifact is

   function "&" (Left, Right : Name_Type) return Name_Type renames GPR2."&";
   --  ??? work around a strange visibility issue

   ------------
   -- Create --
   ------------

   function Create
     (Source : Project.Source.Object) return Artifact.Object
   is
      Src  : constant Name_Type := Source.Source.Path_Name.Base_Name;
      Lang : constant Name_Type := Source.Source.Language;
      View : constant Project.View.Object :=
               (if Source.Has_Extending_View
                then Source.Extending_View
                elsif Source.Has_Aggregating_View
                then Source.Aggregating_View
                else Definition.Strong (Source.View));

      O_Suffix : constant Name_Type := View.Tree.Object_Suffix (Lang);
      D_Suffix : constant Name_Type := View.Tree.Dependency_Suffix (Lang);
      P_Suffix : constant Name_Type := ".prep";
      S_Suffix : constant Name_Type := ".cswi";

      Object_Files     : Index_Path_Name_Map.Map;
      Dependency_Files : Index_Path_Name_Map.Map;

      Preprocessed : constant Path_Name.Object :=
                       Path_Name.Create_File
                         (Src & P_Suffix,
                          Optional_Name_Type (View.Object_Directory.Value));

      Switches     : constant Path_Name.Object :=
                       Path_Name.Create_File
                         (Src & S_Suffix,
                          Optional_Name_Type (View.Object_Directory.Value));

      Art_Dir      : constant Path_Name.Object :=
                       (if View.Kind in K_Aggregate_Library | K_Library
                        then View.Library_Directory
                        else View.Object_Directory);

      Idx          : Positive := 1;

   begin
      if not Source.Source.Has_Units or else Source.Source.Has_Single_Unit then
         if View.Kind in K_Aggregate_Library | K_Library then
            Object_Files.Insert
              (Idx, Path_Name.Create_File
                 (Src & O_Suffix,
                  Optional_Name_Type (View.Library_Directory.Value)));

            Dependency_Files.Insert
              (1, Path_Name.Create_File
                 (Src & D_Suffix,
                  Optional_Name_Type (View.Library_Directory.Value)));

            Idx := Idx + 1;
         end if;

         Object_Files.Insert
           (Idx, Path_Name.Create_File
              (Src & O_Suffix,
               Optional_Name_Type (View.Object_Directory.Value)));

         Dependency_Files.Insert
           (Idx, Path_Name.Create_File
              (Src & D_Suffix,
               Optional_Name_Type (View.Object_Directory.Value)));

      else
         for CU of Source.Source.Compilation_Units loop
            if CU.Kind = S_Body then
               declare
                  use Ada.Strings;
                  Index_Suffix : constant Name_Type :=
                                   "~" & Name_Type
                                     (Fixed.Trim (CU.Index'Image, Left));
               begin
                  Object_Files.Insert
                    (CU.Index,
                     Path_Name.Create_File
                       (Src & Index_Suffix & O_Suffix,
                        Optional_Name_Type (Art_Dir.Value)));
                  Dependency_Files.Insert
                    (CU.Index,
                     Path_Name.Create_File
                       (Src & Index_Suffix & D_Suffix,
                        Optional_Name_Type (Art_Dir.Value)));

                  Idx := Idx + 1;
               end;
            end if;
         end loop;

         --  Adds secondary object code if needed

         if View.Kind in K_Aggregate_Library | K_Library then
            for CU of Source.Source.Compilation_Units loop
               if CU.Kind = S_Body then
                  declare
                     use Ada.Strings;
                     Index_Suffix : constant Name_Type :=
                                      "~" & Name_Type
                                        (Fixed.Trim (CU.Index'Image, Left));
                  begin
                     Object_Files.Insert
                       (Idx,
                        Path_Name.Create_File
                          (Src & Index_Suffix & O_Suffix,
                           Optional_Name_Type (View.Object_Directory.Value)));
                     Dependency_Files.Insert
                       (Idx,
                        Path_Name.Create_File
                          (Src & Index_Suffix & D_Suffix,
                           Optional_Name_Type (View.Object_Directory.Value)));
                     Idx := Idx + 1;
                  end;
               end if;
            end loop;
         end if;
      end if;

      return Artifact.Object'
        (Source           => Source,
         Object_Files     => Object_Files,
         Dependency_Files => Dependency_Files,
         Switches         => Switches,
         Preprocessed_Src => Preprocessed);
   end Create;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Self : Artifact.Object; Index : Natural := 1) return Path_Name.Object is
   begin
      return Self.Dependency_Files (Index);
   end Dependency;

   ----------
   -- List --
   ----------

   function List (Self : Object) return Path_Name.Set.Object is
      Source : constant GPR2.Source.Object := Self.Source.Source;
      View   : constant Project.View.Object :=
                 (if Self.Source.Has_Extending_View
                  then Self.Source.Extending_View
                  elsif Self.Source.Has_Aggregating_View
                  then Self.Source.Aggregating_View
                  else Definition.Strong (Self.Source.View));
      Result : Path_Name.Set.Object;
   begin
      if Self.Has_Object_Code then
         --  Object themselves

         for O of Self.Object_Files loop
            Result.Append (O);
         end loop;

         --  The generated artefacts

         declare
            Name : constant Name_Type := Source.Path_Name.Simple_Name;
            Dir  : constant Optional_Name_Type :=
                     Optional_Name_Type (View.Object_Directory.Value);

            procedure Append_File (Name : Name_Type);
            --  Append full filename constructed from Name and Dir to result

            -----------------
            -- Append_File --
            -----------------

            procedure Append_File (Name : Name_Type) is
            begin
               Result.Append (Path_Name.Create_File (Name, Dir));
            end Append_File;

         begin
            Append_File (Name & ".stdout");
            Append_File (Name & ".stderr");
            Append_File (Source.Path_Name.Base_Name & ".adt");
         end;
      end if;

      --  Append the dependencies

      if Self.Has_Dependency then
         for D of Self.Dependency_Files loop
            Result.Append (D);

            --  Library project has the same ALI files in object and library
            --  directories.

            if View.Is_Library and then Source.Language = "Ada" then
               Result.Append
                 (Path_Name.Create_File
                    (D.Simple_Name,
                     Optional_Name_Type (View.Library_Directory.Value)));
            end if;

            --  Dependency files must be placed into the Library_Directory of
            --  the aggregate library.

            if View.Is_Aggregated then
               declare
                  Aggregate : constant Project.View.Object := View.Aggregate;
               begin
                  if Aggregate.Kind = K_Aggregate_Library then
                     Result.Append
                       (Path_Name.Create_File
                          (D.Simple_Name,
                           Optional_Name_Type
                             (Aggregate.Library_Directory.Value)));
                  end if;
               end;
            end if;
         end loop;
      end if;

      --  Append preprocessed sources

      if Self.Has_Preprocessed_Source then
         Result.Append (Self.Preprocessed_Source);
      end if;

      if Self.Switches.Is_Defined and then Self.Switches.Exists then
         Result.Append (Self.Switches);
      end if;

      return Result;
   end List;

   -----------------
   -- Object_Code --
   -----------------

   function Object_Code
     (Self : Artifact.Object; Index : Natural := 1) return Path_Name.Object is
   begin
      return Self.Object_Files (Index);
   end Object_Code;

   -------------------------
   -- Preprocessed_Source --
   -------------------------

   function Preprocessed_Source (Self : Object) return Path_Name.Object is
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
