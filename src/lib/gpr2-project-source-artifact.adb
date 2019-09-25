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

with GPR2.Project.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.Definition;

package body GPR2.Project.Source.Artifact is

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

   ------------
   -- Create --
   ------------

   function Create
     (Source : Project.Source.Object) return Artifact.Object
   is
      Src    : constant Name_Type := Source.Source.Path_Name.Base_Name;
      Lang   : constant Name_Type := Source.Source.Language;
      S_View : constant Project.View.Object :=
                 Definition.Strong (Source.View);
      O_View : constant Project.View.Object :=
                 (if Source.Has_Extending_View
                  then Source.Extending_View
                  else Definition.Strong (Source.View));

      O_Suffix : constant Name_Type := S_View.Tree.Object_Suffix (Lang);
      D_Suffix : constant Name_Type := S_View.Tree.Dependency_Suffix (Lang);
      P_Suffix : constant Name_Type := ".prep";
      S_Suffix : constant Name_Type := ".cswi";

      Object_Files     : Index_Path_Name_Map.Map;
      Dependency_Files : Index_Path_Name_Map.Map;

      Preprocessed : constant Path_Name.Object :=
                       Path_Name.Create_File
                         (Source.Source.Path_Name.Simple_Name & P_Suffix,
                          Optional_Name_Type (O_View.Object_Directory.Value));

      Switches     : constant Path_Name.Object :=
                       Path_Name.Create_File
                         (Src & S_Suffix,
                          Optional_Name_Type (O_View.Object_Directory.Value));

      Idx          : Positive := 1;

   begin
      if not Source.Source.Has_Units or else Source.Source.Has_Single_Unit then
         --  For aggregated library the .ali is also copied into the
         --  aggregate library directory.

         if Source.Aggregated then
            Dependency_Files.Insert
              (1,
               Path_Name.Create_File
                 (Src & D_Suffix,
                  Optional_Name_Type
                    (Source.Aggregating_View.Library_Ali_Directory.Value)));

         else
            Object_Files.Insert
              (1,
               Path_Name.Create_File
                 (Src & O_Suffix,
                  Optional_Name_Type (O_View.Object_Directory.Value)));

            if S_View.Is_Library and then Lang = "Ada" then
               Dependency_Files.Insert
                 (Idx,
                  Path_Name.Create_File
                    (Src & D_Suffix,
                     Optional_Name_Type (O_View.Library_Ali_Directory.Value)));
               Idx := Idx + 1;
            end if;

            Dependency_Files.Insert
              (Idx,
               Path_Name.Create_File
                 (Src & D_Suffix,
                  Optional_Name_Type (O_View.Object_Directory.Value)));
         end if;

      else
         for CU of Source.Source.Compilation_Units loop
            if CU.Kind = S_Body then
               declare
                  Index_Suffix : constant Name_Type := At_Suffix (CU.Index);
               begin
                  if Source.Aggregated then
                     Dependency_Files.Insert
                       (1,
                        Path_Name.Create_File
                          (Src & Index_Suffix & D_Suffix,
                           Optional_Name_Type
                             (Source.Aggregating_View.
                                  Library_Ali_Directory.Value)));

                  else
                     Object_Files.Insert
                       (CU.Index,
                        Path_Name.Create_File
                          (Src & Index_Suffix & O_Suffix,
                           Optional_Name_Type
                             (O_View.Object_Directory.Value)));

                     Idx := CU.Index;

                     if S_View.Is_Library and then Lang = "Ada" then
                        Dependency_Files.Insert
                          (Idx,
                           Path_Name.Create_File
                             (Src & Index_Suffix & D_Suffix,
                              Optional_Name_Type
                                (O_View.Library_Ali_Directory.Value)));
                        Idx := Idx + 1;
                     end if;

                     Dependency_Files.Insert
                       (Idx,
                        Path_Name.Create_File
                          (Src & Index_Suffix & D_Suffix,
                           Optional_Name_Type
                             (O_View.Object_Directory.Value)));
                  end if;
               end;
            end if;
         end loop;
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
      Lang   : constant Name_Type := Self.Source.Source.Language;
      Source : constant GPR2.Source.Object := Self.Source.Source;
      C_View : constant Project.View.Object :=
                 Definition.Strong (Self.Source.View);
      O_View : constant Project.View.Object :=
                 (if Self.Source.Has_Extending_View
                  then Self.Source.Extending_View
                  else C_View);
      Result : Path_Name.Set.Object;
      Name   : constant Name_Type := Source.Path_Name.Simple_Name;
      O_Dir  : constant Optional_Name_Type :=
                 Optional_Name_Type (O_View.Object_Directory.Value);

      procedure Append_File (Name : Name_Type);
      --  Append full filename constructed from Name and Object_Dir to result

      -----------------
      -- Append_File --
      -----------------

      procedure Append_File (Name : Name_Type) is
      begin
         Result.Append (Path_Name.Create_File (Name, O_Dir));
      end Append_File;

   begin
      for E of C_View.Source_Artifact_Extensions (Lang) loop
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
         for E of C_View.Object_Artifact_Extensions (Lang) loop
            Append_File (Source.Path_Name.Base_Name & Name_Type (E));
         end loop;
      end if;

      --  Append the dependencies

      for D of Self.Dependency_Files loop
         Result.Append (D);
      end loop;

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
