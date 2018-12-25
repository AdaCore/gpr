------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2019, Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.MD5;
with GNAT.OS_Lib;

with GPR2.Message;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Source;
with GPR2.Source_Reference;

package body GPR2.Project.Definition is

   use Ada;
   use GNAT;

   Builtin_Naming_Package : Project.Pack.Object;
   --  The default naming package to use if no Naming package specified in the
   --  project and no configuration file loaded. We at least want to handle in
   --  this case the standard Ada and C namings.

   Builtin_Languages : Project.Attribute.Object;
   --  The default languages to use if no languages attribute specified in the
   --  project. The default value is just "ada".

   function Source_Directories (Def : Data) return Project.Attribute.Object is
     (if Def.Attrs.Has_Source_Dirs
      then Def.Attrs.Source_Dirs
      else Project.Attribute.Default_Source_Dirs);

   function Languages (Def : Data) return Containers.Value_List is
     (if Def.Attrs.Has_Languages
      then Def.Attrs.Languages.Values
      else Builtin_Languages.Values);

   --------------------
   -- Naming_Package --
   --------------------

   function Naming_Package (Def : Data) return Project.Pack.Object is
   begin
      if Def.Has_Packages (Registry.Pack.Naming) then
         declare
            Naming : constant Project.Pack.Object :=
                       Def.Packs (Registry.Pack.Naming);
            Result : Project.Attribute.Set.Object :=
                       Builtin_Naming_Package.Attributes;
         begin
            --  Result is built-in package attributes, now we want to replace
            --  the attribute as defined in the project.

            for A of Naming.Attributes loop
               Result.Include (A);
            end loop;

            return Project.Pack.Create
              (Name       => Registry.Pack.Naming,
               Attributes => Result,
               Variables  => Project.Variable.Set.Set.Empty_Map,
               Sloc       => Source_Reference.Object (Naming));
         end;

      elsif Def.Tree.Has_Configuration then
         return Def.Tree.Configuration.Corresponding_View.Naming_Package;

      else
         return Builtin_Naming_Package;
      end if;
   end Naming_Package;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources (Def : in out Data; View : Project.View.Object) is

      use type Ada.Containers.Count_Type;
      use type MD5.Binary_Message_Digest;
      use type Project.Attribute.Object;

      package Unit_Naming is
        new Ada.Containers.Indefinite_Ordered_Maps (Value_Type, Name_Type);

      package Interfaces_Unit is new Ada.Containers.Indefinite_Ordered_Maps
        (Name_Type, Project.Attribute.Object, "=" => Project.Attribute."=");

      type Insert_Mode is (Replace, Skip, Error);
      --  Controls behavior when a duplicated unit/filename is found
      --
      --  Replace : the new source replace the previous one
      --  Skip    : the new source is ignored
      --  Error   : an error is raised

      package Source_Set is
        new Ada.Containers.Indefinite_Ordered_Sets (Name_Type);

      procedure Handle_Directory (Dir : GPR2.Path_Name.Full_Name);
      --  Handle the specified directory, that is read all files in Dir and
      --  eventually call recursivelly Handle_Directory if a recursive read
      --  is specified.

      procedure Handle_File (Filename : GPR2.Path_Name.Full_Name);
      --  Handle Filename which can eventually be part of the current view
      --  depending on the language handled by the current view.

      function Language_For
        (Filename : GPR2.Path_Name.Full_Name;
         Kind     : out GPR2.Source.Kind_Type) return Value_Type;
      --  The language for Filename based on the Naming package. It also
      --  returns in Kind if Filename is a spec, a body or a separate.

      function Unit_For
        (Filename             : Simple_Name;
         Kind                 : GPR2.Source.Kind_Type;
         Ok                   : out Boolean;
         Has_Naming_Exception : out Boolean) return Name_Type
        with Pre => (for some C of Filename => C = '.');
      --  Given Filename (with extension, needed to compute the language),
      --  returns the unit name. This is meaningful for unit based language
      --  like Ada. For other languages the unit name is the same as the
      --  Filename.

      function Signature return MD5.Binary_Message_Digest;
      --  Compute the signature corresponding to the source context. If the
      --  signature is not the same recorded for the view, the source set
      --  need to be recomputed.

      procedure Read_File
        (Filename : GPR2.Path_Name.Full_Name;
         Set      : in out Source_Set.Set);
      --  Read Filename and insert each line in Set

      procedure Insert
        (Sources : Project.Source.Set.Object; Mode : Insert_Mode);
      --  Insert Sources into Data.Sources

      procedure Fill_Naming_Exceptions (Set : Project.Attribute.Set.Object)
        with Pre =>
          (for all A of Set =>
             A.Name = Registry.Attribute.Spec
             or else A.Name = Registry.Attribute.Specification
             or else A.Name = Registry.Attribute.Body_N
             or else A.Name = Registry.Attribute.Implementation);
      --  Fill the Naming_Exceptions object with the given attribute set values

      Naming : constant Project.Pack.Object := Naming_Package (Def);
      --  Package Naming for the view

      Dot_Repl : constant String :=
                   Naming.Attribute
                     (Registry.Attribute.Dot_Replacement).Value;
      --  Get Dot_Replacement value

      Is_Standard_GNAT_Naming : constant  Boolean :=
                                  (Naming.Spec_Suffix ("ada").Value = ".ads")
                                     and then
                                  (Naming.Body_Suffix ("ada").Value = ".adb")
                                     and then
                                  (Dot_Repl = "-");
      --  True if the current naming scheme is GNAT's default naming scheme.
      --  This is to take into account shortened names like "Ada." (a-),
      --  "System." (s-) and so on.

      Included_Sources  : Source_Set.Set;
      Excluded_Sources  : Source_Set.Set;
      Interfaces        : Interfaces_Unit.Map;
      Interfaces_Found  : Interfaces_Unit.Map;

      Tree              : constant not null access Project.Tree.Object :=
                            Def.Tree;

      Message_Count     : constant Containers.Count_Type :=
                            Tree.Log_Messages.Count;

      Naming_Exceptions : Unit_Naming.Map;

      ----------------------------
      -- Fill_Naming_Exceptions --
      ----------------------------

      procedure Fill_Naming_Exceptions (Set : Project.Attribute.Set.Object) is
      begin
         for A of Set loop
            Naming_Exceptions.Insert
              (Directories.Simple_Name (A.Value), Name_Type (A.Index));
         end loop;
      end Fill_Naming_Exceptions;

      ----------------------
      -- Handle_Directory --
      ----------------------

      procedure Handle_Directory (Dir : GPR2.Path_Name.Full_Name) is
         use all type Directories.File_Kind;

         Is_Recursive : constant Boolean :=
                          Dir'Length > 2
                          and then Dir (Dir'Last) = '*'
                          and then Dir (Dir'Last - 1) = '*';
         --  Recursivity is controlled by a double * at the end of the
         --  directory.

         Dir_Name     : constant GPR2.Path_Name.Full_Name :=
                          (if Is_Recursive
                           then Dir (Dir'First .. Dir'Last - 2)
                           else Dir);
         Dir_Search   : Directories.Search_Type;
         Dir_Entry    : Directories.Directory_Entry_Type;
      begin
         Directories.Start_Search (Dir_Search, Dir_Name, "*");

         while Directories.More_Entries (Dir_Search) loop
            Directories.Get_Next_Entry (Dir_Search, Dir_Entry);

            if Directories.Kind (Dir_Entry) = Ordinary_File then
               Handle_File (Directories.Full_Name (Dir_Entry));

            elsif Directories.Kind (Dir_Entry) = Directory
              and then Is_Recursive
            then
               Handle_Sub_Directory : declare
                  New_Dir : constant String :=
                              Directories.Simple_Name (Dir_Entry);
               begin
                  if New_Dir not in "." | ".." then
                     Handle_Directory (Directories.Full_Name (Dir_Entry));
                  end if;
               end Handle_Sub_Directory;
            end if;
         end loop;

         Directories.End_Search (Dir_Search);
      end Handle_Directory;

      -----------------
      -- Handle_File --
      -----------------

      procedure Handle_File (Filename : GPR2.Path_Name.Full_Name) is
         Kind     : GPR2.Source.Kind_Type;
         Language : constant Value_Type :=
                      Language_For (Filename, Kind);
      begin
         --  Check the language, if no language found this is not a source for
         --  this project.
         --
         --  The source is added if not in the list of excluded sources and if
         --  included sources is defined it must be in.

         if Language /= No_Value
           and then not
             Excluded_Sources.Contains
               (Name_Type (Directories.Simple_Name (Filename)))
           and then
             (Included_Sources.Is_Empty
              or else Included_Sources.Contains
                (Name_Type (Directories.Simple_Name (Filename))))
         then
            declare
               use all type GPR2.Source.Kind_Type;

               function Is_In_Interfaces (Name : Name_Type) return Boolean is
                 (Interfaces.Contains (Name)
                  or else Interfaces_Found.Contains (Name));

               procedure Register_Src;
               --  Register Src below into U_Def. Updating the necessary fields

               B_Name   : constant Simple_Name :=
                            Simple_Name (Directories.Simple_Name (Filename));
               Ok       : Boolean := True;
               N_Except : Boolean := False;
               Lang     : constant Name_Type := Name_Type (Language);
               Unit     : constant Optional_Name_Type :=
                            (if Lang = "ada"
                             then Unit_For (B_Name, Kind, Ok, N_Except)
                             else No_Name);
               File     : constant GPR2.Path_Name.Object :=
                            GPR2.Path_Name.Create_File (Name_Type (Filename));
               Src      : constant GPR2.Source.Object :=
                            GPR2.Source.Create
                              (Filename  => File,
                               Kind      => Kind,
                               Language  => Lang,
                               Unit_Name => Unit);

               Is_Interface : constant Boolean :=
                                Is_In_Interfaces (B_Name)
                                  or else
                                (Unit /= No_Name
                                 and then Is_In_Interfaces (Name_Type (Unit)));

               P_Src : constant GPR2.Project.Source.Object :=
                         Project.Source.Create
                           (Src, View, Is_Interface, N_Except);

               U_Def : GPR2.Unit.Object;

               ------------------
               -- Register_Src --
               ------------------

               procedure Register_Src is
               begin
                  if Kind = S_Spec then
                     U_Def.Update_Spec (P_Src);
                  else
                     U_Def.Update_Bodies (P_Src);
                  end if;
               end Register_Src;

            begin
               if Ok then
                  if Unit /= No_Name then
                     Def.Tree.Record_View
                       (View, Source => File.Value, Unit => Unit);

                     if Def.Units.Contains (Unit) then
                        U_Def := Def.Units (Unit);

                        Register_Src;

                        Def.Units.Replace (Unit, U_Def);

                     else
                        Register_Src;

                        Def.Units.Insert (Unit, U_Def);
                     end if;
                  end if;

                  Def.Sources.Insert (P_Src);

                  --  And make sure that if it is an interface it is removed
                  --  from the set.

                  if Is_Interface then
                     --  We still want to keep unit interface around as we need
                     --  them to be able to check the other part if any.

                     if Interfaces.Contains (Unit) then
                        Interfaces_Found.Include (Unit, Interfaces (Unit));
                     end if;

                     Interfaces.Exclude (Unit);
                     Interfaces.Exclude (B_Name);
                  end if;
               end if;
            end;
         end if;
      end Handle_File;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Sources : Project.Source.Set.Object; Mode : Insert_Mode) is
      begin
         for Source of Sources loop
            if Def.Sources.Contains (Source) then
               case Mode is
                  when Replace =>
                     Def.Sources.Replace (Source);

                  when Error =>
                     null;

                  when Skip =>
                     null;
               end case;

            else
               Def.Sources.Insert (Source);
            end if;
         end loop;
      end Insert;

      ------------------
      -- Language_For --
      ------------------

      function Language_For
        (Filename : GPR2.Path_Name.Full_Name;
         Kind     : out GPR2.Source.Kind_Type)
         return Value_Type
      is

         function Ends_With (Str, Ending : String) return Boolean with Inline;
         --  Returns True if Str ends with the string Ending

         ---------------
         -- Ends_With --
         ---------------

         function Ends_With (Str, Ending : String) return Boolean is
         begin
            if Str'Length >= Ending'Length then
               return Strings.Fixed.Tail (Str, Ending'Length) = Ending;
            else
               return False;
            end if;
         end Ends_With;

         Languages : constant Project.Attribute.Object :=
                       (if Def.Has_Attributes (Registry.Attribute.Languages)
                        then Def.Attrs.Element
                          (Registry.Attribute.Languages, No_Value)
                        else Builtin_Languages);

      begin
         --  For every languages defined for the view

         for Lang of Languages.Values loop
            Check_Spec : declare
               Spec_Suffix : constant Project.Attribute.Object :=
                               Naming.Spec_Suffix (Name_Type (Lang));
            begin
               if Spec_Suffix.Is_Defined
                 and then Ends_With (Filename, Spec_Suffix.Value)
               then
                  Kind := GPR2.Source.S_Spec;
                  return Lang;
               end if;
            end Check_Spec;

            --  If the body and separate suffixes are identical we cannot get
            --  here a proper kind. We should parse the source to actually get
            --  the proper kind. Note that this is not done here but at the
            --  point it is really needed, that is when the S.Kind accessor is
            --  called. So we defer the parsing when needed if at all needed.
            --  See GPR2.Source.Kind implementation.

            Check_Body : declare
               Body_Suffix : constant Project.Attribute.Object :=
                               Naming.Body_Suffix (Name_Type (Lang));
            begin
               if Body_Suffix.Is_Defined
                 and then Ends_With (Filename, Body_Suffix.Value)
               then
                  Kind := GPR2.Source.S_Body;
                  return Lang;
               end if;
            end Check_Body;

            Check_Separate : declare
               Sep_Suffix : constant Project.Attribute.Object :=
                              Naming.Separate_Suffix (Name_Type (Lang));
            begin
               if Sep_Suffix.Is_Defined
                 and then Ends_With (Filename, Sep_Suffix.Value)
               then
                  Kind := GPR2.Source.S_Separate;
                  return Lang;
               end if;
            end Check_Separate;
         end loop;

         return No_Value;
      end Language_For;

      ---------------
      -- Read_File --
      ---------------

      procedure Read_File
        (Filename : GPR2.Path_Name.Full_Name;
         Set      : in out Source_Set.Set)
      is
         F      : Text_IO.File_Type;
         Buffer : String (1 .. 1_024);
         Last   : Natural;
      begin
         Text_IO.Open (F, Text_IO.In_File, Filename);

         while not Text_IO.End_Of_File (F) loop
            Text_IO.Get_Line (F, Buffer, Last);
            Set.Include (Name_Type (Buffer (Buffer'First .. Last)));
         end loop;

         Text_IO.Close (F);
      end Read_File;

      ---------------
      -- Signature --
      ---------------

      function Signature return MD5.Binary_Message_Digest is
         C : MD5.Context;

         procedure Handle (Data : Definition.Data);
         --  Handle the given project's definition

         ------------
         -- Handle --
         ------------

         procedure Handle (Data : Definition.Data) is

            procedure Add (A : Project.Attribute.Object);
            --  Add attribute name and values into the MD5 context

            ---------
            -- Add --
            ---------

            procedure Add (A : Project.Attribute.Object) is
            begin
               MD5.Update (C, String (A.Name) & "/");
               for Value of A.Values loop
                  MD5.Update (C, Value);
               end loop;
            end Add;

         begin
            --  The signature to detect the source change is based on the
            --  attributes which are used to compute the actual source set.

            if Data.Attrs.Has_Languages then
               Add (Data.Attrs.Languages);
            end if;

            if Data.Attrs.Has_Source_Dirs then
               Add (Data.Attrs.Source_Dirs);
            end if;

            if Data.Attrs.Has_Source_Files then
               Add (Data.Attrs.Source_Files);
            end if;

            if Data.Attrs.Has_Excluded_Source_Files then
               Add (Data.Attrs.Excluded_Source_Files);
            end if;

            if Data.Attrs.Has_Excluded_Source_List_File then
               Add (Data.Attrs.Excluded_Source_List_File);
            end if;

            if Data.Attrs.Has_Source_List_File then
               Add (Data.Attrs.Source_List_File);
            end if;

            --  Handle also the naming definitions

            if Data.Packs.Contains (Project.Registry.Pack.Naming) then
               Handle_Naming : declare
                  use Registry.Attribute;

                  Naming : constant Project.Pack.Object :=
                             Data.Packs (Project.Registry.Pack.Naming);
               begin
                  if Naming.Has_Attributes (Dot_Replacement) then
                     Add (Naming.Attribute (Dot_Replacement));
                  end if;

                  for Attr of Naming.Attributes (Spec_Suffix) loop
                     Add (Attr);
                  end loop;

                  for Attr of Naming.Attributes (Body_Suffix) loop
                     Add (Attr);
                  end loop;

                  for Attr of Naming.Attributes (Separate_Suffix) loop
                     Add (Attr);
                  end loop;

                  for Attr of Naming.Attributes (Spec) loop
                     Add (Attr);
                  end loop;

                  for Attr of Naming.Attributes (Body_N) loop
                     Add (Attr);
                  end loop;

                  for Attr of Naming.Attributes (Specification) loop
                     Add (Attr);
                  end loop;

                  for Attr of Naming.Attributes (Implementation) loop
                     Add (Attr);
                  end loop;
               end Handle_Naming;
            end if;
         end Handle;

      begin
         Handle (Def);

         --  If an aggregate library project take into account the
         --  aggregated projects.

         if Def.Kind = K_Aggregate_Library then
            for A of Def.Aggregated loop
               Handle (Definition.Get_RO (A).all);
            end loop;
         end if;

         return MD5.Digest (C);
      end Signature;

      --------------
      -- Unit_For --
      --------------

      function Unit_For
        (Filename             : Simple_Name;
         Kind                 : GPR2.Source.Kind_Type;
         Ok                   : out Boolean;
         Has_Naming_Exception : out Boolean) return Name_Type
      is
         use Ada.Strings;
         use Ada.Strings.Unbounded;

         function Compute_Unit_From_Filename return Unbounded_String;
         --  This routine compute the unit from the filename

         --------------------------------
         -- Compute_Unit_From_Filename --
         --------------------------------

         function Compute_Unit_From_Filename return Unbounded_String is
            Result : Unbounded_String :=
                       To_Unbounded_String (String (Filename));
         begin
            --  First remove the suffix for the given language

            declare
               Suffix : constant Value_Type :=
                          (case Kind is
                              when GPR2.Source.S_Spec     =>
                                 Naming.Spec_Suffix ("ada").Value,
                              when GPR2.Source.S_Body     =>
                                 Naming.Body_Suffix ("ada").Value,
                              when GPR2.Source.S_Separate =>
                                 Naming.Separate_Suffix ("ada").Value);
            begin
               if Length (Result) > Suffix'Length then
                  Delete
                    (Result,
                     From    => Length (Result) - Suffix'Length + 1,
                     Through => Length (Result));
               end if;
            end;

            --  If Dot_Replacement is not a single dot, then there should not
            --  be any dot in the name.

            if Dot_Repl /= "." then
               if Index (Result, ".") /= 0 then
                  Ok := False;
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error, "invalid name, contains dot"));

               else
                  declare
                     I : Natural := 1;
                  begin
                     loop
                        I := Index (Result, Dot_Repl, From => I);
                        exit when I = 0;

                        Replace_Slice
                          (Result, I, I + Dot_Repl'Length - 1, ".");
                     end loop;
                  end;
               end if;
            end if;

            --  Casing for the unit is all lowercase

            Translate (Result, Maps.Constants.Lower_Case_Map);

            --  In the standard GNAT naming scheme, check for special cases:
            --  children or separates of A, G, I or S, and run time sources.

            if Is_Standard_GNAT_Naming and then Length (Result) >= 3 then
               declare
                  S1 : constant Character := Element (Result, 1);
                  S2 : constant Character := Element (Result, 2);
                  S3 : constant Character := Element (Result, 3);

               begin
                  if S1 in 'a' | 'g' | 'i' | 's' then
                     --  Children or separates of packages A, G, I or S. These
                     --  names are x__ ... or x~... (where x is a, g, i, or s).
                     --  Both versions (x__... and x~...) are allowed in all
                     --  platforms, because it is not possible to know the
                     --  platform before processing of the project files.

                     if S2 = '_' and then S3 = '_' then
                        --  Replace first _ by a dot
                        Replace_Element (Result, 2, '.');

                        --  and remove the second _
                        Delete (Result, 3, 3);

                     elsif S2 = '~' then
                        Replace_Element (Result, 2, '.');

                     elsif S2 = '.' then

                        --  If it is potentially a run time source

                        null;
                     end if;
                  end if;
               end;
            end if;

            return Result;
         end Compute_Unit_From_Filename;

         Result : Unbounded_String := To_Unbounded_String (String (Filename));

      begin
         --  Let's pretend the filename/unit is part of the sources

         Ok := True;
         Has_Naming_Exception := False;

         --  First check for a naming exception for this filename

         if Naming_Exceptions.Contains (To_String (Result)) then
            --  In this case we have the unit specified explicitly

            Has_Naming_Exception := True;

            Result := To_Unbounded_String
              (String (Naming_Exceptions.Element (To_String (Result))));

            Translate (Result, Maps.Constants.Lower_Case_Map);

         else
            --  Let's compute the unit based on the filename

            Result := Compute_Unit_From_Filename;

            if Strings.Fixed.Index (To_String (Result), "__") /= 0 then
               --  The filename has two underlines, skip it as it cannot be
               --  part of the sources except if there is a naming exception
               --  (checked above).

               Ok := False;
               return Name_Type (To_String (Result));

            else
               --  Then check that we do not have an exception for this unit

               declare
                  Unit : constant Value_Type :=
                           Value_Type (To_String (Result));
                  Attr : constant Project.Attribute.Object :=
                           (case Kind is
                               when GPR2.Source.S_Spec     =>
                                  Naming.Specification (Unit),
                               when GPR2.Source.S_Body     =>
                                  Naming.Implementation (Unit),
                               when GPR2.Source.S_Separate =>
                                  Project.Attribute.Undefined);
               begin
                  if Attr.Is_Defined
                    and then Attr.Value /= String (Filename)
                  then
                     --  We have a naming exception for this unit and the body
                     --  does not corresponds to the current filename. We skip
                     --  this unit.
                     Ok := False;
                     return Name_Type (Unit);
                  end if;
               end;
            end if;
         end if;

         --  Result contains the name of the unit in lower-cases. Check
         --  that this is a valid unit name.

         --  Must start with a letter

         if not Characters.Handling.Is_Letter (Element (Result, 1)) then
            Ok := False;
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  "unit '" & To_String (Result)
                  & "' not valid, should start with a letter",
                  Def.Attrs.Source_Dirs));
         end if;

         --  Cannot have 2 consecutive underscores, cannot have a dot after an
         --  underscore and should contains only alphanumeric characters.

         for K in 2 .. Length (Result) loop
            declare
               Prev    : constant Character := Element (Result, K - 1);
               Current : constant Character := Element (Result, K);
            begin
               if Current = '_' then
                  if Prev = '.' then
                     Ok := False;
                     Tree.Append_Message
                       (Message.Create
                          (Message.Error,
                           "unit '" & To_String (Result)
                           & "' not valid, cannot contains"
                           & " dot after underscore",
                           Def.Attrs.Source_Dirs));

                  elsif Prev = '_' then
                     Ok := False;
                     Tree.Append_Message
                       (Message.Create
                          (Message.Error,
                           "unit '" & To_String (Result)
                           & "' not valid, two consecutive"
                           & " underlines not permitted",
                           Def.Attrs.Source_Dirs));
                  end if;

               elsif not Characters.Handling.Is_Alphanumeric (Current)
                 and then Current /= '.'
               then
                  Ok := False;
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "unit '" & To_String (Result)
                        & "' not valid, should have only alpha numeric"
                        & " characters",
                        Def.Attrs.Source_Dirs));
               end if;
            end;
         end loop;

         return Name_Type (To_String (Result));
      end Unit_For;

      Current_Signature : constant MD5.Binary_Message_Digest :=
                            Signature;

      Root              : constant GPR2.Path_Name.Full_Name :=
                            Directories.Containing_Directory
                              (Def.Trees.Project.Path_Name.Value);
   begin
      --  Check if up-to-date using signature for source_dirs, source_files...
      --  An abstract or aggregate project has no sources.

      if Def.Sources_Signature /= Current_Signature
        and then Def.Kind not in K_Abstract | K_Aggregate
      then
         --  Setup the naming exceptions look-up table if needed

         Fill_Naming_Exceptions
           (Naming.Attributes (Registry.Attribute.Spec));
         Fill_Naming_Exceptions
           (Naming.Attributes (Registry.Attribute.Specification));
         Fill_Naming_Exceptions
           (Naming.Attributes (Registry.Attribute.Body_N));
         Fill_Naming_Exceptions
           (Naming.Attributes (Registry.Attribute.Implementation));

         --  Record units being set as interfaces, first for Library_Interface
         --  which containes unit names.

         if Def.Attrs.Has_Library_Interface then
            for Unit of Def.Attrs.Library_Interface.Values loop
               if Interfaces.Contains (Name_Type (Unit)) then
                  Tree.Append_Message
                    (Message.Create
                       (Message.Warning,
                        "duplicate unit '" & Unit
                        & "' in library_interface attribute",
                        Def.Attrs.Library_Interface));
               else
                  Interfaces.Insert
                    (Name_Type (Unit), Def.Attrs.Library_Interface);
               end if;
            end loop;
         end if;

         --  And then Interfaces which contains filenames

         if Def.Attrs.Has_Interfaces then
            for Source of Def.Attrs.Interfaces.Values loop
               if Interfaces.Contains (Simple_Name (Source)) then
                  Tree.Append_Message
                    (Message.Create
                       (Message.Warning,
                        "duplicate unit '" & Source
                        & "' in interfaces attribute",
                        Def.Attrs.Interfaces));
               else
                  Interfaces.Insert
                    (Simple_Name (Source), Def.Attrs.Interfaces);
               end if;
            end loop;
         end if;

         --  Read sources and set-up the corresponding definition

         --  First reset the current set

         Def.Sources.Clear;

         --  Clear the units record, note that we also want to record the
         --  unit_name -> view lookup table in the tree.

         for U of Def.Units loop
            Def.Tree.Clear_View (Unit => U);
         end loop;

         Def.Units.Clear;

         --  If we have attribute Excluded_Source_List_File

         if Def.Attrs.Has_Excluded_Source_List_File then
            declare
               File : constant GPR2.Path_Name.Full_Name :=
                        Directories.Compose
                          (Root,
                           Def.Attrs.Element
                             (Registry.Attribute.Excluded_Source_List_File)
                           .Value);
            begin
               Read_File (File, Excluded_Sources);
            end;
         end if;

         --  If we have attribute Excluded_Source_Files

         if Def.Attrs.Has_Excluded_Source_Files then
            for File of Def.Attrs.Excluded_Source_Files.Values loop
               Excluded_Sources.Include (Optional_Name_Type (File));
            end loop;
         end if;

         --  If we have attribute Source_List_File

         if Def.Attrs.Has_Source_List_File then
            declare
               File : constant GPR2.Path_Name.Full_Name :=
                        Directories.Compose
                          (Root,
                           Def.Attrs.Element
                             (Registry.Attribute.Source_List_File).Value);
            begin
               Read_File (File, Included_Sources);
            end;
         end if;

         --  If we have attribute Source_Files

         if Def.Attrs.Has_Source_Files then
            for File of Def.Attrs.Source_Files.Values loop
               Included_Sources.Include (Optional_Name_Type (File));
            end loop;
         end if;

         if Def.Kind = K_Aggregate_Library then
            --  Sources for an aggregate library is the cumulative set of
            --  sources of the aggregated projects.

            for Agg of Def.Aggregated loop
               declare
                  Sources : Project.Source.Set.Object;
               begin
                  for S of Agg.Sources loop
                     --  Update the View to aggregate library as dependency
                     --  files must be placed into the Library_Directory of the
                     --  aggregate library.

                     Sources.Insert
                       (Source.Create
                          (Source               => S.Source,
                           View                 => View,
                           Is_Interface         => S.Is_Interface,
                           Has_Naming_Exception => S.Has_Naming_Exception));
                  end loop;

                  Insert (Sources, Error);
               end;
            end loop;

         else
            Populate_Sources : begin
               --  Handle Source_Dirs

               for Dir of Def.Source_Directories.Values loop
                  if OS_Lib.Is_Absolute_Path (Dir) then
                     Handle_Directory (Dir);
                  else
                     Handle_Directory
                       (Root & OS_Lib.Directory_Separator & Dir);
                  end if;
               end loop;
            end Populate_Sources;
         end if;

         --  Finally get the sources from the extended's project if defined. We
         --  only add the sources not already defined in the current set.

         if Def.Extended.Is_Defined then
            Insert (Def.Extended.Sources, Skip);
         end if;

         --  Check that all interfaces have been found in the project view

         if not Interfaces.Is_Empty then
            for Unit in Interfaces.Iterate loop
               declare
                  Attr : constant Project.Attribute.Object :=
                           Interfaces_Unit.Element (Unit);
               begin
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "source for interface "
                        & (if Attr.Name = "library_interface"
                          then "unit" else "")
                        & " '"
                        & String (Interfaces_Unit.Key (Unit))
                        & "' not found",
                        Attr));
               end;
            end loop;
         end if;

         --  Record back new definition for the view with updated sources

         Def.Sources_Signature := Current_Signature;

         --  Then returns the sources

         if Message_Count < Tree.Log_Messages.Count
           and then Tree.Log_Messages.Has_Error
         then
            --  Some messages have been logged, raise an exception
            raise Project_Error with "cannot retrieve the sources";
         end if;
      end if;
   end Update_Sources;

begin
   --  Setup the default/build-in naming package

   declare
      Undef_Sloc : Source_Reference.Object renames Source_Reference.Undefined;

      function Create
        (Name : Name_Type; Index, Value : Value_Type)
         return Project.Attribute.Object;
      --  Create attribute Name for the Naming package

      ------------
      -- Create --
      ------------

      function Create
        (Name : Name_Type; Index, Value : Value_Type)
         return Project.Attribute.Object
      is
         A   : Project.Attribute.Object :=
                 Project.Attribute.Create (Name, Index, Value, Undef_Sloc);
         Def : constant Project.Registry.Attribute.Def :=
                 Project.Registry.Attribute.Get
                   (Project.Registry.Attribute.Create
                      (Name, Registry.Pack.Naming));
      begin
         A.Set_Case (Def.Index_Case_Sensitive, Def.Value_Case_Sensitive);
         return A;
      end Create;

      Ada_Spec : constant Project.Attribute.Object :=
                   Create (Registry.Attribute.Spec_Suffix, "ada", ".ads");
      Ada_Body : constant  Project.Attribute.Object :=
                   Create (Registry.Attribute.Body_Suffix, "ada", ".adb");
      C_Spec   : constant Project.Attribute.Object :=
                   Create (Registry.Attribute.Spec_Suffix, "c", ".h");
      C_Body   : constant Project.Attribute.Object :=
                   Create (Registry.Attribute.Body_Suffix, "c", ".c");
      Dot_Repl : constant Project.Attribute.Object :=
                   Project.Attribute.Create
                     (Registry.Attribute.Dot_Replacement,
                      "", "-", Undef_Sloc);
      Attrs    : Project.Attribute.Set.Object;
      Langs    : Containers.Value_List;
   begin
      --  Default naming package

      Attrs.Insert (Ada_Spec);
      Attrs.Insert (Ada_Body);
      Attrs.Insert (C_Spec);
      Attrs.Insert (C_Body);
      Attrs.Insert (Dot_Repl);
      Builtin_Naming_Package :=
        Project.Pack.Create
          (Registry.Pack.Naming,
           Attrs, Project.Variable.Set.Set.Empty_Map,
           Undef_Sloc);

      --  Default languages attribute

      Langs.Append ("ada");
      Builtin_Languages :=
        Project.Attribute.Create
          (Registry.Attribute.Languages, Langs, Undef_Sloc);
   end;
end GPR2.Project.Definition;
