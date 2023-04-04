--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GPR2.Containers;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.View;
with GPR2.Source_Reference.Value;

separate (GPR2.Build.View_Tables)
package body Update_Sources_List is

   use type Project.Attribute.Object;

   use GPR2.Containers;
   use GPR2.Path_Name;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PRP renames GPR2.Project.Registry.Pack;
   package SR  renames GPR2.Source_Reference;

   package Attribute_List is new
     Ada.Containers.Doubly_Linked_Lists (Project.Attribute.Object);
   --  Element type for Source_Path_To_Attribute_List below

   package Source_Path_To_Attribute_List is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Filename_Type,
        Element_Type    => Attribute_List.List,
        Hash            => GPR2.Hash,
        Equivalent_Keys => GPR2."=",
        "="             => Attribute_List."=");
   --  Used for the Ada_Naming_Exceptions container which maps a filename to
   --  the list of naming attributes (Body/Spec) that reference it.

   type Naming_Schema
     (Spec_Suffix_Length,
      Body_Suffix_Length,
      Separate_Suffix_Length : Natural)
   is record
      Has_Spec_Suffix     : Boolean;
      Has_Body_Suffix     : Boolean;
      Has_Separate_Suffix : Boolean;
      Spec_Suffix         : String (1 .. Spec_Suffix_Length);
      Body_Suffix         : String (1 .. Body_Suffix_Length);
      Sep_Suffix          : String (1 .. Separate_Suffix_Length);
   end record;

   package Naming_Schema_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Language_Id, Naming_Schema, GPR2.Hash, GPR2."=");

   package Source_Set renames Containers.Filename_Type_Set;

   package Lang_Boolean_Map is new Ada.Containers.Hashed_Maps
     (Language_Id, Boolean, Hash, "=");

   procedure Include_Simple_Filename
     (Data  : View_Data;
      Set   : in out Source_Set.Set;
      Value : Value_Type;
      Sloc  : SR.Value.Object);

   function Naming_Exception_Equal
     (A : Project.Attribute.Object;
      B : Value_Type;
      I : Unit_Index) return Boolean
   is (A.Value.Text = B and then A.Value.At_Pos = I);
   --  Check if two naming exception attributes are equal

   procedure Fill_Ada_Naming_Exceptions
     (View         : GPR2.Project.View.Object;
      Attr         : Attribute_Id;
      Src_Map      : in out Source_Path_To_Attribute_List.Map)
     with Pre => Attr in  PRA.Naming.Spec.Attr | PRA.Naming.Body_N.Attr;
   --  Populate the src->unit and unit->src maps for Ada sources

   procedure Fill_Naming_Schema
     (View : Project.View.Object;
      Map  : in out Naming_Schema_Maps.Map);
   --  Populate Map with the naming schema information from view, for every
   --  language in use for the view.

   procedure Read_Source_List
     (View      : Project.View.Object;
      Filename  : Source_Reference.Value.Object;
      Set       : in out Source_Set.Set;
      Messages  : in out GPR2.Log.Object);
   --  Read from file defined in project attribute Attr_Name and insert each
   --  line into Set

   function Compute_Unit_From_Filename
     (File     : Path_Name.Object;
      Kind     : Unit_Kind;
      NS       : Naming_Schema;
      Dot_Repl : String;
      Messages : in out GPR2.Log.Object;
      Last_Dot : out Natural;
      Success  : out Boolean) return Name_Type;
   --  For an Ada source and given its kind, try to compute a valid unit
   --  name. Success takes True if such a valid name is found.
   --  Set Last_Dot to last dot index in result to split separate unit
   --  name.

   --------------------------------
   -- Compute_Unit_From_Filename --
   --------------------------------

   function Compute_Unit_From_Filename
     (File     : Path_Name.Object;
      Kind     : Unit_Kind;
      NS       : Naming_Schema;
      Dot_Repl : String;
      Messages : in out GPR2.Log.Object;
      Last_Dot : out Natural;
      Success  : out Boolean) return Name_Type
   is
      use Ada.Strings;
      use Ada.Strings.Maps;

      Result     : Unbounded_String :=
                     To_Unbounded_String (String (File.Simple_Name));
      Default_NS : constant  Boolean :=
                     NS.Spec_Suffix = ".ads" and then NS.Body_Suffix = ".adb"
                      and then NS.Sep_Suffix = ".adb" and then Dot_Repl = "-";
      --  True if the current naming scheme is GNAT's default naming scheme.
      --  This is to take into account shortened names like "Ada." (a-),
      --  "System." (s-) and so on.
   begin
      --  First remove the suffix for the given language

      declare
         Suffix : constant Value_Type :=
                    (case Kind is
                        when S_Spec     => NS.Spec_Suffix,
                        when S_Body     => NS.Body_Suffix,
                        when S_Separate => NS.Sep_Suffix);
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
            Messages.Append
              (Message.Create
                 (Message.Warning, "invalid file name, contains dot",
                  SR.Create (File.Value, 1, 1)));
            Last_Dot := 0;
            goto Invalid;

         else
            declare
               I : Natural := 1;
            begin
               Last_Dot := 0;

               loop
                  I := Index (Result, Dot_Repl, From => I);
                  exit when I = 0;

                  Replace_Slice
                    (Result, I, I + Dot_Repl'Length - 1, ".");

                  Last_Dot := I;
               end loop;
            end;
         end if;

      else
         Last_Dot := Index (Result, Dot_Repl, Going => Backward);
      end if;

      --  In the standard GNAT naming scheme, check for special cases:
      --  children or separates of A, G, I or S, and run time sources.

      if Default_NS and then Length (Result) >= 3 then
         declare
            S1 : constant Character := Element (Result, 1);
            S2 : constant Character := Element (Result, 2);

         begin
            if S1 in 'a' | 'g' | 'i' | 's' then
               --  Children or separates of packages A, G, I or S. These
               --  names are x~... (where x is a, g, i, or s).

               if S2 = '~' then
                  Replace_Element (Result, 2, '.');
               end if;

               --  We do nothing with S2 = '.' case here because it can
               --  be regular package with one letter named parent. We
               --  will detect runtime source later at unit name
               --  clarification stage in Ada or ALI parser.
            end if;
         end;
      end if;

      Success := True;

      return Name_Type (To_String (Result));

      <<Invalid>>

      Success := False;

      return "0"; -- Some dummy unit name
   end Compute_Unit_From_Filename;

   --------------------------------
   -- Fill_Ada_Naming_Exceptions --
   --------------------------------

   procedure Fill_Ada_Naming_Exceptions
     (View         : GPR2.Project.View.Object;
      Attr         : Attribute_Id;
      Src_Map      : in out Source_Path_To_Attribute_List.Map)
   is
   begin
      for A of View.Attributes
        (Name          => (PRP.Naming, Attr),
         With_Defaults => False,
         With_Config   => False)
      loop
         declare
            Source          : constant Filename_Type :=
                                Filename_Type (A.Value.Text);
            Insert_Position : Source_Path_To_Attribute_List.Cursor;
            Is_Inserted     : Boolean;
         begin
            Src_Map.Insert
              (Key      => Source,
               New_Item => Attribute_List.Empty_List,
               Position => Insert_Position,
               Inserted => Is_Inserted);
            Src_Map (Insert_Position).Append (A);
         end;
      end loop;
   end Fill_Ada_Naming_Exceptions;

   ------------------------
   -- Fill_Naming_Schema --
   ------------------------

   procedure Fill_Naming_Schema
     (View : Project.View.Object;
      Map  : in out Naming_Schema_Maps.Map)
   is
   begin
      for L of View.Languages loop
         declare
            Lang            : constant Language_Id := +Name_Type (L.Text);
            Has_Spec_Suffix : constant Boolean :=
                                View.Has_Spec_Suffix (Lang);
            Spec_Suffix     : constant String :=
                                (if Has_Spec_Suffix
                                 then View.Spec_Suffix (Lang).Value.Text
                                 else "");
            Has_Body_Suffix : constant Boolean :=
                                View.Has_Body_Suffix (Lang);
            Body_Suffix     : constant String :=
                                (if Has_Body_Suffix
                                 then View.Body_Suffix (Lang).Value.Text
                                 else "");
            Has_Sep_Suffix  : constant Boolean :=
                                Lang = Ada_Language
                                    and then View.Has_Separate_Suffix;
            Sep_Suffix      : constant String :=
                                (if Has_Sep_Suffix
                                 then View.Separate_Suffix.Value.Text
                                 else "");
         begin
            Map.Insert
              (Lang,
               (Spec_Suffix_Length     => Spec_Suffix'Length,
                Body_Suffix_Length     => Body_Suffix'Length,
                Separate_Suffix_Length => Sep_Suffix'Length,
                Has_Spec_Suffix        => Has_Spec_Suffix,
                Has_Body_Suffix        => Has_Body_Suffix,
                Has_Separate_Suffix    => Has_Sep_Suffix,
                Spec_Suffix            => Spec_Suffix,
                Body_Suffix            => Body_Suffix,
                Sep_Suffix             => Sep_Suffix));
         end;
      end loop;
   end Fill_Naming_Schema;

   -----------------------------
   -- Include_Simple_Filename --
   -----------------------------

   procedure Include_Simple_Filename
     (Data  : View_Data;
      Set   : in out Source_Set.Set;
      Value : Value_Type;
      Sloc  : SR.Value.Object)
   is
      Position : Source_Set.Cursor;
      Inserted : Boolean;
   begin
      if Has_Directory_Separator (Value) then
         Data.View.Tree.Append_Message
           (Message.Create
              (Message.Error,
               "file name cannot include directory information (""" & Value
               & """)",
               Sloc));
      else
         Set.Insert (Filename_Type (Value), Position, Inserted);
      end if;
   end Include_Simple_Filename;

   -------------
   -- Process --
   -------------

   procedure Process
     (Data          : in out View_Data;
      Stop_On_Error : Boolean;
      Messages      : in out GPR2.Log.Object)
   is
      function Is_Compilable (Language : Language_Id) return Boolean;
      --  Check whether the language is compilable on the current View. This
      --  includes information provided by the Tree (Driver attribute). Note
      --  that this routine caches the result into a map.

      procedure Handle_File
        (Dir_Ref   : SR.Value.Object;
         File      : GPR2.Path_Name.Object;
         Timestamp : Ada.Calendar.Time);
      --  Callback to Source_Directories_Walk: update the list of files found
      --  in the source directories.

      function Process_File (File : File_Info) return Boolean;

      Previous_Files     : File_Sets.Set := Data.Src_Files;
      --  List of files that were present in the source dirs during last call
      --  to Process. Used for delta updates

      Changed_Sources    : Basename_Sets.Set;

      Current_Src_Dir_SR : GPR2.Source_Reference.Value.Object;
      --  Identifies the Source_Dirs value being processed

      Source_Name_Set    : GPR2.Containers.Filename_Set;
      --  Collection of source simple names for a given Source_Dirs value

      Dot_Repl : constant String :=
                   Data.View.Attribute (PRA.Naming.Dot_Replacement).Value.Text;
      --  Get Dot_Replacement value

      Naming_Schema_Map       : Naming_Schema_Maps.Map;

      Listed_Sources          : Source_Set.Set;
      Excluded_Sources        : Source_Set.Set;
      --  Has either Source_Files or Source_List_File attributes

      Has_Src_In_Lang         : Language_Set;
      --  Insert record there if the language has a source

      Tree                    : constant not null access Project.Tree.Object :=
                                  Data.View.Tree;

      Ada_Naming_Exceptions   : Source_Path_To_Attribute_List.Map;
      Attr                    : Project.Attribute.Object;

      Compilable_Language     : Lang_Boolean_Map.Map;
      --  List of compilable languages for the view

      -----------------
      -- Handle_File --
      -----------------

      procedure Handle_File
        (Dir_Ref   : SR.Value.Object;
         File      : GPR2.Path_Name.Object;
         Timestamp : Ada.Calendar.Time)
      is
      begin
         Data.Src_Files.Include ((File, Timestamp, Dir_Ref));
      end Handle_File;

      -------------------
      -- Is_Compilable --
      -------------------

      function Is_Compilable (Language : Language_Id) return Boolean
      is
         C    : constant Lang_Boolean_Map.Cursor :=
                  Compilable_Language.Find (Language);
         Attr : GPR2.Project.Attribute.Object;
         Res  : Boolean;
      begin
         if not Lang_Boolean_Map.Has_Element (C) then
            Attr := Data.View.Attribute
              (PRA.Compiler.Driver, Project.Attribute_Index.Create (Language));
            Res := Attr.Is_Defined
              and then Length (Attr.Value.Unchecked_Text) > 0;

            Compilable_Language.Insert (Language, Res);

            return Res;

         else
            return Lang_Boolean_Map.Element (C);
         end if;
      end Is_Compilable;

      ------------------
      -- Process_File --
      ------------------

      function Process_File (File : File_Info) return Boolean is

         use all type GPR2.Build.Source.Naming_Exception_Kind;

         --  The implementation works as follows:
         --    For every language L in the project:
         --      1- Check if F matches with a naming exception (see
         --         Check_Naming_Exceptions):
         --           - if L is Ada, look F up the Ada_Naming_Exceptions map
         --           - else, check the attributes Implementation_Exceptions
         --             and Specification_Exceptions for the language L.
         --             This was missing in the previous implementation.
         --           - This also computes Kind for non-Ada sources.
         --             For Ada sources we will compute a Kind for every
         --             compilation unit later on.
         --         If a match is found and L is Ada, then compute the
         --         compilation units.
         --      2- If no naming exceptions matched, check the naming scheme
         --         for L (see Check_Naming_Scheme):
         --           - Separate_Suffix is only checked for Ada.
         --           - This time, Kind is computed no matter the language, as
         --             for Ada we will assume the source is single-unit.
         --         If a match is found and L is Ada then compute the single
         --         compilation unit for the source. The unit name is derived
         --         from the file name (see Compute_Unit_From_Filename):
         --           - Validity checks on the unit name are only done here.
         --             Should this be reverted to the previous behavior, i.e.
         --             some checks apply to both the naming exception and
         --             naming scheme cases???
         --      3- If either one or the other method resulted in a match,
         --         - update the source/unit interface containers as done in
         --           the previous implementation (except that the unit and
         --           source interface cases are now handled separately).
         --         - Create the GPR2.Source object. We now have different
         --           constructors for Ada and for other languages. This change
         --           is just to make things more explicit. In addition to the
         --           Units argument, the Ada source constructor
         --           takes a new argument Is_RTS_Source, used to handle this
         --           special case when parsing the source.
         --         - Create the GPR2.Project.Source. Nothing special here.
         --           A new check is added to report duplicate project sources.
         --           Add it to the project definition.
         --         - For Ada, create/add the source object to the project
         --           definition: no change from the initial code, but it is
         --           now inside a loop over the compilation units.
         --         - Exit.

         Basename         : constant Filename_Type := File.Path.Simple_Name;

         Match            : Boolean := False;

         Naming_Exception : Source.Naming_Exception_Kind := No;
         Units            : Source.Unit_List;  --  For Ada
         Kind             : Unit_Kind;
         Index            : Unit_Index;
         Source           : Build.Source.Object;
         Attr             : Project.Attribute.Object;
         Ada_Exc_CS       : Source_Path_To_Attribute_List.Cursor;
         Ambiguous_Kind   : Boolean := False;

      begin
         --  Stop here if it's one of the excluded sources, or it's not in the
         --  included sources if those are given explicitely.

         if Excluded_Sources.Contains (Basename)
           or else (not Listed_Sources.Is_Empty
                    and then not Listed_Sources.Contains (Basename))
         then
            return False;
         end if;

         if File.Dir_Ref /= Current_Src_Dir_SR then
            Current_Src_Dir_SR := File.Dir_Ref;
            Source_Name_Set.Clear;
         end if;

         for Language of Data.View.Language_Ids loop
            --  First, try naming exceptions

            Naming_Exception := No;
            Kind  := S_Spec;  --  Dummy value

            if Language = Ada_Language
              and then not Ada_Naming_Exceptions.Is_Empty
            then
               Ada_Exc_CS := Ada_Naming_Exceptions.Find (Basename);

               if Source_Path_To_Attribute_List.Has_Element (Ada_Exc_CS) then
                  for Exc of Ada_Naming_Exceptions (Ada_Exc_CS) loop
                     --  Found at least one: so we have naming exceptions for
                     --  this source

                     if Exc.Value.Has_At_Pos then
                        Naming_Exception := Multi_Unit;
                        Index := Exc.Value.At_Pos;
                        pragma Assert (Index /= No_Index);
                     else
                        Naming_Exception := Yes;
                        Index := No_Index;
                     end if;

                     Kind := (if Exc.Name.Id = PRA.Naming.Spec
                              then S_Spec
                              else S_Body);

                     --  May actually be a Separate, we cannot know
                     --  until we parse the file.

                     --  We know only Name, Index and Kind unit
                     --  properties for now. Others will be taken on
                     --  source parsing.

                     Units.Insert
                       (Build.Source.Create
                          (Unit_Name      => Name_Type (Exc.Index.Text),
                           Index          => Index,
                           Kind           => Kind,
                           Kind_Ambiguous => Kind = S_Body));
                  end loop;
               end if;

            elsif Language /= Ada_Language then
               --  Non-Ada case: we suppose that the language doesn't support
               --  the notion of compilation unit.

               if Data.View.Check_Attribute
                    (PRA.Naming.Specification_Exceptions,
                     Project.Attribute_Index.Create (Language),
                     Result => Attr)
                 and then Attr.Has_Value (Value_Type (Basename))
                 --  ??? Doesn't take care of FS casing
               then
                  Naming_Exception := Yes;
                  Kind  := S_Spec;

               elsif Data.View.Check_Attribute
                       (PRA.Naming.Implementation_Exceptions,
                        Project.Attribute_Index.Create (Language),
                        Result => Attr)
                 and then Attr.Has_Value (Value_Type (Basename))
               then
                  Naming_Exception := Yes;
                  Kind  := S_Body;
               end if;
            end if;

            if Naming_Exception = No then
               --  If no naming exception matched, try with naming schema

               declare
                  use GNATCOLL.Utils;

                  NS               : Naming_Schema renames
                                       Naming_Schema_Map.Element (Language);
                  Matches_Spec     : Boolean;
                  Matches_Body     : Boolean;
                  Matches_Separate : Boolean;

               begin
                  Matches_Spec := NS.Has_Spec_Suffix
                    and then Ends_With (String (Basename), NS.Spec_Suffix);

                  Matches_Body := NS.Has_Body_Suffix
                    and then Ends_With (String (Basename), NS.Body_Suffix);

                  Matches_Separate := NS.Has_Separate_Suffix
                    and then Ends_With (String (Basename), NS.Sep_Suffix);

                  --  See GA05-012: if there's ambiguity with suffixes (e.g.
                  --  one of the suffixes if a suffix of another) we use
                  --  with the most explicit one (e.g. the longest one)
                  --  that matches.

                  if Matches_Spec and then Matches_Body then
                     if NS.Spec_Suffix'Length >= NS.Body_Suffix'Length then
                        Matches_Body := False;
                     else
                        Matches_Spec := False;
                     end if;
                  end if;

                  if Matches_Spec and then Matches_Separate then
                     if NS.Spec_Suffix'Length >= NS.Sep_Suffix'Length then
                        Matches_Separate := False;
                     else
                        Matches_Spec := False;
                     end if;
                  end if;

                  if Matches_Body and then Matches_Separate then
                     if NS.Body_Suffix'Length >= NS.Sep_Suffix'Length then
                        if NS.Body_Suffix'Length = NS.Sep_Suffix'Length then
                           --  Default naming schema for Ada is ambiguous for
                           --  separates.
                           Ambiguous_Kind := True;
                        end if;

                        Matches_Separate := False;
                     else
                        Matches_Body := False;
                     end if;
                  end if;

                  if Matches_Spec then
                     Match := True;
                     Kind  := S_Spec;

                  elsif Matches_Body then
                     Match := True;
                     Kind  := S_Body;

                  elsif Matches_Separate then
                     Match := True;
                     Kind  := S_Separate;

                  else
                     Match := False;
                     Kind  := S_Spec;
                  end if;
               end;

               if Match and then Language = Ada_Language then
                  declare
                     Last_Dot  : Natural;
                     Unit_Name : constant Name_Type :=
                                   Compute_Unit_From_Filename
                                     (File     => File.Path,
                                      Kind     => Kind,
                                      NS       =>
                                        Naming_Schema_Map (Ada_Language),
                                      Dot_Repl => Dot_Repl,
                                      Messages => Messages,
                                      Last_Dot => Last_Dot,
                                      Success  => Match);

                     function Has_Conflict_NE
                       (Attr_Name : Q_Attribute_Id) return Boolean;
                     --  Search the Naming package for attributes with name
                     --  Attr_Name and index Unit_Name, and return True if
                     --  at least one of the matching attributes references
                     --  a different (source,index) than the current one.

                     ---------------------
                     -- Has_Conflict_NE --
                     ---------------------

                     function Has_Conflict_NE
                       (Attr_Name : Q_Attribute_Id) return Boolean
                     is
                        Cursor : Source_Path_To_Attribute_List.Cursor;
                        use Source_Path_To_Attribute_List;
                     begin
                        Cursor := Ada_Naming_Exceptions.Find
                          (Filename_Optional (Unit_Name));

                        if Has_Element (Cursor) then
                           for Attr of Element (Cursor) loop
                              if Attr.Name.Id = Attr_Name then
                                 if not Naming_Exception_Equal
                                   (Attr, Value_Type (Basename), 1)
                                 then
                                    return True;
                                 end if;
                              end if;
                           end loop;
                        end if;

                        return False;
                     end Has_Conflict_NE;

                  begin
                     if Match then
                        --  Check if we have conflicting naming exceptions:
                        --  same (unit,kind) but different source.
                        --  In this case we skip this source.

                        if (Kind = S_Spec
                            and then Has_Conflict_NE (PRA.Naming.Spec))
                          or else
                            (Kind = S_Body
                             and then Has_Conflict_NE (PRA.Naming.Body_N))
                        then
                           return False;
                        end if;

                        if Last_Dot = 0 then
                           --  If not dot in the unit name, then there's no
                           --  confusion between body and separate.

                           Ambiguous_Kind := False;
                        end if;

                        if Kind = S_Separate then
                           pragma Assert
                             (Last_Dot in
                                Unit_Name'First + 1 .. Unit_Name'Last - 1);

                           Units.Insert
                             (Build.Source.Create
                                (Unit_Name      => Unit_Name
                                                     (Unit_Name'First ..
                                                      Last_Dot - 1),
                                 Separate_Name  => Unit_Name
                                                     (Last_Dot + 1 ..
                                                      Unit_Name'Last),
                                 Index          => No_Index,
                                 Kind           => S_Separate,
                                 Kind_Ambiguous => False));
                        else
                           Units.Insert
                             (Build.Source.Create
                                (Unit_Name      => Unit_Name,
                                 Index          => No_Index,
                                 Kind           => Kind,
                                 Kind_Ambiguous => Ambiguous_Kind));
                        end if;
                     end if;
                  end;
               end if;
            end if;

            --  If we have a match from either naming exception or scheme
            --  we create the Source object.

            if Naming_Exception /= No or else Match then
               Has_Src_In_Lang.Include (Language);

               if Language = Ada_Language then
                  Source := Build.Source.Create_Ada
                    (Filename            => File.Path,
                     Timestamp           => File.Stamp,
                     Tree_Db             => Data.Tree_Db,
                     Naming_Exception    => Naming_Exception,
                     Units               => Units,
                     Source_Ref          => File.Dir_Ref);

               else
                  Source := Build.Source.Create
                    (File.Path,
                     Language         => Language,
                     Kind             => Kind,
                     Timestamp        => File.Stamp,
                     Tree_Db          => Data.Tree_Db,
                     Naming_Exception => Naming_Exception,
                     Source_Ref       => File.Dir_Ref,
                     Is_Compilable    => Is_Compilable (Language));
               end if;

               Data.Src_Infos.Insert (File.Path, Source);

               return True;
            end if;

            exit when Stop_On_Error and then Tree.Log_Messages.Has_Error;
         end loop;

         return False;
      end Process_File;

   begin
      --  Skip projects that don't have source directories

      if Data.View.Kind not in With_Source_Dirs_Kind then
         return;
      end if;

      --  Prepare tables used to detect actual sources and their language

      Fill_Naming_Schema (Data.View, Naming_Schema_Map);

      if Data.View.Language_Ids.Contains (Ada_Language) then
         Fill_Ada_Naming_Exceptions
           (Data.View, PRA.Naming.Spec.Attr, Ada_Naming_Exceptions);
         Fill_Ada_Naming_Exceptions
           (Data.View, PRA.Naming.Body_N.Attr, Ada_Naming_Exceptions);
      end if;

      --  If we have attribute Excluded_Source_List_File

      Attr := Data.View.Attribute (PRA.Excluded_Source_List_File);

      if Attr.Is_Defined then
         Read_Source_List
           (Data.View, Attr.Value, Excluded_Sources, Messages);
      end if;

      --  If we have attribute Excluded_Source_Files

      Attr := Data.View.Attribute (PRA.Excluded_Source_Files);

      if Attr.Is_Defined then
         for File of Attr.Values loop
            Include_Simple_Filename (Data, Excluded_Sources, File.Text, File);
         end loop;
      end if;

      --  Remove naming exception sources from inactive case alternatives

      for File of Data.View.Skipped_Sources loop
         Include_Simple_Filename (Data, Excluded_Sources, File.Text, File);
      end loop;

      --  If we have attribute Source_List_File

      Attr := Data.View.Attribute (PRA.Source_List_File);

      if Attr.Is_Defined then
         Read_Source_List
           (Data.View, Attr.Value, Listed_Sources, Messages);
      end if;

      --  If we have attribute Source_Files

      Attr := Data.View.Attribute (PRA.Source_Files);

      if Attr.Is_Defined then
         for File of Attr.Values loop
            Include_Simple_Filename (Data, Listed_Sources, File.Text, File);
         end loop;
      end if;

      --  Lookup for all files in the source directories

      Data.Src_Files.Clear;

      Data.View.Source_Directories_Walk
        (Source_CB => Handle_File'Access, Dir_CB => null);

      Changed_Sources.Clear;

      --  Check deleted files

      for F of Previous_Files loop
         if not Data.Src_Files.Contains (F) then
            --  File F has disapeared, check if it was a source file

            declare
               C : Src_Info_Maps.Cursor;
            begin
               C := Data.Src_Infos.Find (F.Path);

               if Src_Info_Maps.Has_Element (C) then
                  --  File was a source and has disapeared: notify the build
                  --  db object to cleanup tables.
                  Remove_Source
                    (Data, Data.View, F.Path, Project.View.Undefined);

                  Changed_Sources.Include (F.Path.Simple_Name);
               end if;
            end;
         end if;
      end loop;

      --  Check new or updated sources

      for F of Data.Src_Files loop
         declare
            use type Ada.Calendar.Time;

            C     : File_Sets.Cursor;
            C_Src : Src_Info_Maps.Cursor;

         begin
            C := Previous_Files.Find (F);

            if not File_Sets.Has_Element (C) then
               --  New potential source: process it

               if Process_File (F) then
                  --  First add source to Data view.
                  Add_Source
                    (Data, Data.View, F.Path,
                     Extended_View   => Project.View.Undefined);
                  Changed_Sources.Include (F.Path.Simple_Name);
               end if;

            elsif File_Sets.Element (C).Stamp /= F.Stamp then
               C_Src := Data.Src_Infos.Find (F.Path);

               if Src_Info_Maps.Has_Element (C_Src) then
                  Data.Src_Infos.Reference (C_Src).Update_Modification_Time
                    (F.Stamp);
               end if;
            end if;
         end;
      end loop;

      --  All source changes have been processed: now resolve potential
      --  visibility issues

      for Base_Name of Changed_Sources loop
         declare
            C_Overload : Basename_Source_List_Maps.Cursor;
         begin
            C_Overload := Data.Overloaded_Srcs.Find (Base_Name);
            Resolve_Visibility (Data, C_Overload);
         end;
      end loop;

      --  Data.Sources is now updated, we don't need Previous_Files anymore
      Previous_Files.Clear;

      --  Check that we've found all the listed sources
      for S of Listed_Sources loop
         if not Excluded_Sources.Contains (S)
           and then not Data.Sources.Contains (S)
         then
            Messages.Append
              (Message.Create
                 (Message.Error,
                  "source file """ & String (S) & """ not found",
                  (if Data.View.Has_Attribute (PRA.Source_List_File)
                   then Data.View.Attribute (PRA.Source_List_File)
                   else Data.View.Attribute (PRA.Source_Files))));
         end if;
      end loop;

      if Data.View.Has_Package (PRP.Naming,
                                Check_Extended => False,
                                With_Defaults  => False,
                                With_Config    => False)
      then
         --  Check all naming exceptions is used only in the original
         --  project where Naming package is declared. If nameing package is
         --  inherited then not all sources from naming exceptions have to
         --  be defined, some of then inherited from extended project.

         for A of Data.View.Attributes
                    (Name          => PRA.Naming.Spec,
                     With_Defaults => False,
                     With_Config   => False)
         loop
            if not Data.Sources.Contains (Simple_Name (A.Value.Text)) then
               Messages.Append
                 (Message.Create
                    ((if Data.View.Has_Attribute (PRA.Source_Files)
                     or else Data.View.Has_Attribute (PRA.Source_List_File)
                     then Message.Warning
                     else Message.Error),
                     "source file """ & A.Value.Text
                     & """ for unit """ & A.Index.Text
                     & """ not found",
                     A.Value));
            end if;
         end loop;

         for A of Data.View.Attributes
                    (Name          => PRA.Naming.Body_N,
                     With_Defaults => False,
                     With_Config   => False)
         loop
            if not Data.Sources.Contains (Simple_Name (A.Value.Text)) then
               Messages.Append
                 (Message.Create
                    ((if Data.View.Has_Attribute (PRA.Source_Files)
                     or else Data.View.Has_Attribute (PRA.Source_List_File)
                     then Message.Warning
                     else Message.Error),
                     "source file """ & A.Value.Text
                     & """ for unit """ & A.Index.Text
                     & """ not found",
                     A.Value));
            end if;
         end loop;

         for Language of Data.View.Language_Ids loop
            if Data.View.Check_Attribute
              (PRA.Naming.Specification_Exceptions,
               Project.Attribute_Index.Create (Language),
               Result => Attr)
            then
               for V of Attr.Values loop
                  if not Data.Sources.Contains (Simple_Name (V.Text)) then
                     Messages.Append
                       (Message.Create
                          (Message.Warning,
                           "source file """ & V.Text & """ not found",
                           V));
                  end if;
               end loop;
            end if;

            if Data.View.Check_Attribute
              (PRA.Naming.Implementation_Exceptions,
               Project.Attribute_Index.Create (Language),
               Result => Attr)
            then
               for V of Attr.Values loop
                  if not Data.Sources.Contains (Simple_Name (V.Text)) then
                     Messages.Append
                       (Message.Create
                          (Message.Warning,
                           "source file """ & V.Text & """ not found",
                           V));
                  end if;
               end loop;
            end if;
         end loop;
      end if;
   end Process;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_Source_List
     (View      : Project.View.Object;
      Filename  : Source_Reference.Value.Object;
      Set       : in out Source_Set.Set;
      Messages  : in out GPR2.Log.Object)
   is
      use Ada.Strings;
      Fullname   : constant GPR2.Path_Name.Full_Name :=
                     (if GNAT.OS_Lib.Is_Absolute_Path (Filename.Text)
                      then Filename.Text
                      else View.Dir_Name.Compose
                        (Filename_Type (Filename.Text)).Value);
      F          : Text_IO.File_Type;
   begin
      Text_IO.Open (F, Text_IO.In_File, Fullname);

      while not Text_IO.End_Of_File (F) loop
         declare
            use GNATCOLL.Utils;

            Line     : constant String :=
                         Fixed.Trim
                           (Text_IO.Get_Line (F),
                            Maps.Constants.Control_Set,
                            Maps.Constants.Control_Set);
            Position : Source_Set.Cursor;
            Inserted : Boolean;

         begin
            if Line /= "" and then not Starts_With (Line, "-- ") then
               if Has_Directory_Separator (Line) then
                  Messages.Append
                    (Message.Create
                       (Message.Error,
                        "file name cannot include directory information ("""
                        & Line & """)",
                        Filename));
               else
                  Set.Insert (Filename_Type (Line), Position, Inserted);
               end if;
            end if;
         end;
      end loop;

      Text_IO.Close (F);
   end Read_Source_List;

end Update_Sources_List;
