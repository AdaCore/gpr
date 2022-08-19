--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNAT.MD5;

with GPR2.Unit.List;
with GPR2.Source;
with GPR2.Source_Reference.Identifier.Set;

separate (GPR2.Project.Definition)
procedure Update_Sources_List
  (Def           : in out Data;
   View          : Project.View.Object;
   Stop_On_Error : Boolean)
is
   use type MD5.Binary_Message_Digest;
   use type Project.Attribute.Object;

   use GPR2.Containers;
   use GPR2.Path_Name;

   Root : constant GPR2.Path_Name.Object := Def.Path;

   Current_Src_Dir_SR : GPR2.Source_Reference.Value.Object;
   --  Identifies the Source_Dirs value being processed

   Source_Name_Set    : GPR2.Containers.Filename_Set;
   --  Collection of source simple names for a given Source_Dirs value

   package Lang_Boolean_Map is new Ada.Containers.Hashed_Maps
     (Language_Id, Boolean, Hash, "=");

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

   package Naming_Exceptions_Usage renames Value_Source_Reference_Package;

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

   procedure Register_Units
     (Source : Project.Source.Object)
     with Pre => Source.Language = Ada_Language;
   --  Registers units for the given project source. Note that we need to
   --  pass the Units and not to use the one registered with the
   --  source as the later could have been updated by a real parser based on
   --  Libadalang for example. And in this case the units name could be non
   --  matching. This is true for the initial call in Handle_File.

   type Insert_Mode is (Extended_Copy, Aggregated_Copy);
   --  Controls behavior when a duplicated unit/filename is found
   --
   --  Extended_Copy   : the new source is ignored
   --  Aggregated_Copy : an error is raised

   package Source_Set renames Containers.Filename_Type_Set;

   procedure Handle_File
     (Dir_Ref   : SR.Value.Object;
      File      : GPR2.Path_Name.Object;
      Timestamp : Ada.Calendar.Time);
   --  Processes the given file: see if it should be added to the view's
   --  sources, and compute information such as language/unit(s)/...

   function Signature return MD5.Binary_Message_Digest;
   --  Compute the signature corresponding to the source context. If the
   --  signature is not the same recorded for the view, the source set
   --  need to be recomputed.

   procedure Read_Source_List
     (Attr_Name : Q_Attribute_Id;
      Set       : in out Source_Set.Set);
   --  Read from file defined in project attribute Attr_Name and insert each
   --  line into Set

   procedure Insert
     (Sources : Project.Source.Set.Object;
      Mode    : Insert_Mode;
      Sloc    : SR.Object'Class);
   --  Insert Sources from an extended or aggregated project into
   --  Def.Sources. Mode is Skip for extended projects (ignore sources from
   --  the extended project that have been replaced in the extending one),
   --  or Error for aggregated projects (reject duplicate sources).

   procedure Include_Simple_Filename
     (Set   : in out Source_Set.Set;
      Value : Value_Type;
      Sloc  : SR.Value.Object);
   --  Includes Value into Set. If Value contains directory separator put
   --  error message into log.

   procedure Fill_Naming_Schema;

   procedure Fill_Ada_Naming_Exceptions (Attr : Q_Attribute_Id)
     with Pre => Attr in  PRA.Naming.Spec | PRA.Naming.Body_N;
   --  Fill the Ada_Naming_Exceptions object with the given attribute set

   procedure Fill_Other_Naming_Exceptions
     (Set : Project.Attribute.Set.Object)
     with Pre =>
       (for all A of Set =>
          A.Name.Id = PRA.Naming.Specification_Exceptions
          or else A.Name.Id = PRA.Naming.Implementation_Exceptions);

   function Is_Compilable (Language : Language_Id) return Boolean;
   --  Check whether the language is compilable on the current View. This
   --  includes information provided by the Tree (Driver attribute). Note
   --  that this routine caches the result into a map.

   Dot_Repl : constant String :=
                View.Attribute (PRA.Naming.Dot_Replacement).Value.Text;
   --  Get Dot_Replacement value

   Naming_Schema_Map : Naming_Schema_Maps.Map;

   Is_Standard_GNAT_Naming : constant  Boolean :=
                               (View.Spec_Suffix
                                    (Ada_Language).Value.Text = ".ads")
                                  and then
                               (View.Body_Suffix
                                    (Ada_Language).Value.Text = ".adb")
                                  and then
                               (Dot_Repl = "-");
   --  True if the current naming scheme is GNAT's default naming scheme.
   --  This is to take into account shortened names like "Ada." (a-),
   --  "System." (s-) and so on.

   Included_Sources  : Source_Set.Set;
   Excluded_Sources  : Source_Set.Set;
   Has_Source_List   : Boolean := False;
   --  Has either Source_Files or Source_List_File attributes

   Inserted              : Boolean;
   Language_Compilable   : Lang_Boolean_Map.Map;
   Has_Src_In_Lang       : Language_Set;
   --  Insert record there if the language has a source

   Tree                  : constant not null access Project.Tree.Object :=
                             Def.Tree;
   Message_Count         : constant Containers.Count_Type :=
                             Tree.Log_Messages.Count;

   Ada_Naming_Exceptions : Source_Path_To_Attribute_List.Map;
   Ada_Except_Usage      : Naming_Exceptions_Usage.Map;
   Other_Except_Usage    : Filename_Source_Reference;

   procedure Mark_Language (Lang : Language_Id);
   --  Mark that language exists in sources

   function Ada_Use_Index (Attr : Attribute.Object) return Value_Type is
     (Attr.Index.Text & Image (Attr.Name.Id.Attr) (1));
   --  Index created from Body or Spec attribute index i.e. Ada unit name
   --  and first character of the attribute name i.e. B or S. It is used to
   --  distinct body naming exception from spec naming exception.

   --------------------------------
   -- Fill_Ada_Naming_Exceptions --
   --------------------------------

   procedure Fill_Ada_Naming_Exceptions (Attr : Q_Attribute_Id) is
   begin
      for A of View.Attributes
        (Name          => Attr,
         With_Defaults => False,
         With_Config   => False)
      loop
         declare
            Source          : constant Filename_Type :=
                                Filename_Type (A.Value.Text);
            Insert_Position : Source_Path_To_Attribute_List.Cursor;
            Is_Inserted     : Boolean;
         begin
            Ada_Naming_Exceptions.Insert
              (Key      => Source,
               New_Item => Attribute_List.Empty_List,
               Position => Insert_Position,
               Inserted => Is_Inserted);
            Ada_Naming_Exceptions (Insert_Position).Append (A);
            Ada_Except_Usage.Insert (Ada_Use_Index (A), A.Value);
         end;
      end loop;
   end Fill_Ada_Naming_Exceptions;

   ------------------------
   -- Fill_Naming_Schema --
   ------------------------

   procedure Fill_Naming_Schema
   is
   begin
      for L of View.Languages loop
         declare
            Lang : constant Language_Id := +Name_Type (L.Text);
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
            Naming_Schema_Map.Insert
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

   ----------------------------------
   -- Fill_Other_Naming_Exceptions --
   ----------------------------------

   procedure Fill_Other_Naming_Exceptions
     (Set : Project.Attribute.Set.Object)
   is
      CE : Filename_Source_Reference_Package.Cursor;
      OK : Boolean;
   begin
      for A of Set loop
         for V of A.Values loop
            Other_Except_Usage.Insert (Filename_Type (V.Text), V, CE, OK);

            if not OK then
               Tree.Append_Message
                 (Message.Create
                    (Message.Error,
                     "File """ & V.Text
                     & """ specified in naming exception more than once",
                     V));
            end if;
         end loop;
      end loop;
   end Fill_Other_Naming_Exceptions;

   -----------------
   -- Handle_File --
   -----------------

   procedure Handle_File
     (Dir_Ref   : SR.Value.Object;
      File      : GPR2.Path_Name.Object;
      Timestamp : Ada.Calendar.Time)
   is
      use all type GPR2.Project.Source.Naming_Exception_Kind;
      use all type Unit.Library_Unit_Type;

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

      procedure Check_Naming_Exceptions
        (Basename : Filename_Type;
         Language : Language_Id;
         Match    : out Boolean;
         Kind     : out Unit.Library_Unit_Type);
      --  Try to match a file using its Basename and the project's
      --  naming exceptions for Language.
      --  If Language is Ada, use the attributes "for Body|Spec ... ".
      --  For other languages, use the attributes:
      --    for (Implementation|Specification)_Exceptions ...".
      --  If success, set Match to True and Kind to the appropriate value.

      procedure Check_Naming_Scheme
        (Basename : Value_Type;
         Language : Language_Id;
         Match    : out Boolean;
         Kind     : out Unit.Library_Unit_Type);
      --  Try to match a file using its extension and the project's
      --  naming scheme for Language.
      --  If Language is Ada, use the attributes "for (Body|Spec|
      --    Separate)_Suffix ... ".
      --  For other languages, use only Body|Spec.
      --  If success, set Match to True and Kind to the appropriate value.

      function Compute_Unit_From_Filename
        (File     : Path_Name.Object;
         Kind     : Unit.Library_Unit_Type;
         Last_Dot : out Natural;
         Success  : out Boolean) return Name_Type;
      --  For an Ada source and given its kind, try to compute a valid unit
      --  name. Success takes True if such a valid name is found.
      --  Set Last_Dot to last dot index in result to split separate unit
      --  name.

      function Is_Valid_Unit_Name (Unit_Name : Name_Type) return Boolean;
      --  Check that unit name is correct

      -----------------------------
      -- Check_Naming_Exceptions --
      -----------------------------

      procedure Check_Naming_Exceptions
        (Basename : Filename_Type;
         Language : Language_Id;
         Match    : out Boolean;
         Kind     : out Unit.Library_Unit_Type)
      is
         Attr : Attribute.Object;
      begin
         Match := False;
         Kind  := Unit.S_Spec;  --  Dummy value

         if Language = Ada_Language then
            Match := Ada_Naming_Exceptions.Contains (Basename);

         else
            if View.Check_Attribute
              (PRA.Naming.Specification_Exceptions,
               Attribute_Index.Create (Language),
               Result => Attr)
              and then Attr.Has_Value (Value_Type (Basename))
            then
               Match := True;
               Kind  := Unit.S_Spec;

            elsif View.Check_Attribute
              (PRA.Naming.Implementation_Exceptions,
               Attribute_Index.Create (Language),
               Result => Attr)
              and then Attr.Has_Value (Value_Type (Basename))
            then
               Match := True;
               Kind  := Unit.S_Body;
            end if;

            if Match then
               declare
                  use Filename_Source_Reference_Package;

                  C : Cursor := Other_Except_Usage.Find (Basename);
               begin
                  if Has_Element (C) then
                     Other_Except_Usage.Delete (C);
                  else
                     --  Basename was already handled, don't handle twice.

                     Match := False;
                     Kind  := Unit.S_Spec;  --  Dummy value
                  end if;
               end;
            end if;
         end if;
      end Check_Naming_Exceptions;

      -------------------------
      -- Check_Naming_Scheme --
      -------------------------

      procedure Check_Naming_Scheme
        (Basename : Value_Type;
         Language : Language_Id;
         Match    : out Boolean;
         Kind     : out Unit.Library_Unit_Type)
      is

         function Test_Charset (Suffix : String) return Boolean;
         --  Check that the filename complies with the naming defined
         --  charset and the dot replacement is ada compliant.

         ------------------
         -- Test_Charset --
         ------------------

         function Test_Charset (Suffix : String) return Boolean is
            use Ada.Strings.Maps;
            Casing  : constant String :=
                        ACH.To_Lower
                          (View.Attribute (PRA.Naming.Casing).Value.Text);
            Charset : constant Character_Set :=
                        (if not File_Names_Case_Sensitive
                         or else Casing = "mixedcase"
                         then Constants.Letter_Set
                         elsif Casing = "lowercase"
                         then Constants.Lower_Set
                         elsif Casing = "uppercase"
                         then Constants.Upper_Set
                         else Null_Set);
            --  On Windows, file names are case insensitive, so Casing
            --  attribute is irrelevant and Letter_Set is used

            J       : Positive := Basename'First; -- Iterates over Basename
            DP      : Positive := Basename'First;
            --  Next char after last dot replacement
            DD      : Positive := Basename'First;
            --  Next char after last dot replacement or underscore.
            --  To avoid dot replacements and underscores to be one after
            --  another.

         begin
            while J <= Basename'Last - Suffix'Length loop
               if Is_In (Basename (J), Charset) then
                  J := J + 1;

               elsif J + Dot_Repl'Length <= Basename'Last - Suffix'Length
                 and then DD < J -- Don't after underscore or dot replace
                 and then
                   (Basename (J .. J + Dot_Repl'Length - 1) = Dot_Repl
                    or else
                    --  In the standard GNAT naming scheme,
                    --  handle children or separates of A, G, I or S.
                    --  "~" is used as dot replacement by gnatkr
                      (Is_Standard_GNAT_Naming
                       and then Basename (J) = '~'
                       and then J = Basename'First + 1
                       and then Basename (Basename'First) in
                           'a' | 'g' | 'i' | 's'))
               then
                  if Basename (J .. J + Dot_Repl'Length - 1) = Dot_Repl then
                     J := J + Dot_Repl'Length;
                  else
                     J := J + 1;
                  end if;

                  DD := J;
                  DP := J;

               elsif Basename (J) in '0' .. '9' and then DP < J then
                  --  Decimal can't be next char after dot replacement

                  J := J + 1;

               elsif Basename (J) = '_' then
                  if DD < J then
                     J := J + 1;
                     DD := J;
                  else
                     --  Double underscores and not dot replacement

                     return False;
                  end if;

               else
                  return False;
               end if;
            end loop;

            return True;
         end Test_Charset;

         Matches_Spec     : Boolean;
         Matches_Body     : Boolean;
         Matches_Separate : Boolean;
         NS               : constant Naming_Schema :=
                              Naming_Schema_Map.Element (Language);
         use GNATCOLL.Utils;

      begin
         Matches_Spec := NS.Has_Spec_Suffix
           and then Ends_With (Basename, NS.Spec_Suffix);

         Matches_Body := NS.Has_Body_Suffix
           and then Ends_With (Basename, NS.Body_Suffix);

         Matches_Separate := NS.Has_Separate_Suffix
           and then Ends_With (Basename, NS.Sep_Suffix);

         --  See GA05-012: if there's ambiguity with suffixes (e.g. one of
         --  the suffixes if a suffix of another) we use with the most
         --  explicit one (e.g. the longest one) that matches.

         if Matches_Spec and then Matches_Body then
            if NS.Spec_Suffix'Length >= NS.Body_Suffix'Length then
               pragma Assert (Ends_With (NS.Spec_Suffix, NS.Body_Suffix));
               Matches_Body := False;
            else
               pragma Assert (Ends_With (NS.Body_Suffix, NS.Spec_Suffix));
               Matches_Spec := False;
            end if;
         end if;

         if Matches_Spec and then Matches_Separate then
            if NS.Spec_Suffix'Length >= NS.Sep_Suffix'Length then
               pragma Assert (Ends_With (NS.Spec_Suffix, NS.Sep_Suffix));
               Matches_Separate := False;
            else
               pragma Assert (Ends_With (NS.Sep_Suffix, NS.Spec_Suffix));
               Matches_Spec := False;
            end if;
         end if;

         if Matches_Body and then Matches_Separate then
            if NS.Body_Suffix'Length >= NS.Sep_Suffix'Length then
               pragma Assert (Ends_With (NS.Body_Suffix, NS.Sep_Suffix));
               Matches_Separate := False;
            else
               pragma Assert (Ends_With (NS.Sep_Suffix, NS.Body_Suffix));
               Matches_Body := False;
            end if;
         end if;

         --  Additional check: dot replacement and charset
         if Language = Ada_Language then
            if Matches_Spec then
               Matches_Spec := Test_Charset (NS.Spec_Suffix);
            elsif Matches_Body then
               Matches_Body := Test_Charset (NS.Body_Suffix);
            elsif Matches_Separate then
               Matches_Separate := Test_Charset (NS.Sep_Suffix);
            end if;
         end if;

         if Matches_Spec then
            Match := True;
            Kind  := Unit.S_Spec;

         elsif Matches_Body then
            Match := True;
            Kind  := Unit.S_Body;

         elsif Matches_Separate then
            Match := True;
            Kind  := Unit.S_Separate;

         else
            Match := False;
            Kind  := Unit.S_Spec;
         end if;
      end Check_Naming_Scheme;

      --------------------------------
      -- Compute_Unit_From_Filename --
      --------------------------------

      function Compute_Unit_From_Filename
        (File     : Path_Name.Object;
         Kind     : Unit.Library_Unit_Type;
         Last_Dot : out Natural;
         Success  : out Boolean) return Name_Type
      is
         use Ada.Strings;
         use Ada.Strings.Maps;

         Result : Unbounded_String :=
                    To_Unbounded_String (String (File.Simple_Name));
      begin
         --  First remove the suffix for the given language

         declare
            Suffix : constant Value_Type :=
                       (case Kind is
                           when Unit.Spec_Kind =>
                             Naming_Schema_Map (Ada_Language).Spec_Suffix,
                           when Unit.Body_Kind =>
                             Naming_Schema_Map (Ada_Language).Body_Suffix,
                           when S_Separate     =>
                             Naming_Schema_Map (Ada_Language).Sep_Suffix);
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
               Tree.Append_Message
                 (Message.Create
                    (Message.Error, "invalid name, contains dot",
                     SR.Create (File.Value, 1, 1)));
               Last_Dot := 0;
               goto Invalid;

            else
               declare
                  I : Natural := 1;
               begin
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

         if Is_Standard_GNAT_Naming and then Length (Result) >= 3 then
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

         declare
            Unit_Name : constant Name_Type :=
                          Name_Type (To_String (Result));
         begin
            --  Some additional checks on the unit name
            if not Is_Valid_Unit_Name (Unit_Name) then
               goto Invalid;
            end if;

            Success := True;

            return Unit_Name;
         end;

         <<Invalid>>

         Success := False;

         return "0"; -- Some dummy unit name
      end Compute_Unit_From_Filename;

      ------------------------
      -- Is_Valid_Unit_Name --
      ------------------------

      function Is_Valid_Unit_Name (Unit_Name : Name_Type) return Boolean is

         procedure On_Error (Text : String);

         procedure On_Error (Text : String) is
         begin
            Tree.Append_Message
              (Message.Create (Message.Error, Text, Current_Src_Dir_SR));
         end On_Error;

      begin
         return Unit.Valid_Unit_Name (Unit_Name, On_Error'Access);
      end Is_Valid_Unit_Name;

      Languages : constant Project.Attribute.Object :=
                    View.Attribute (PRA.Languages);

      Basename  : constant Filename_Type := File.Simple_Name;

      Match            : Boolean := False;

      Naming_Exception : Project.Source.Naming_Exception_Kind := No;
      Units            : Unit.List.Object;  --  For Ada
      Kind             : Unit.Library_Unit_Type;
      Source           : GPR2.Source.Object;

      function Naming_Exception_Equal
        (A : Attribute.Object;
         B : Value_Type;
         I : Unit_Index) return Boolean
      is (A.Value.Text = B and then At_Pos_Or (A.Value, 1) = I);

   begin
      --  Stop here if it's one of the excluded sources, or it's not in the
      --  included sources if those are given explicitely.

      if Excluded_Sources.Contains (Basename)
        or else (Has_Source_List
                 and then not Included_Sources.Contains (Basename))
      then
         return;
      end if;

      if Dir_Ref /= Current_Src_Dir_SR then
         Current_Src_Dir_SR := Dir_Ref;
         Source_Name_Set.Clear;
      end if;

      for L of Languages.Values loop
         declare
            Language        : constant Language_Id := +Name_Type (L.Text);
            Is_Indexed      : Boolean := False;
         begin
            --  First, try naming exceptions

            Check_Naming_Exceptions
              (Basename => Basename,
               Language => Language,
               Match    => Match,
               Kind     => Kind);

            if Match then
               --  Got some naming exceptions for the source

               Naming_Exception := Yes;

               if Language = Ada_Language then
                  --  For Ada, fill the compilation units

                  for Exc of Ada_Naming_Exceptions (Basename) loop
                     declare
                        Unit_Name : constant Name_Type :=
                                      Name_Type (Exc.Index.Text);
                        Index     : Unit_Index;
                        Value     : constant SR.Value.Object := Exc.Value;
                        Pos       : Naming_Exceptions_Usage.Cursor :=
                                      Ada_Except_Usage.Find
                                        (Ada_Use_Index (Exc));
                     begin
                        if Naming_Exceptions_Usage.Has_Element (Pos) then
                           if Value.Has_At_Pos then
                              Naming_Exception := Multi_Unit;
                              Is_Indexed       := True;
                              Index            := Value.At_Pos;
                              pragma Assert (Index /= No_Index);
                           else
                              Index := No_Index;
                           end if;

                           Kind := (if Exc.Name.Id = PRA.Naming.Spec
                                    then Unit.S_Spec
                                    else Unit.S_Body);
                           --  May actually be a Separate, we cannot know
                           --  until we parse the file.

                           Ada_Except_Usage.Delete (Pos);

                           --  We know only Name, Index and Kind unit
                           --  properties for now. Others will be taken on
                           --  source parsing.

                           if Is_Valid_Unit_Name (Unit_Name) then
                              Units.Insert
                                (Unit.Create
                                   (Name          => Unit_Name,
                                    Index         => Index,
                                    Lib_Unit_Kind => Kind,
                                    Lib_Item_Kind => Unit.Is_Package,
                                    Main          => Unit.None,
                                    Flags         => Unit.Default_Flags,
                                    Dependencies  => SRI.Set.Empty_Set,
                                    Sep_From      => No_Name));
                           else
                              Match := False;
                           end if;

                        else
                           --  Duplicated source file in naming exception

                           Match := False;
                        end if;
                     end;
                  end loop;
               end if;

            else
               --  If no naming exception matched, try with naming scheme

               Check_Naming_Scheme
                 (Basename => Value_Type (Basename),
                  Language => Language,
                  Match    => Match,
                  Kind     => Kind);

               if Match and then Language = Ada_Language then
                  --  For Ada, create a single compilation unit

                  declare
                     Last_Dot  : Natural;
                     Unit_Name : constant Name_Type :=
                                   Compute_Unit_From_Filename
                                     (File     => File,
                                      Kind     => Kind,
                                      Last_Dot => Last_Dot,
                                      Success  => Match);

                     function Has_Conflict_NE
                       (Attr_Name : Q_Attribute_Id) return Boolean;
                     --  Search the Naming package for attributes with name
                     --  Attr_Name and index Unit_Name, and return True if
                     --  at least one of the matching attributes references
                     --  a different (source,index) than the current one.

                     procedure Append_Unit
                       (Name : Name_Type; Sep_From : Optional_Name_Type);
                     --  Append unit into Units

                     -----------------
                     -- Append_Unit --
                     -----------------

                     procedure Append_Unit
                       (Name : Name_Type; Sep_From : Optional_Name_Type) is
                     begin
                        Units.Insert
                          (Unit.Create
                             (Name          => Name,
                              Index         => No_Index,
                              Main          => Unit.None,
                              Flags         => Unit.Default_Flags,
                              Lib_Unit_Kind => Kind,
                              Lib_Item_Kind => Unit.Is_Package,
                              Dependencies  => SRI.Set.Empty_Set,
                              Sep_From      => Sep_From));
                     end Append_Unit;

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

                        if (Kind = Unit.S_Spec
                            and then Has_Conflict_NE (PRA.Naming.Spec))
                          or else
                            (Kind = Unit.S_Body
                             and then Has_Conflict_NE (PRA.Naming.Body_N))
                        then
                           return;
                        end if;

                        if Kind = Unit.S_Separate then
                           pragma Assert
                             (Last_Dot in
                                Unit_Name'First + 1 .. Unit_Name'Last - 1);

                           Append_Unit
                             (Unit_Name (Last_Dot + 1 .. Unit_Name'Last),
                              Unit_Name (Unit_Name'First .. Last_Dot - 1));
                        else
                           Append_Unit (Unit_Name, No_Name);
                        end if;
                     end if;
                  end;
               end if;
            end if;

            --  Got a match from either naming exception or scheme

            if Match then
               Mark_Language (Language);

               if Language = Ada_Language then
                  if Is_Indexed then
                     Source := GPR2.Source.Object
                       (GPR2.Source.Create_Ada
                          (Filename      => File,
                           Units         => Units,
                           Timestamp     => Timestamp));
                  else
                     Source := GPR2.Source.Object
                       (GPR2.Source.Create_Ada
                          (Filename      => File,
                           Unit          => Units (No_Index),
                           Is_RTS_Source => View.Is_Runtime,
                           Timestamp     => Timestamp));
                  end if;

               else
                  Source := GPR2.Source.Object
                    (GPR2.Source.Create (File, Language, Kind, Timestamp));
               end if;

               --  Final processing

               --  if Source_Is_In_Interface then
               --     Interface_Sources.Exclude (Basename);
               --  end if;

               declare
                  Project_Source : constant GPR2.Project.Source.Object :=
                                     Project.Source.Create
                                       (Source           => Source,
                                        View             => View,
                                        Naming_Exception =>
                                          Naming_Exception,
                                        Is_Compilable    => Is_Compilable
                                                              (Language));

                  --  Check source duplication and insert if possible or
                  --  replace if necessary.

                  CS             : Project.Source.Set.Cursor :=
                                     Def.Sources.Find (Project_Source);
               begin
                  if Project.Source.Set.Has_Element (CS) then
                     if not Def.Sources (CS).Has_Naming_Exception
                       and then Project_Source.Has_Naming_Exception
                     then
                        --  We are here only when
                        --  Src_Dir_Set (CS).Has_Naming_Exception is False
                        --  and Project_Source.Has_Naming_Exception is True.
                        --  Module with naming exception has priority over
                        --  default naming. Replace the old source with the
                        --  new one.

                        Def.Sources.Replace (Project_Source);

                     elsif Def.Sources (CS).Has_Naming_Exception
                       and then not Project_Source.Has_Naming_Exception
                     then
                        --  Old source has naming exception but new one
                        --  does not have it. We don't need to do anything
                        --  because of more priority source already in its
                        --  place.

                        return;

                     elsif Source_Name_Set.Contains
                       (Project_Source.Path_Name.Simple_Name)
                     then
                        --  Remaining case is when both sources have the
                        --  same naming exception. If they also comme from
                        --  the same base directory value (because of
                        --  recursive search there), then we issue an error
                        --  as the first source found is fs-dependent.

                        Tree.Append_Message
                          (Message.Create
                             (Message.Error,
                              '"' & String (File.Simple_Name) & '"'
                              & " is found in several source directories",
                              Current_Src_Dir_SR));
                     end if;

                  else
                     Def.Sources.Insert (Project_Source, CS, Inserted);
                     Def.Sources_Map_Insert (Project_Source, CS);
                     Source_Name_Set.Include
                       (Project_Source.Path_Name.Simple_Name);
                  end if;

                  --  For Ada, register the Unit object into the view

                  if Language = Ada_Language then
                     Register_Units (Project_Source);
                  end if;
               end;

               --  Exit the languages loop

               exit;
            end if;
         end;
      end loop;
   end Handle_File;

   -----------------------------
   -- Include_Simple_Filename --
   -----------------------------

   procedure Include_Simple_Filename
     (Set   : in out Source_Set.Set;
      Value : Value_Type;
      Sloc  : SR.Value.Object)
   is
      Position : Source_Set.Cursor;
      Inserted : Boolean;
   begin
      if Has_Directory_Separator (Value) then
         Tree.Append_Message
           (Message.Create
              (Message.Error,
               "file name cannot include directory information (""" & Value
               & """)",
               Sloc));
      else
         Set.Insert (Filename_Type (Value), Position, Inserted);
      end if;
   end Include_Simple_Filename;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Sources : Project.Source.Set.Object;
      Mode    : Insert_Mode;
      Sloc    : SR.Object'Class)
   is
      procedure Add_Source (Src : Project.Source.Object);

      procedure Source_Message (Src : Project.Source.Object);

      procedure Exclude_Recursively
        (View      : in out Project.View.Object;
         Source    : Project.Source.Object);

      ----------------
      -- Add_Source --
      ----------------

      procedure Add_Source (Src : Project.Source.Object) is
         --
         --  TODO: avoid the code duplication from Handle_File
         --

         Language : constant Language_Id := Src.Language;

      begin
         --  Mark the language as used

         Mark_Language (Language);

         --  Final processing

         declare
            Position : Project.Source.Set.Cursor;
            Inserted : Boolean;
         begin
            Def.Sources.Insert (Src, Position, Inserted);
            pragma Assert (Inserted);
            Def.Sources_Map_Insert (Src, Position);
         end;

         --  For Ada, register the Unit object into the view

         if Language = Ada_Language then
            Register_Units (Src);
         end if;
      end Add_Source;

      -------------------------
      -- Exclude_Recursively --
      -------------------------

      procedure Exclude_Recursively
        (View      : in out Project.View.Object;
         Source    : Project.Source.Object)
      is
         Def : constant Ref := Get_RW (View);
      begin
         if Def.Sources.Contains (Source) then
            Def.Sources.Delete (Source);
            Def.Sources_Map.Delete (Source.Path_Name.Simple_Name);

            if Source.Has_Units then
               for U of Source.Units loop
                  Def.Units_Map.Delete (Key (U));
               end loop;
            end if;

            if Def.Extended_Root.Is_Defined then
               Exclude_Recursively (Def.Extended_Root, Source);
            end if;
         end if;
      end Exclude_Recursively;

      --------------------
      -- Source_Message --
      --------------------

      procedure Source_Message (Src : Project.Source.Object) is
      begin
         Tree.Append_Message
           (Message.Create
              (Message.Error,
               "project """ & String (Src.Aggregated.Name)
               & """, """ & Src.Path_Name.Value & '"',
               Sloc, Indent => 1));
      end Source_Message;

      C : Project.Source.Set.Cursor;

   begin
      for Source of Sources loop
         C := Def.Sources.Find (Source);

         if Project.Source.Set.Has_Element (C) then
            case Mode is
               when Aggregated_Copy =>
                  Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "source """ & String (Source.Path_Name.Simple_Name)
                        & """ cannot belong to several projects",
                        Sloc));

                  Source_Message (Project.Source.Set.Element (C));
                  Source_Message (Source);

               when Extended_Copy =>
                  null;
            end case;

         elsif not Excluded_Sources.Contains (Source.Path_Name.Simple_Name)
         then
            --  Do not just insert into Def.Sources: we need to do the same
            --  operations as in Handle_File, except that the Source object
            --  is already constructed here.

            Add_Source
              (if Mode = Extended_Copy
               then Change_Actual_View (Source, View)
               else Source);

         elsif Mode = Extended_Copy then
            Exclude_Recursively (Def.Extended_Root, Source);
         end if;
      end loop;
   end Insert;

   -------------------
   -- Is_Compilable --
   -------------------

   function Is_Compilable (Language : Language_Id) return Boolean is

      function Check_View (View : Project.View.Object) return Boolean
        with Pre => View.Is_Defined;
      --  Check if View has a driver for the source language

      ----------------
      -- Check_View --
      ----------------

      function Check_View (View : Project.View.Object) return Boolean is
         Att : Project.Attribute.Object;
      begin
         if View.Has_Package (PRP.Compiler) then
            if View.Check_Attribute
              (PRA.Compiler.Driver,
               Attribute_Index.Create (Language),
               Result => Att)
            then
               return Att.Value.Text /= "";
            end if;
         end if;

         return False;
      end Check_View;

      Res : Boolean;

   begin
      if Language_Compilable.Contains (Language) then
         Res := Language_Compilable (Language);

      else
         Res := Check_View (View);

         if not Res and then View.Tree.Has_Configuration then
            Res := Check_View (View.Tree.Configuration.Corresponding_View);
         end if;

         Language_Compilable.Insert (Language, Res);
      end if;

      return Res;
   end Is_Compilable;

   -------------------
   -- Mark_Language --
   -------------------

   procedure Mark_Language (Lang : Language_Id) is
      CL : Language_Id_Set.Cursor;
      OK : Boolean;
   begin
      Has_Src_In_Lang.Insert (Lang, CL, OK);
   end Mark_Language;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_Source_List
     (Attr_Name : Q_Attribute_Id;
      Set       : in out Source_Set.Set)
   is
      Attr_Value : constant SR.Value.Object :=
                     Def.Attrs.Element (Attr_Name.Attr).Value;
      Filename   : constant GPR2.Path_Name.Full_Name :=
                     (if GNAT.OS_Lib.Is_Absolute_Path (Attr_Value.Text)
                      then Attr_Value.Text
                      else Root.Compose
                        (Filename_Type (Attr_Value.Text)).Value);
      F          : Text_IO.File_Type;
   begin
      Text_IO.Open (F, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (F) loop
         declare
            use Ada.Strings;
            use GNATCOLL.Utils;
            Line : constant String :=
                     Fixed.Trim
                       (Text_IO.Get_Line (F),
                        Maps.Constants.Control_Set,
                        Maps.Constants.Control_Set);
         begin
            if Line /= "" and then not Starts_With (Line, "-- ") then
               Include_Simple_Filename (Set, Line, Attr_Value);
            end if;
         end;
      end loop;

      Text_IO.Close (F);
   end Read_Source_List;

   --------------------
   -- Register_Units --
   --------------------

   procedure Register_Units
     (Source : Project.Source.Object)
   is

      File : constant Path_Name.Object := Source.Path_Name;

      procedure Register_Src
        (U_Def : in out Unit_Info.Object;
         Index : Unit_Index;
         Kind  : Unit.Library_Unit_Type);
      --  Register Project_Source into U_Def, according to its kind

      ------------------
      -- Register_Src --
      ------------------

      procedure Register_Src
        (U_Def : in out Unit_Info.Object;
         Index : Unit_Index;
         Kind  : Unit.Library_Unit_Type)
      is
         use all type Unit.Library_Unit_Type;
      begin
         case Kind is
            when Unit.Spec_Kind =>
               U_Def.Update_Spec ((Source.Path_Name, Index));
            when Unit.Body_Kind =>
               U_Def.Update_Body ((Source.Path_Name, Index));
            when S_Separate =>
               U_Def.Update_Separates ((Source.Path_Name, Index));
         end case;
      end Register_Src;

   begin
      for CU of Source.Units loop
         declare
            Unit_Name : constant Name_Type := CU.Name;
            Position  : Unit_Info.Set.Cursor;
            Inserted  : Boolean;
         begin
            Def.Tree.Record_View
              (View   => View,
               Source => File,
               Unit   => Unit_Name);

            Def.Units.Insert
              (Unit_Name,
               Unit_Info.Create
                 (Unit_Name,
                  Spec      => (Path_Name.Undefined, No_Index),
                  Main_Body => (Path_Name.Undefined, No_Index),
                  Separates => Unit.Source_Unit_Vectors.Empty_Vector),
               Position,
               Inserted);

            Register_Src (Def.Units (Position), CU.Index, CU.Kind);
         end;
      end loop;
   end Register_Units;

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

         procedure Add (Attribute_Name : Q_Attribute_Id);
         --  Add attribute by into the MD5 context

         ---------
         -- Add --
         ---------

         procedure Add (A : Project.Attribute.Object) is
         begin
            MD5.Update (C, String (Name (A.Name.Id.Attr)) & "/");
            for Value of A.Values loop
               MD5.Update (C, Value.Text);
            end loop;
         end Add;

         procedure Add (Attribute_Name : Q_Attribute_Id) is
            Attr : constant Project.Attribute.Object :=
                     Data.Attrs.Element (Attribute_Name.Attr);
         begin
            if Attr.Is_Defined then
               Add (Attr);
            end if;
         end Add;

      begin
         --  The signature to detect the source change is based on the
         --  attributes which are used to compute the actual source set.

         Add (PRA.Languages);
         Add (PRA.Source_Dirs);
         Add (PRA.Source_Files);
         Add (PRA.Excluded_Source_Files);
         Add (PRA.Excluded_Source_List_File);
         Add (PRA.Source_List_File);

         --  Handle also the naming definitions

         if Data.Packs.Contains (PRP.Naming) then
            Handle_Naming : declare
               Attr   : Attribute.Object;
            begin
               if View.Check_Attribute
                 (PRA.Naming.Dot_Replacement, Result => Attr)
               then
                  Add (Attr);
               end if;

               for L of View.Languages loop
                  declare
                     L_Id  : constant Language_Id :=
                               +Name_Type (L.Text);
                     Index : constant Attribute_Index.Object :=
                               Attribute_Index.Create (L_Id);
                  begin
                     if View.Check_Attribute
                       (PRA.Naming.Spec_Suffix,
                        Index,
                        Result => Attr)
                     then
                        Add (Attr);
                     end if;

                     if View.Check_Attribute
                       (PRA.Naming.Body_Suffix,
                        Index,
                        Result => Attr)
                     then
                        Add (Attr);
                     end if;

                     if L_Id = Ada_Language then
                        if View.Check_Attribute
                          (PRA.Naming.Separate_Suffix, Result => Attr)
                        then
                           Add (Attr);
                        end if;
                     end if;
                  end;
               end loop;

               for A of View.Attributes (PRA.Naming.Spec,
                                         With_Defaults => False,
                                         With_Config   => False)
               loop
                  Add (A);
               end loop;

               for A of View.Attributes (PRA.Naming.Body_N,
                                         With_Defaults => False,
                                         With_Config   => False)
               loop
                  Add (A);
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

   Current_Signature : MD5.Binary_Message_Digest;

begin
   --  Check if up-to-date using signature for source_dirs, source_files...
   --  An abstract or aggregate project has no sources.

   if Def.Kind in K_Abstract | K_Configuration | K_Aggregate then
      return;
   end if;

   Current_Signature := Signature;

   if Def.Sources_Signature = Current_Signature then
      return;
   end if;

   Fill_Naming_Schema;

   --  Setup the naming exceptions look-up table if needed

   Fill_Ada_Naming_Exceptions (PRA.Naming.Spec);
   Fill_Ada_Naming_Exceptions (PRA.Naming.Body_N);

   Fill_Other_Naming_Exceptions
     (View.Attributes (PRA.Naming.Specification_Exceptions));
   Fill_Other_Naming_Exceptions
     (View.Attributes (PRA.Naming.Implementation_Exceptions));

   --  Read sources and set up the corresponding definition

   --  First reset the current set

   Def.Sources.Clear;
   Def.Sources_Map.Clear;
   Def.Units_Map.Clear;

   --  Clear the units record, note that we also want to record the
   --  unit_name -> view lookup table in the tree.

   for U of Def.Units loop
      Def.Tree.Clear_View (Unit => U);
   end loop;

   Def.Units.Clear;

   --  If we have attribute Excluded_Source_List_File

   if View.Has_Attribute (PRA.Excluded_Source_List_File) then
      Read_Source_List (PRA.Excluded_Source_List_File, Excluded_Sources);
   end if;

   --  If we have attribute Excluded_Source_Files

   if View.Has_Attribute (PRA.Excluded_Source_Files) then
      for File of View.Attribute (PRA.Excluded_Source_Files).Values loop
         Include_Simple_Filename (Excluded_Sources, File.Text, File);
      end loop;
   end if;

   --  Remove naming exception sources from inactive case alternatives

   for File of Def.Trees.Project.Skip_Sources loop
      Include_Simple_Filename (Excluded_Sources, File.Text, File);
   end loop;

   --  If we have attribute Source_List_File

   if View.Has_Attribute (PRA.Source_List_File) then
      Read_Source_List (PRA.Source_List_File, Included_Sources);
      Has_Source_List := True;
   end if;

   --  If we have attribute Source_Files

   if View.Has_Attribute (PRA.Source_Files) then
      for File of View.Attribute (PRA.Source_Files).Values loop
         Include_Simple_Filename (Included_Sources, File.Text, File);
      end loop;

      Has_Source_List := True;
   end if;

   if Def.Kind = K_Aggregate_Library then
      --  Sources for an aggregate library is the cumulative set of
      --  sources of the aggregated projects.

      for Agg of Def.Aggregated loop
         declare
            DA           : constant Ref := Get_RW (Agg);
            A_Set        : Project.Source.Set.Object;
         begin
            Update_Sources
              (DA.all, Agg, Stop_On_Error => True,
               Backends => Source_Info.No_Backends);

            for P of Agg.Sources loop
               --  An aggregate library project does not allow naming
               --  exception. So the source naming exception status is
               --  the one from the aggregated project.

               A_Set.Insert
                 (Project.Source.Create
                    (Source           => GPR2.Source.Object (P),
                     View             => View,
                     Naming_Exception => P.Naming_Exception,
                     Is_Compilable    => P.Is_Compilable,
                     Aggregated       => P.View));
            end loop;

            Insert
              (A_Set,
               Aggregated_Copy,
               (if Agg.Has_Attribute (PRA.Source_Dirs)
                then Agg.Attribute (PRA.Source_Dirs)
                else Source_Reference.Create
                  (DA.Trees.Project.Path_Name.Value, 0, 0)));
         end;
      end loop;

   else
      --  Handle Source_Dirs

      Definition.Source_Directories_Walk
        (View, Source_CB => Handle_File'Access, Dir_CB => null);

      for C in Def.Sources.Iterate loop
         Def.Sources_Map_Insert (Project.Source.Set.Element (C), C);
      end loop;

      if Has_Source_List then
         --  Check that we've found all the listed sources
         for S of Included_Sources loop
            if not Excluded_Sources.Contains (S)
              and then not Def.Sources_Map.Contains (S)
            then
               Tree.Append_Message
                 (Message.Create
                    (Message.Error,
                     "source file """ & String (S) & """ not found",
                     (if View.Has_Attribute (PRA.Source_List_File)
                      then View.Attribute (PRA.Source_List_File)
                      else View.Attribute (PRA.Source_Files))));
            end if;
         end loop;
      end if;

      if View.Has_Package (PRP.Naming,
                           Check_Extended => False,
                           With_Defaults  => False,
                           With_Config    => False)
      then
         --  Check all naming exceptions is used only in the original
         --  project where Naming package is declared. If nameing package is
         --  inherited then not all sources from naming exceptions have to
         --  be defined, some of then inherited from extended project.

         for C in Ada_Except_Usage.Iterate loop
            declare
               package NEU renames Naming_Exceptions_Usage;

               Key  : constant Value_Type := NEU.Key (C);
               Item : constant NEU.Constant_Reference_Type :=
                        Ada_Except_Usage (C);
            begin
               pragma Assert
                 (Key (Key'Last) in 'B' | 'S', "unexpected key: " & Key);

               Tree.Append_Message
                 (Message.Create
                    ((if View.Has_Attribute (PRA.Source_Files)
                         or else View.Has_Attribute (PRA.Source_List_File)
                      then Message.Warning
                      else Message.Error),
                     "source file """ & Item.Text
                     & """ for unit """ & Key (Key'First .. Key'Last - 1)
                     --  Last character in Key is 'B' - Body or 'S' - Spec
                     & """ not found",
                     Item));
            end;
         end loop;
      end if;

      for V of Other_Except_Usage loop
         Tree.Append_Message
           (Message.Create
              (Message.Warning,
               "source file """ & V.Text & """ not found",
               V));
      end loop;
   end if;

   --  Finally get the sources from the extended project if defined. We
   --  only add the sources not already defined in the currebnt set.

   if not Def.Extended.Is_Empty then
      for Ext of Def.Extended loop
         Update_Sources
           (Get (Ext).all, Ext, Stop_On_Error => True,
            Backends => Source_Info.No_Backends);

         Insert (Ext.Sources, Extended_Copy, SR.Undefined);
      end loop;
   end if;

   if View.Has_Attribute (PRA.Languages)
     and then Def.Kind not in K_Abstract | K_Configuration
     and then not View.Attribute (PRA.Source_Dirs).Values.Is_Empty
     and then Excluded_Sources.Is_Empty
   then
      declare
         SF : constant Attribute.Object :=
                View.Attribute (PRA.Source_Files);
      begin
         if not SF.Is_Defined or else not SF.Values.Is_Empty then
            for L of View.Languages loop
               if not Has_Src_In_Lang.Contains (+Name_Type (L.Text)) then
                  Tree.Append_Message
                    (Message.Create
                       (Message.Warning,
                        "there are no sources of language """ & L.Text
                        & """ in this project",
                        L));
               end if;
            end loop;
         end if;
      end;
   end if;

   --  Check that all source interfaces have been found in the
   --  project view.

   if not Def.Interface_Sources.Is_Empty then
      for C in Def.Interface_Sources.Iterate loop
         declare
            Name : constant Filename_Type := Source_Path_To_Sloc.Key (C);
         begin
            if not Def.Sources_Map.Contains (Name) then
               Def.Tree.Append_Message
                 (Message.Create
                    (Message.Error,
                     "interface source '" & String (Name)
                     & "' not found",
                     Source_Path_To_Sloc.Element (C)));
            end if;
         end;
      end loop;
   end if;

   --  Record back new definition for the view with updated sources

   Def.Sources_Signature := Current_Signature;

   if Stop_On_Error
     and then Message_Count < Tree.Log_Messages.Count
     and then Tree.Log_Messages.Has_Error
   then
      --  Some error messages have been logged, raise an exception
      raise Project_Error with "cannot retrieve the sources";
   end if;
end Update_Sources_List;
