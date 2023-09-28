--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Doubly_Linked_Lists;

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

   package Lang_Boolean_Map is new Ada.Containers.Hashed_Maps
     (Language_Id, Boolean, Hash, "=");

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

   function Compute_Unit_From_Filename
     (File     : Filename_Type;
      Kind     : Valid_Unit_Kind;
      NS       : Naming_Schema;
      Dot_Repl : String;
      Messages : in out GPR2.Log.Object;
      Last_Dot : out Natural;
      Success  : out Boolean) return Name_Type;
   --  For an Ada source and given its kind, try to compute a valid unit
   --  name. Success takes True if such a valid name is found.
   --  Set Last_Dot to last dot index in result to split separate unit
   --  name.

   procedure Krunch
     (Buffer        : in out String;
      Len           : in out Natural;
      Maxlen        : Natural;
      No_Predef     : Boolean);
   --  From gnat tools: krunch a unit name to achieve a short-variant for the
   --  source filename

   function Krunch (Unit_Name : Name_Type) return Filename_Type;
   --  convenient wrapper around the above procedure

   function Compute_Unit_From_Filename
     (File     : Filename_Type;
      Kind     : Valid_Unit_Kind;
      NS       : Naming_Schema;
      Dot_Repl : String;
      Messages : in out GPR2.Log.Object;
      Last_Dot : out Natural;
      Success  : out Boolean) return Name_Type
   is
      use Ada.Strings;
      use Ada.Strings.Maps;

      Result     : Unbounded_String := +GPR2.Path_Name.Simple_Name (File);
      Default_NS : constant  Boolean :=
                     NS.Spec_Suffix = ".ads" and then NS.Body_Suffix = ".adb"
                      and then NS.Sep_Suffix = ".adb" and then Dot_Repl = "-";
      --  True if the current naming scheme is GNAT's default naming scheme.
      --  This is to take into account shortened names like "Ada." (a-),
      --  "System." (s-) and so on.
   begin
      --  Check binder-generated source, and ignore it

      if Length (Result) > 3
        and then Slice (Result, 1, 3) = "b__"
      then
         goto Invalid;
      end if;

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
                  SR.Create (Full_Name (File), 1, 1)));
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

   ------------
   -- Krunch --
   ------------

   procedure Krunch
     (Buffer        : in out String;
      Len           : in out Natural;
      Maxlen        : Natural;
      No_Predef     : Boolean)
   is
      pragma Assert (Buffer'First = 1);
      --  This is a documented requirement; the assert turns off index warnings

      B1       : Character renames Buffer (1);
      Curlen   : Natural;
      Krlen    : Natural;
      Num_Seps : Natural;
      Startloc : Natural;
      J        : Natural;

   begin
      --  Deal with special predefined children cases. Startloc is the first
      --  location for the krunch, set to 1, except for the predefined children
      --  case, where it is set to 3, to start after the standard prefix.

      if No_Predef then
         Startloc := 1;
         Curlen := Len;
         Krlen := Maxlen;

      elsif Len >= 18
        and then Buffer (1 .. 17) = "ada-wide_text_io-"
      then
         Startloc := 3;
         Buffer (2 .. 5) := "-wt-";
         Buffer (6 .. Len - 12) := Buffer (18 .. Len);
         Curlen := Len - 12;
         Krlen  := 8;

      elsif Len >= 23
        and then Buffer (1 .. 22) = "ada-wide_wide_text_io-"
      then
         Startloc := 3;
         Buffer (2 .. 5) := "-zt-";
         Buffer (6 .. Len - 17) := Buffer (23 .. Len);
         Curlen := Len - 17;
         Krlen := 8;

      elsif Len >= 27
        and then Buffer (1 .. 27) = "ada-long_long_long_integer_"
      then
         Startloc := 3;
         Buffer (2 .. Len - 2) := Buffer (4 .. Len);
         Buffer (18 .. Len - 10) := Buffer (26 .. Len - 2);
         Curlen := Len - 10;
         Krlen := 8;

      elsif Len >= 4 and then Buffer (1 .. 4) = "ada-" then
         Startloc := 3;
         Buffer (2 .. Len - 2) := Buffer (4 .. Len);
         Curlen := Len - 2;
         Krlen  := 8;

      elsif Len >= 5 and then Buffer (1 .. 5) = "gnat-" then
         Startloc := 3;
         Buffer (2 .. Len - 3) := Buffer (5 .. Len);
         Curlen := Len - 3;
         Krlen  := 8;

      elsif Len >= 7 and then Buffer (1 .. 7) = "system-" then
         Startloc := 3;
         Buffer (2 .. Len - 5) := Buffer (7 .. Len);
         Curlen := Len - 5;
         if (Curlen >= 3 and then Buffer (Curlen - 2 .. Curlen) = "128")
           or else (Len >= 9 and then
                      (Buffer (3 .. 9) = "exn_lll"
                       or else Buffer (3 .. 9) = "exp_lll"
                       or else Buffer (3 .. 9) = "img_lll"
                       or else Buffer (3 .. 9) = "val_lll"
                       or else Buffer (3 .. 9) = "wid_lll"))
           or else (Curlen = 10 and then Buffer (3 .. 6) = "pack")
         then
            if Len >= 15 and then Buffer (3 .. 15) = "compare_array" then
               Buffer (3 .. 4) := "ca";
               Buffer (5 .. Curlen - 11) := Buffer (16 .. Curlen);
               Curlen := Curlen - 11;
            end if;
            Krlen := 9;
         else
            Krlen := 8;
         end if;

      elsif Len >= 11 and then Buffer (1 .. 11) = "interfaces-" then
         Startloc := 3;
         Buffer (2 .. Len - 9) := Buffer (11 .. Len);
         Curlen := Len - 9;

         --  Only fully krunch historical units. For new units, simply use
         --  the 'i-' prefix instead of 'interfaces-'. Packages Interfaces.C
         --  and Interfaces.Cobol are already in the right form. Package
         --  Interfaces.Definitions is krunched for backward compatibility.

         if        (Curlen >  3 and then Buffer (3 ..  4) = "c-")
           or else (Curlen >  3 and then Buffer (3 ..  4) = "c_")
           or else (Curlen = 13 and then Buffer (3 .. 13) = "definitions")
           or else (Curlen =  9 and then Buffer (3 ..  9) = "fortran")
           or else (Curlen = 16 and then Buffer (3 .. 16) = "packed_decimal")
           or else (Curlen >  8 and then Buffer (3 ..  9) = "vxworks")
           or else (Curlen >  5 and then Buffer (3 ..  6) = "java")
         then
            Krlen := 8;
         else
            Krlen := Maxlen;
         end if;

         --  For the renamings in the obsolescent section, we also force
         --  krunching to 8 characters, but no other special processing is
         --  required here. Note that text_io and calendar are already short
         --  enough anyway.

      elsif     (Len =  9 and then Buffer (1 ..  9) = "direct_io")
        or else (Len = 10 and then Buffer (1 .. 10) = "interfaces")
        or else (Len = 13 and then Buffer (1 .. 13) = "io_exceptions")
        or else (Len = 12 and then Buffer (1 .. 12) = "machine_code")
        or else (Len = 13 and then Buffer (1 .. 13) = "sequential_io")
        or else (Len = 20 and then Buffer (1 .. 20) = "unchecked_conversion")
        or else (Len = 22 and then Buffer (1 .. 22) = "unchecked_deallocation")
      then
         Startloc := 1;
         Krlen    := 8;
         Curlen   := Len;

         --  Special case of a child unit whose parent unit is a single letter
         --  that is A, G, I, or S. In order to prevent confusion with krunched
         --  names of predefined units use a tilde rather than a minus as the
         --  second character of the file name.

      elsif Len > 1
        and then Buffer (2) = '-'
        and then (B1 = 'a' or else B1 = 'g' or else B1 = 'i' or else B1 = 's')
        and then Len <= Maxlen
      then
         Buffer (2) := '~';
         return;

         --  Normal case, not a predefined file

      else
         Startloc := 1;
         Curlen   := Len;
         Krlen    := Maxlen;
      end if;

      --  Immediate return if file name is short enough now

      if Curlen <= Krlen then
         Len := Curlen;
         return;
      end if;

      --  If string contains Wide_Wide, replace by a single z

      J := Startloc;
      while J <= Curlen - 8 loop
         if Buffer (J .. J + 8) = "wide_wide"
           and then (J = Startloc
                     or else Buffer (J - 1) = '-'
                     or else Buffer (J - 1) = '_')
           and then (J + 8 = Curlen
                     or else Buffer (J + 9) = '-'
                     or else Buffer (J + 9) = '_')
         then
            Buffer (J) := 'z';
            Buffer (J + 1 .. Curlen - 8) := Buffer (J + 9 .. Curlen);
            Curlen := Curlen - 8;
         end if;

         J := J + 1;
      end loop;

      --  For now, refuse to krunch a name that contains an ESC character (wide
      --  character sequence) since it's too much trouble to do this right ???

      for J in 1 .. Curlen loop
         if Buffer (J) = ASCII.ESC then
            return;
         end if;
      end loop;

      --  Count number of separators (minus signs and underscores) and for now
      --  replace them by spaces. We keep them around till the end to control
      --  the krunching process, and then we eliminate them as the last step

      Num_Seps := 0;
      for J in Startloc .. Curlen loop
         if Buffer (J) = '-' or else Buffer (J) = '_' then
            Buffer (J) := ' ';
            Num_Seps := Num_Seps + 1;
         end if;
      end loop;

      --  Now we do the one character at a time krunch till we are short enough

      while Curlen - Num_Seps > Krlen loop
         declare
            Long_Length : Natural := 0;
            Long_Last   : Natural := 0;
            Piece_Start : Natural;
            Ptr         : Natural;

         begin
            Ptr := Startloc;

            --  Loop through pieces to find longest piece

            while Ptr <= Curlen loop
               Piece_Start := Ptr;

               --  Loop through characters in one piece of name

               while Ptr <= Curlen and then Buffer (Ptr) /= ' ' loop
                  Ptr := Ptr + 1;
               end loop;

               if Ptr - Piece_Start > Long_Length then
                  Long_Length := Ptr - Piece_Start;
                  Long_Last := Ptr - 1;
               end if;

               Ptr := Ptr + 1;
            end loop;

            --  Remove last character of longest piece

            if Long_Last < Curlen then
               Buffer (Long_Last .. Curlen - 1) :=
                 Buffer (Long_Last + 1 .. Curlen);
            end if;

            Curlen := Curlen - 1;
         end;
      end loop;

      --  Final step, remove the spaces

      Len := 0;

      for J in 1 .. Curlen loop
         if Buffer (J) /= ' ' then
            Len := Len + 1;
            Buffer (Len) := Buffer (J);
         end if;
      end loop;

      return;
   end Krunch;

   function Krunch (Unit_Name : Name_Type) return Filename_Type is
      Buffer : String := Ada.Characters.Handling.To_Lower (String (Unit_Name));
      Len    : Natural := Buffer'Last;
   begin
      for J in Buffer'Range loop
         if Buffer (J) = '.' then
            Buffer (J) := '-';
         end if;
      end loop;

      Krunch (Buffer, Len, 8, False);

      return Filename_Type (Buffer (1 .. Len));
   end Krunch;

   -------------
   -- Process --
   -------------

   procedure Process
     (Data          : View_Data_Ref;
      Stop_On_Error : Boolean;
      Messages      : in out GPR2.Log.Object)
   is
      function Is_Compilable (Language : Language_Id) return Boolean;
      --  Check whether the language is compilable on the current View. This
      --  includes information provided by the Tree (Driver attribute). Note
      --  that this routine caches the result into a map.

      procedure Handle_File
        (Dir_Ref   : SR.Value.Object;
         File      : GPR2.Path_Name.Full_Name;
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

      Dot_Repl : constant String :=
                   Data.View.Attribute (PRA.Naming.Dot_Replacement).Value.Text;
      --  Get Dot_Replacement value

      Source_Name_Set    : GPR2.Containers.Filename_Set;
      --  Collection of source simple names for a given Source_Dirs value

      Naming_Schema_Map       : Naming_Schema_Maps.Map;

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
         File      : GPR2.Path_Name.Full_Name;
         Timestamp : Ada.Calendar.Time)
      is
      begin
         Data.Src_Files.Include ((File'Length, Timestamp, Dir_Ref, File));
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

         Basename         : constant Filename_Type :=
                                GPR2.Path_Name.Simple_Name (File.Path);

         type Source_Detection is
           (Naming_Exception,
            Naming_Convention,
            No_Match);
         Match            : Source_Detection := No_Match;
         Units            : Source.Unit_List;  --  For Ada
         Kind             : Valid_Unit_Kind;
         Index            : Unit_Index;
         Source           : Build.Source.Object;
         Attr             : Project.Attribute.Object;
         Ada_Exc_CS       : Source_Path_To_Attribute_List.Cursor;
         Parsed           : Boolean;
         Exc_Attr         : Source_Reference.Object;

      begin
         --  Stop here if it's one of the excluded sources, or it's not in the
         --  included sources if those are given explicitely.

         if Data.Excluded_Sources.Contains (Basename)
           or else (not Data.Listed_Sources.Is_Empty
                    and then not Data.Listed_Sources.Contains (Basename))
         then
            return False;
         end if;

         if File.Dir_Ref /= Current_Src_Dir_SR then
            Current_Src_Dir_SR := File.Dir_Ref;
            Source_Name_Set.Clear;
         end if;

         for Language of Data.View.Language_Ids loop
            --  First, try naming exceptions

            Match := No_Match;
            Kind  := S_Spec;  --  Dummy value

            if Language = Ada_Language
              and then not Ada_Naming_Exceptions.Is_Empty
            then
               Ada_Exc_CS := Ada_Naming_Exceptions.Find (Basename);

               if Source_Path_To_Attribute_List.Has_Element (Ada_Exc_CS) then
                  for Exc of Ada_Naming_Exceptions (Ada_Exc_CS) loop
                     --  Found at least one: so we have naming exceptions for
                     --  this source

                     Match := Naming_Exception;
                     Exc_Attr := Source_Reference.Object (Exc);

                     if Exc.Value.Has_At_Pos then
                        Index := Exc.Value.At_Pos;
                        pragma Assert (Index /= No_Index);
                     else
                        Index := No_Index;
                     end if;

                     Kind := (if Exc.Name.Id = PRA.Naming.Spec
                              then S_Spec
                              else S_Body);

                     Units.Insert
                       (Build.Source.Create
                          (Unit_Name      => Name_Type (Exc.Index.Text),
                           Index          => Index,
                           Kind           => Kind));
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
                  Match := Naming_Exception;
                  Kind  := S_Spec;

               elsif Data.View.Check_Attribute
                       (PRA.Naming.Implementation_Exceptions,
                        Project.Attribute_Index.Create (Language),
                        Result => Attr)
                 and then Attr.Has_Value (Value_Type (Basename))
               then
                  Match := Naming_Exception;
                  Kind  := S_Body;
               end if;
            end if;

            if Match = No_Match then
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
                        Matches_Separate := False;
                     else
                        Matches_Body := False;
                     end if;
                  end if;

                  Match := (if Matches_Spec
                              or else Matches_Body
                              or else Matches_Separate
                            then Naming_Convention
                            else No_Match);

                  if Matches_Spec then
                     Kind  := S_Spec;

                  elsif Matches_Body then
                     Kind  := S_Body;

                  elsif Matches_Separate then
                     Kind  := S_Separate;
                  end if;
               end;
            end if;

            --  If we have a match from either naming exception or scheme
            --  we create the Source object.

            if Match /= No_Match then
               Source := Build.Source.Create
                 (Path_Name.Create_File (File.Path),
                  Language         => Language,
                  Kind             => Kind,
                  Timestamp        => File.Stamp,
                  Tree_Db          => Data.Tree_Db,
                  Naming_Exception => Match = Naming_Exception,
                  Source_Ref       => File.Dir_Ref,
                  Is_Compilable    => Is_Compilable (Language));

               --  If we know the units in the source (from naming exception),
               --  then add them now.

               for U of Units loop
                  Source.Update_Unit (U);
               end loop;
            end if;

            --  Check Unit names and kind for Ada:

            if Match /= No_Match
              and then not Units.Is_Indexed_List
              and then Language = Ada_Language
            then
               --  There can be ambiguities in Ada with the default naming
               --  convention or with naming exception with regards to
               --  separate and child bodies. In order to disambiguate that
               --  we need to parse the source.

               --  Parse the source to get unit and update its kind if
               --  needed.

               Build.Source.Ada_Parser.Compute
                 (Tree             => Tree,
                  Data             => Source,
                  Get_Withed_Units => False,
                  Success          => Parsed);

               if Parsed and then Match = Naming_Exception then
                  --  Check parsed unit name is the same as the one declared
                  --  in the gpr project.

                  if Source.Unit.Kind /= S_No_Body
                    and then Build.Source.Full_Name (Source.Unit) /=
                      Build.Source.Full_Name (Units.Element (No_Index))
                  then
                     Messages.Append
                       (Message.Create
                          (Message.Warning,
                           "actual unit name """ &
                             String (Build.Source.Full_Name (Source.Unit)) &
                             """ differs from the one declared in the " &
                             "project : """ &
                             String (Build.Source.Full_Name
                                       (Units.Element (No_Index))) & '"',
                           Exc_Attr));
                  end if;

               elsif Match = Naming_Convention then
                  declare
                     use type GPR2.View_Ids.View_Id;
                     Last_Dot  : Natural;
                     Success   : Boolean;
                     Unit_Name : constant Name_Type :=
                                   Compute_Unit_From_Filename
                                     (File     => File.Path,
                                      Kind     => Kind,
                                      NS       =>
                                        Naming_Schema_Map (Ada_Language),
                                      Dot_Repl => Dot_Repl,
                                      Messages => Messages,
                                      Last_Dot => Last_Dot,
                                      Success  => Success);

                  begin
                     if not Success then
                        return False;
                     end if;

                     if Parsed then
                        --  Check unit name from convention is the same as
                        --  the parsed one.

                        if Data.View.Id /= GPR2.View_Ids.Runtime_View_Id
                          and then Source.Kind /= S_No_Body
                          and then Unit_Name /=
                            Build.Source.Full_Name (Source.Unit)
                          and then Path_Name.Base_Name (File.Path) /=
                            Krunch (Build.Source.Full_Name (Source.Unit))
                        then
                           Messages.Append
                             (Message.Create
                                (Message.Warning,
                                 "unit name """ & String (Source.Unit.Name) &
                                   """ does not match source name",
                                 SR.Create (File.Path, 0, 0)));
                        end if;

                     elsif Kind = S_Separate then
                        --  Separates must have a doted unit name

                        if Last_Dot > 0 then
                           Source.Update_Unit
                             (Build.Source.Create
                                (Unit_Name     => Unit_Name
                                     (Unit_Name'First .. Last_Dot - 1),
                                 Index         => No_Index,
                                 Kind          => Kind,
                                 Separate_Name => Unit_Name
                                   (Last_Dot + 1 .. Unit_Name'Last),
                                 Parsed        => False));
                        else
                           Messages.Append
                             (Message.Create
                                (Message.Warning,
                                 "invalid file name: no '.' in deduced " &
                                   "separate unit name """ &
                                   String (Unit_Name) & '"',
                                 SR.Create (File.Path, 0, 0)));

                           return False;
                        end if;

                     else
                        Source.Update_Unit
                          (Build.Source.Create
                             (Unit_Name     => Unit_Name,
                              Index         => No_Index,
                              Kind          => Kind,
                              Separate_Name => "",
                              Parsed        => False));
                     end if;
                  end;
               end if;
            end if;

            --  Now check for conflicts between naming exceptions and naming
            --  convention.

            if Match = Naming_Convention and then Language = Ada_Language then
               declare
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
                       (Filename_Optional (Source.Unit.Name));

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
                  --  Check if we have conflicting naming exceptions:
                  --  same (unit,kind) but different source.
                  --  In this case we skip this source.

                  if (Source.Kind = S_Spec
                      and then Has_Conflict_NE (PRA.Naming.Spec))
                    or else
                      (Source.Kind = S_Body
                       and then Has_Conflict_NE (PRA.Naming.Body_N))
                  then
                     return False;
                  end if;
               end;
            end if;

            if Match /= No_Match then
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
                    (Data, Data.View, F.Path,
                     Project.View.Undefined,
                     Messages => Messages);

                  Changed_Sources.Include
                    (Path_Name.Simple_Name (F.Path));
               end if;
            end;
         end if;
      end loop;

      --  Check new or updated sources

      for F of Data.Src_Files loop
         declare
            use type Ada.Calendar.Time;
            C     : File_Sets.Cursor;
            C2    : Src_Info_Maps.Cursor;

         begin
            C := Previous_Files.Find (F);

            if not File_Sets.Has_Element (C) then
               --  New potential source: process it

               if Process_File (F) then
                  --  First add source to Data view.
                  Add_Source
                    (Data, Data.View, F.Path,
                     Extended_View => Project.View.Undefined,
                     Messages      => Messages);
                  Changed_Sources.Include
                    (Path_Name.Simple_Name (F.Path));
               end if;

            else
               C2 := Data.Src_Infos.Find (F.Path);

               if Src_Info_Maps.Has_Element (C2) then
                  declare
                     Src_Ref : constant Src_Info_Maps.Reference_Type :=
                                 Data.Src_Infos.Reference (C2);
                     Success : Boolean;
                  begin
                     if Src_Ref.Modification_Time /= F.Stamp then
                        Src_Ref.Update_Modification_Time (F.Stamp);

                        if not Src_Ref.Has_Naming_Exception then
                           Source.Ada_Parser.Compute
                             (Tree, Src_Ref, False, Success);
                        end if;
                     end if;
                  end;
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
            Resolve_Visibility (Data, C_Overload, Messages);
         end;
      end loop;

      --  Data.Sources is now updated, we don't need Previous_Files anymore
      Previous_Files.Clear;

      --  Check that we've found all the listed sources
      for S of Data.Listed_Sources loop
         if not Data.Excluded_Sources.Contains (S)
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

end Update_Sources_List;
