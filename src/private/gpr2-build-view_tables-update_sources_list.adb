--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.Utils;

with GPR2.Build.Unit_Info.List;
with GPR2.Build.Source_Base.Ada_Parser;
with GPR2.Containers;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.View;
with GPR2.Source_Reference.Value;
with GPR2.Tree_Internal;
with GPR2.View_Internal;

separate (GPR2.Build.View_Tables)
package body Update_Sources_List is

   use type Project.Attribute.Object;
   use type Project.View.Object;
   package PAI renames GPR2.Project.Attribute_Index;

   use GPR2.Path_Name;

   package PRA renames GPR2.Project.Registry.Attribute;
   package PRP renames GPR2.Project.Registry.Pack;
   package SR  renames GPR2.Source_Reference;

   package Source_Set renames GPR2.Containers.Filename_Type_Set;

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

   type Source_Detection is
     (Naming_Exception, Naming_Convention, No_Match);

   type Source_Context (Dot_Repl_Length : Natural) is limited record
      Dot_Repl              : String (1 .. Dot_Repl_Length);
      --  Filename sequence representing a dot in an Ada unit name
      Tree                  : not null access Tree_Internal.Object;
      --  Access to the file reader and source subdirectory settings
      Listed_Sources        : Source_Set.Set;
      --  Basenames selected by Source_Files and Source_List_File
      Excluded_Sources      : Containers.Source_Path_To_Sloc.Map;
      --  Excluded basenames and their locations for diagnostics
      Has_Source_List       : Boolean := False;
      --  An explicit source list is defined, even if it is empty
      Naming_Schema_Map     : Naming_Schema_Maps.Map;
      --  Filename suffixes for each language
      Ada_Naming_Exceptions : Source_Path_To_Attribute_List.Map;
      --  Ada basenames mapped to their Naming.Spec and Naming.Body attributes
   end record;
   --  Selection and naming information owned by one Process call

   package Message_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Filename_Type, GPR2.Log.Object, "<", GPR2.Log."=");

   procedure Add_Source (Data : View_Data_Ref; Src : Source_Proxy);
   --  Register a newly discovered or inherited source

   procedure Check_Naming_Exceptions
     (Data     : View_Data_Ref;
      Context  : Source_Context;
      File     : Full_Name;
      Language : Language_Id;
      Match    : out Source_Detection;
      Kind     : out Valid_Unit_Kind;
      Units    : in out Unit_Info.List.Object;
      Exc_Attr : out Source_Reference.Object;
      Messages : in out GPR2.Log.Object);
   --  Identify sources and declared units from naming exceptions

   procedure Check_Naming_Scheme
     (Context  : Source_Context;
      Basename : Filename_Type;
      Language : Language_Id;
      Match    : out Source_Detection;
      Kind     : out Valid_Unit_Kind);
   --  Match the most specific language suffix

   procedure Include_Simple_Filename
     (Set      : in out Source_Set.Set;
      Value    : Value_Type;
      Sloc     : Source_Reference.Object'Class;
      Messages : in out GPR2.Log.Object);

   procedure Initialize_Source_Lists
     (Data     : View_Data_Ref;
      Context  : in out Source_Context;
      Messages : in out GPR2.Log.Object);
   --  Read source selection attributes before collecting sources

   procedure Insert_Inherited
     (Data    : View_Data_Ref;
      Src     : Source_Proxy;
      Clashes : in out Basename_Source_Maps.Map);
   --  Own sources take precedence over inherited sources

   function Process_File
     (Data           : View_Data_Ref;
      Context        : Source_Context;
      File           : Full_Name;
      Matching_Units : Boolean;
      Messages       : in out GPR2.Log.Object) return Boolean;

   procedure Read_Source_List
     (View      : Project.View.Object;
      Attr      : Project.Attribute.Object;
      Set       : in out Source_Set.Set;
      Messages  : in out GPR2.Log.Object);
   --  Read filenames from the file designated by Attr

   procedure Register_Units
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object);
   --  Register the view's visible units in their namespaces

   procedure Report_Source
     (Src : Source_Proxy; Messages : in out GPR2.Log.Object);
   --  Report a source involved in an inheritance conflict

   procedure Fill_Ada_Naming_Exceptions
     (View         : GPR2.Project.View.Object;
      Attr         : Attribute_Id;
      Src_Map      : in out Source_Path_To_Attribute_List.Map)
     with Pre => Attr in  PRA.Naming.Spec.Attr | PRA.Naming.Body_N.Attr;
   --  Map source filenames to their Ada naming exceptions

   procedure Fill_Naming_Schema
     (View : Project.View.Object;
      Map  : in out Naming_Schema_Maps.Map);
   --  Cache naming suffixes for each language

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

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source (Data : View_Data_Ref; Src : Source_Proxy) is
      Owner_Db : constant View_Data_Ref :=
                   Get_Data (Data.Tree_Db, Src.View);
      Src_Info : constant Src_Info_Maps.Constant_Reference_Type :=
                   Owner_Db.Src_Infos.Constant_Reference (Src.Path_Name);
      C_Lang   : Sources_By_Langs_Maps.Cursor;
      Inserted : Boolean;
   begin
      Data.Sources.Insert (Src.Path_Name, Src);
      Data.Langs_Usage.Insert (Src_Info.Language, 1, C_Lang, Inserted);
      if not Inserted then
         declare
            Count : constant Sources_By_Langs_Maps.Reference_Type :=
                      Data.Langs_Usage.Reference (C_Lang);
         begin
            Count.Element.all := Count + 1;
         end;
      end if;
   end Add_Source;

   -----------------------------
   -- Check_Naming_Exceptions --
   -----------------------------

   procedure Check_Naming_Exceptions
     (Data     : View_Data_Ref;
      Context  : Source_Context;
      File     : Full_Name;
      Language : Language_Id;
      Match    : out Source_Detection;
      Kind     : out Valid_Unit_Kind;
      Units    : in out Unit_Info.List.Object;
      Exc_Attr : out Source_Reference.Object;
      Messages : in out GPR2.Log.Object)
   is
      Basename : constant Filename_Type := Path_Name.Simple_Name (File);
      Attr     : Project.Attribute.Object;
      Index    : Unit_Index;
      Ada_Exc  : Source_Path_To_Attribute_List.Cursor;
   begin
      Exc_Attr := Source_Reference.Undefined;
      Match := No_Match;
      Kind  := S_Spec;  --  Dummy value

      if Language = Ada_Language
        and then not Context.Ada_Naming_Exceptions.Is_Empty
      then
         Ada_Exc := Context.Ada_Naming_Exceptions.Find (Basename);

         if Source_Path_To_Attribute_List.Has_Element (Ada_Exc) then
            for Exc of Context.Ada_Naming_Exceptions (Ada_Exc) loop
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

               declare
                  Unit : constant GPR2.Build.Unit_Info.Object :=
                    (Unit_Info.Create
                       (Unit_Name => Name_Type (Exc.Index.Text),
                        Index     => Index,
                        Kind      => Kind));
               begin
                  if Units.Contains (Unit.Index) then
                     Messages.Append
                       (Message.Create
                          (Message.Critical_Error,
                           "file "
                           & String (File)
                           & " contains duplicated units """
                           & String (Unit.Name)
                           & " and """
                           & String (Units.Element (Unit.Index).Name)
                           & """ at index"
                           & Unit.Index'Image
                           & "",
                           SR.Create (Data.View.Path_Name.Value, 0, 0)));
                  else
                     Units.Insert (Unit);
                  end if;
               end;

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
   end Check_Naming_Exceptions;

   -------------------------
   -- Check_Naming_Scheme --
   -------------------------

   procedure Check_Naming_Scheme
     (Context  : Source_Context;
      Basename : Filename_Type;
      Language : Language_Id;
      Match    : out Source_Detection;
      Kind     : out Valid_Unit_Kind)
   is
      NS               : Naming_Schema renames
                           Context.Naming_Schema_Map.Element (Language);
      Matches_Spec     : Boolean;
      Matches_Body     : Boolean;
      Matches_Separate : Boolean;

   begin
      Kind := S_Spec;
      Matches_Spec :=
        NS.Has_Spec_Suffix
        and then GPR2.Path_Name.Ends_With
                   (Basename, NS.Spec_Suffix);

      Matches_Body :=
        NS.Has_Body_Suffix
        and then GPR2.Path_Name.Ends_With
                   (Basename, NS.Body_Suffix);

      Matches_Separate :=
        NS.Has_Separate_Suffix
        and then GPR2.Path_Name.Ends_With
                   (Basename, NS.Sep_Suffix);

      --  Use the longest matching suffix

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
   end Check_Naming_Scheme;

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
                 (Message.Hint, "invalid file name, contains dot",
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
            S3 : constant Character := Element (Result, 3);

         begin
            if S1 in 'a' | 'g' | 'i' | 's' then
               --  Children or separates of packages A, G, I or S. These
               --  names are x~... (where x is a, g, i, or s) or x__...

               if S2 = '~' then
                  Replace_Element (Result, 2, '.');
               elsif S2 = S3 and then S2 = '_' then
                  declare
                     Tmp : String := To_String (Result);
                  begin
                     Tmp (2) := '.';
                     Tmp (3 .. Tmp'Last - 1) := Tmp (4 .. Tmp'Last);
                     Result :=
                       To_Unbounded_String (Tmp (Tmp'First .. Tmp'Last - 1));
                  end;
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
     (Set      : in out Source_Set.Set;
      Value    : Value_Type;
      Sloc     : Source_Reference.Object'Class;
      Messages : in out GPR2.Log.Object)
   is
      Position : Source_Set.Cursor;
      Inserted : Boolean;
   begin
      if Has_Directory_Separator (Value) then
         Messages.Append
           (Message.Create
              (Message.Critical_Error,
               "file name cannot include directory information (""" & Value
               & """)",
               Sloc));

      elsif Value'Length = 0 then
         Messages.Append
           (Message.Create
              (Message.Critical_Error,
               "file name cannot be empty",
               Sloc));
      else
         Set.Insert (Filename_Type (Value), Position, Inserted);
      end if;
   end Include_Simple_Filename;

   -----------------------------
   -- Initialize_Source_Lists --
   -----------------------------

   procedure Initialize_Source_Lists
     (Data     : View_Data_Ref;
      Context  : in out Source_Context;
      Messages : in out GPR2.Log.Object)
   is
      Attr     : Project.Attribute.Object;
      Exc_List : Source_Set.Set;
   begin
      --  If we have attribute Excluded_Source_List_File

      Attr := Data.View.Attribute (PRA.Excluded_Source_List_File);

      if Attr.Is_Defined then
         Read_Source_List
           (Data.View, Attr, Exc_List, Messages);
         if not Messages.Has_Critical_Error then
            for Src of Exc_List loop
               Context.Excluded_Sources.Include
                 (Src, Source_Reference.Object (Attr.Value));
            end loop;
         end if;
      end if;

      --  If we have attribute Excluded_Source_Files

      Attr := Data.View.Attribute (PRA.Excluded_Source_Files);

      if Attr.Is_Defined then
         for File of Attr.Values loop
            Exc_List.Clear;
            Include_Simple_Filename
              (Exc_List, File.Text, File, Messages);
            if not Exc_List.Is_Empty then
               Context.Excluded_Sources.Include
                 (Exc_List.First_Element, Source_Reference.Object (File));
            end if;
         end loop;
      end if;

      --  Remove naming exception sources from inactive case alternatives

      for File of Data.View.Skipped_Sources loop
         Exc_List.Clear;
         Include_Simple_Filename
           (Exc_List, File.Text, File, Messages);
         if not Exc_List.Is_Empty then
            Context.Excluded_Sources.Include
              (Exc_List.First_Element, Source_Reference.Undefined);
         end if;
      end loop;

      --  If we have attribute Source_List_File

      Attr := Data.View.Attribute (PRA.Source_List_File);

      if Attr.Is_Defined then
         Context.Has_Source_List := True;
         Read_Source_List
           (Data.View, Attr, Context.Listed_Sources, Messages);
      end if;

      --  If we have attribute Source_Files

      Attr := Data.View.Attribute (PRA.Source_Files);

      if Attr.Is_Defined then
         Context.Has_Source_List := True;
         for File of Attr.Values loop
            Include_Simple_Filename
              (Context.Listed_Sources, File.Text, File, Messages);
         end loop;
      end if;
   end Initialize_Source_Lists;

   ----------------------
   -- Insert_Inherited --
   ----------------------

   procedure Insert_Inherited
     (Data    : View_Data_Ref;
      Src     : Source_Proxy;
      Clashes : in out Basename_Source_Maps.Map)
   is
      Name     : constant Simple_Name :=
                   Path_Name.Simple_Name (Src.Path_Name);
      Position : Basename_Source_Maps.Cursor;
      Inserted : Boolean;
   begin
      Data.Basenames.Insert (Name, Src, Position, Inserted);
      if not Inserted and then Data.Basenames (Position).View /= Data.View then
         Clashes.Include (Name, Src);
      end if;
   end Insert_Inherited;

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
      Buffer : String := To_Lower (Unit_Name);
      Len    : Natural := Buffer'Last;
   begin
      for J in Buffer'Range loop
         if Buffer (J) = '.' then
            Buffer (J) := '-';
         end if;
      end loop;

      --  Set Maxlen to the Buffer length, Krunching further more already
      --  detect cases where the Maxlen must be adjusted to 8.
      --  this allows to not krunch units like interfaces-smt32-pwr further
      --  more than i-smt32-pwr instead of forcing down the Maxlen to 8 giving
      --  i-smtpwr which doesn't exists.
      Krunch (Buffer, Len, Len, False);

      return Filename_Type (Buffer (1 .. Len));
   end Krunch;

   -------------
   -- Process --
   -------------

   procedure Process
     (Data           : View_Data_Ref;
      Messages       : in out GPR2.Log.Object;
      Matching_Units : Boolean := True)
   is
      Dot_Repl : constant String :=
                   Data.View.Attribute (PRA.Naming.Dot_Replacement).Value.Text;
      Context : Source_Context :=
                  (Dot_Repl_Length => Dot_Repl'Length,
                   Dot_Repl        => Dot_Repl,
                   Tree            => View_Internal.Get_RO (Data.View).Tree,
                   others          => <>);
      Attr          : Project.Attribute.Object;
      Clashes       : Basename_Source_Maps.Map;
      File_Messages : Message_Maps.Map;
      Current_Dir   : Natural := 0;
      Source_Names  : Source_Set.Set;
      --  Basenames selected from the current Source_Dirs value

      procedure Handle_File
        (Dir_Index : Natural;
         File      : GPR2.Path_Name.Full_Name;
         Timestamp : Ada.Calendar.Time);
      --  Register sources in Source_Dirs order during the directory walk

      -----------------
      -- Handle_File --
      -----------------

      procedure Handle_File
        (Dir_Index : Natural;
         File      : GPR2.Path_Name.Full_Name;
         Timestamp : Ada.Calendar.Time)
      is
         pragma Unreferenced (Timestamp);
         Messages : GPR2.Log.Object;
      begin
         if Dir_Index /= Current_Dir then
            Current_Dir := Dir_Index;
            Source_Names.Clear;
         end if;

         if Process_File (Data, Context, File, Matching_Units, Messages)
         then
            declare
               Src      : constant Source_Proxy :=
                            (File'Length, Data.View,
                             Project.View.Undefined, File);
               Name     : constant Simple_Name := Path_Name.Simple_Name (File);
               Position : Basename_Source_Maps.Cursor;
               Inserted : Boolean;
            begin
               Add_Source (Data, Src);
               Data.Basenames.Insert (Name, Src, Position, Inserted);
               if Inserted then
                  Source_Names.Include (Name);
               elsif Source_Names.Contains (Name) then
                  Clashes.Include (Name, Src);
               end if;
            end;
         end if;

         if not Messages.Is_Empty then
            File_Messages.Insert (File, Messages);
         end if;
      end Handle_File;

   begin
      Initialize_Source_Lists (Data, Context, Messages);

      --  Prepare tables used to detect actual sources and their language

      Fill_Naming_Schema (Data.View, Context.Naming_Schema_Map);

      if Data.View.Language_Ids.Contains (Ada_Language) then
         Fill_Ada_Naming_Exceptions
           (Data.View, PRA.Naming.Spec.Attr, Context.Ada_Naming_Exceptions);
         Fill_Ada_Naming_Exceptions
           (Data.View, PRA.Naming.Body_N.Attr,
            Context.Ada_Naming_Exceptions);
      end if;

      --  Lookup for all files in the source directories

      Data.View.Source_Directories_Walk
        (Source_CB => Handle_File'Access,
         Dir_CB    => null,
         Messages  => Messages);

      for Source_Messages of File_Messages loop
         for Msg of Source_Messages loop
            Messages.Append (Msg);
         end loop;
      end loop;

      if Data.View.Is_Extending then
         for Parent of Data.View.Extended loop
            declare
               Parent_Db : constant View_Data_Ref :=
                             Get_Data (Data.Tree_Db, Parent);
            begin
               Load_Sources (Parent_Db, Messages, Matching_Units);

               for Src of Parent_Db.Basenames loop
                  declare
                     Name : constant Simple_Name :=
                              Path_Name.Simple_Name (Src.Path_Name);
                  begin
                     if Context.Excluded_Sources.Contains (Name) then
                        Data.Actually_Excluded.Include (Name, Src);
                     elsif not Data.Sources.Contains (Src.Path_Name) then
                        declare
                           Inherited : constant Source_Proxy :=
                             (Src.Path_Len, Src.View, Parent, Src.Path_Name);
                        begin
                           Add_Source (Data, Inherited);
                           Insert_Inherited (Data, Inherited, Clashes);
                        end;
                     end if;
                  end;
               end loop;
            end;
         end loop;
      end if;

      for C in Clashes.Iterate loop
         declare
            Name : constant Simple_Name := Basename_Source_Maps.Key (C);
            First : constant Source_Proxy := Data.Basenames (Name);
            Owner : constant View_Data_Ref :=
                      Get_Data (Data.Tree_Db, First.View);
            Info : constant Src_Info_Maps.Constant_Reference_Type :=
                     Owner.Src_Infos.Constant_Reference (First.Path_Name);
         begin
            if First.View.Is_Compilable (Info.Language) then
               if First.View = Data.View then
                  Messages.Append
                    (Message.Create
                       (Message.Critical_Error,
                        "The source with basename """ & String (Name) &
                          """ appears multiple times",
                        Data.View.Attribute
                          (Project.Registry.Attribute.Source_Dirs)));
               else
                  Messages.Append
                    (Message.Create
                       (Message.Critical_Error,
                        '"' & String (Name) & '"' &
                          " is found in several extended projects",
                        Source_Reference.Create
                          (Data.View.Path_Name.Value, 0, 0)));

                  declare
                     Second : constant Source_Proxy := Clashes (C);

                  begin
                     if First.Path_Name < Second.Path_Name then
                        Report_Source (First, Messages);
                        Report_Source (Second, Messages);
                     else
                        Report_Source (Second, Messages);
                        Report_Source (First, Messages);
                     end if;
                  end;
               end if;
               Data.Basenames.Delete (Name);
            end if;
         end;
      end loop;

      Register_Units (Data, Messages);

      --  Check that we've found all the listed sources
      for S of Context.Listed_Sources loop
         if not Context.Excluded_Sources.Contains (S)
           and then not Data.Basenames.Contains (S)
           and then not Clashes.Contains (S)
         then
            Messages.Append
              (Message.Create
                 (Message.Critical_Error,
                  "source file """ & String (S) & """ not found",
                  (if Data.View.Has_Attribute (PRA.Source_List_File)
                   then Data.View.Attribute (PRA.Source_List_File)
                   else Data.View.Attribute (PRA.Source_Files))));
         end if;
      end loop;

      --  Check that we found all the excluded sources

      for C in Context.Excluded_Sources.Iterate loop
         declare
            Name : constant Filename_Type :=
                     Containers.Source_Path_To_Sloc.Key (C);
            Sloc : constant Source_Reference.Object'Class :=
                     Containers.Source_Path_To_Sloc.Element (C);
         begin
            if Sloc.Is_Defined
              and then not Data.View.Is_Runtime
              and then not Data.Actually_Excluded.Contains (Name)
            then
               Messages.Append
                 (Message.Create
                    (Message.Warning,
                     "unknown excluded file """ & String (Name) & '"',
                     Sloc));
            end if;
         end;
      end loop;

      if Data.View.Has_Package (PRP.Naming,
                                Check_Extended => False,
                                With_Defaults  => False,
                                With_Config    => False)
      then
         --  Check only locally declared naming exceptions

         for A of Data.View.Attributes
                    (Name          => PRA.Naming.Spec,
                     With_Defaults => False,
                     With_Config   => False)
         loop
            if not Data.Basenames.Contains (Simple_Name (A.Value.Text))
              and then not Clashes.Contains (Simple_Name (A.Value.Text))
            then
               Messages.Append
                 (Message.Create
                    ((if Data.View.Has_Attribute (PRA.Source_Files)
                     or else Data.View.Has_Attribute (PRA.Source_List_File)
                     then Message.Warning
                     else Message.Critical_Error),
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
            if not Is_Simple_Name (Filename_Type (A.Value.Text)) then
               Messages.Append
                 (Message.Create
                    (Message.Critical_Error,
                     "source file """ & A.Value.Text
                     & """ for unit """ & A.Index.Text
                     & """ must be a simple filename",
                     A.Value));

            elsif not Data.Basenames.Contains (Simple_Name (A.Value.Text))
              and then not Clashes.Contains (Simple_Name (A.Value.Text))
            then
               Messages.Append
                 (Message.Create
                    ((if Data.View.Has_Attribute (PRA.Source_Files)
                     or else Data.View.Has_Attribute (PRA.Source_List_File)
                     then Message.Warning
                     else Message.Critical_Error),
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
                  if not Data.Basenames.Contains (Simple_Name (V.Text))
                    and then not Clashes.Contains (Simple_Name (V.Text))
                  then
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
                  if not Data.Basenames.Contains (Simple_Name (V.Text))
                    and then not Clashes.Contains (Simple_Name (V.Text))
                  then
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

   ------------------
   -- Process_File --
   ------------------

   function Process_File
     (Data           : View_Data_Ref;
      Context        : Source_Context;
      File           : Full_Name;
      Matching_Units : Boolean;
      Messages       : in out GPR2.Log.Object) return Boolean
   is

      --  Check naming exceptions before language suffixes

      Basename         : constant Filename_Type :=
                             GPR2.Path_Name.Simple_Name (File);
      Current_Is_Src_Subdirs : constant Boolean :=
        Context.Tree.Has_Src_Subdirs
        and then
          Path_Name.Create_File (File).Containing_Directory =
            Data.View.Source_Subdirectory;
      --  Use the file's directory: recursive walks can return to a parent

      Match            : Source_Detection := No_Match;
      Units            : Unit_Info.List.Object;  --  For Ada
      Kind             : Valid_Unit_Kind;
      Source           : Build.Source_Base.Object;
      Parsed           : Boolean := False;
      Exc_Attr         : Source_Reference.Object;

   begin
      if Current_Is_Src_Subdirs then
         --  consider everything from src_subdirs
         null;

      elsif Context.Excluded_Sources.Contains (Basename) then
         --  Stop here if it's one of the excluded sources, or it's not in the
         --  included sources if those are given explicitely.

         Data.Actually_Excluded.Include
           (Basename,
            (File'Length, Project.View.Undefined,
             Project.View.Undefined, File));

         return False;

      elsif Context.Has_Source_List
        and then not Context.Listed_Sources.Contains (Basename)
      then
         return False;
      end if;

      for Language of Data.View.Language_Ids loop
         --  First, try naming exceptions

         Check_Naming_Exceptions
           (Data, Context, File, Language, Match, Kind, Units,
            Exc_Attr, Messages);

         if Match = No_Match then
            Check_Naming_Scheme (Context, Basename, Language, Match, Kind);
         end if;

         --  If we have a match from either naming exception or scheme
         --  we create the Source object.

         if Match /= No_Match then
            Source := Build.Source_Base.Object
              (Build.Source_Base.Create
                 (Path_Name.Create_File (File),
                  Language         => Language,
                  Kind             => Kind,
                  Tree_Db          => Data.Tree_Db,
                  Naming_Exception => Match = Naming_Exception,
                  From_Src_Subdirs => Current_Is_Src_Subdirs));

            --  If we know the units in the source (from naming exception),
            --  then add them now.

            for U of Units loop
               Source.Update_Unit (U);
            end loop;
         end if;

         --  Check Unit names and kind for Ada:

         if Match = Naming_Convention and then Language = Ada_Language then
            declare
               Last_Dot  : Natural;
               Success   : Boolean;
               Unit_Name : constant Name_Type :=
                             Compute_Unit_From_Filename
                               (File     => File,
                                Kind     => Kind,
                                NS       =>
                                  Context.Naming_Schema_Map (Ada_Language),
                                Dot_Repl => Context.Dot_Repl,
                                Messages => Messages,
                                Last_Dot => Last_Dot,
                                Success  => Success);

            begin
               if not Success then
                  return False;
               end if;

               if not Compilation_Unit.Check_Name_Validity
                 (Unit_Name,
                  Source_Reference.Create (File, 0, 0),
                  False,
                  Messages)
               then
                  return False;
               end if;

               --  Naming exceptions override the naming scheme

               if Kind in S_Spec | S_Body
                 and then not Context.Ada_Naming_Exceptions.Is_Empty
               then
                  declare
                     Attr : constant Project.Attribute.Object :=
                              Data.View.Attribute
                                ((if Kind = S_Spec
                                  then PRA.Naming.Spec
                                  else PRA.Naming.Body_N),
                                 PAI.Create (Value_Type (Unit_Name)));
                  begin
                     if Attr.Is_Defined
                       and then Attr.Value.Text /= String (Basename)
                     then
                        return False;
                     end if;
                  end;
               end if;

               --  Sources_Only uses inferred units. Parse to resolve
               --  krunched names, separates and No_Body when requested.

               if Data.Tree_Db.Source_Option >= Sources_Units then
                  Build.Source_Base.Ada_Parser.Compute
                    (File_Reader      => Context.Tree.File_Reader,
                     Data             => Source,
                     Get_Withed_Units => True,
                     Success          => Parsed);
               end if;

               --  Check if we have a conflicting naming exception with the
               --  same unit but a different source

               if Parsed
                 and then Source.Kind /= S_No_Body
               then
                  if not Context.Ada_Naming_Exceptions.Is_Empty then
                     declare
                        Attr : GPR2.Project.Attribute.Object;
                     begin
                        if Source.Kind = S_Spec then
                           Attr := Data.View.Attribute
                             (PRA.Naming.Spec,
                              PAI.Create
                                (Value_Type (Source.Unit.Full_Name)));
                        else
                           Attr := Data.View.Attribute
                             (PRA.Naming.Body_N,
                              PAI.Create
                                (Value_Type (Source.Unit.Full_Name)));
                        end if;

                        if Attr.Is_Defined
                          and then Attr.Value.Text /=
                            String (Source.Path_Name.Simple_Name)
                        then
                           --  We have a naming exception on the unit name
                           --  for a different source, so ignore the other
                           --  sources with the same unit.
                           return False;
                        end if;
                     end;
                  end if;

                  if Unit_Name /= Source.Unit.Full_Name
                    and then Path_Name.Base_Name (File) /=
                      Krunch (Source.Unit.Full_Name)
                  then
                     if Data.View.Is_Runtime then
                        --  Ignore sources from the runtime that don't
                        --  follow the naming convention: they're not
                        --  part of the runtime library.
                        return False;

                     elsif Matching_Units then
                        Messages.Append
                          (Message.Create
                             (Message.Warning,
                              "unit name """ &
                                String (Source.Unit.Name) &
                                """ does not match source name. " &
                                "The source will be ignored.",
                              SR.Create (File, 0, 0)));

                        return False;
                     end if;
                  end if;

               elsif not Parsed and then Kind = S_Separate then
                  --  Separates must have a doted unit name

                  if Last_Dot > 0 then
                     Source.Update_Unit
                       (Unit_Info.Create
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
                           SR.Create (File, 0, 0)));

                     return False;
                  end if;

               elsif not Parsed then
                  Source.Update_Unit
                    (Unit_Info.Create
                       (Unit_Name     => Unit_Name,
                        Index         => No_Index,
                        Kind          => Kind,
                        Separate_Name => "",
                        Parsed        => False));
               end if;
            end;

         elsif Match = Naming_Exception
           and then Language = Ada_Language
           and then not Units.Is_Indexed_List
           and then Data.Tree_Db.Source_Option >= Sources_Units
         then
            Build.Source_Base.Ada_Parser.Compute
              (File_Reader      => Context.Tree.File_Reader,
               Data             => Source,
               Get_Withed_Units => True,
               Success          => Parsed);

            --  Check parsed unit name is the same as the one declared
            --  in the gpr project.

            if Parsed
              and then Source.Unit.Kind /= S_No_Body
              and then Source.Unit.Full_Name /=
                Units (No_Index).Full_Name
            then
               Messages.Append
                 (Message.Create
                    (Message.Warning,
                     "in """ & String (Source.Path_Name.Simple_Name) &
                       """, actual unit name """ &
                       String
                       (Source.Unit.Full_Name) &
                       """ differs from the one declared in the " &
                       "project : """ &
                       String (Units (No_Index).Full_Name) & '"',
                     Exc_Attr));
            end if;
         end if;

         if Match /= No_Match then
            Data.Src_Infos.Insert (File, Source);

            return True;
         end if;
      end loop;

      return False;
   end Process_File;

   ----------------------
   -- Read_Source_List --
   ----------------------

   procedure Read_Source_List
     (View      : Project.View.Object;
      Attr      : Project.Attribute.Object;
      Set       : in out Source_Set.Set;
      Messages  : in out GPR2.Log.Object)
   is
      use Ada.Strings;
      use type Ada.Strings.Maps.Character_Set;
      package PRA renames Project.Registry.Attribute;

      Filename : constant GPR2.Path_Name.Object :=
                   (if GNAT.OS_Lib.Is_Absolute_Path (Attr.Value.Text)
                    then Path_Name.Create_File
                      (Filename_Type (Attr.Value.Text))
                    else View.Dir_Name.Compose
                      (Filename_Type (Attr.Value.Text)));
      Skip_Set : constant Strings.Maps.Character_Set :=
                   Maps.Constants.Control_Set or Maps.To_Set (" ");
      F        : Text_IO.File_Type;

   begin
      if View.Kind not in K_Standard | K_Library then
         return;
      end if;

      if not Filename.Exists or else Filename.Is_Directory then
         Messages.Append
           (Message.Create
              (Message.Critical_Error,
               (if Attr.Name.Id = PRA.Excluded_Source_List_File
                then "excluded "
                else "") & "source list file " &
                  Filename.String_Value & " not found",
               Sloc => Attr));

         return;
      end if;

      Text_IO.Open (F, Text_IO.In_File, Filename.String_Value);

      while not Text_IO.End_Of_File (F) loop
         declare
            Line : constant String :=
                     Fixed.Trim (Text_IO.Get_Line (F), Skip_Set, Skip_Set);

         begin
            if Line /= ""
              and then not GNATCOLL.Utils.Starts_With (Line, "--")
            then
               Include_Simple_Filename
                 (Set, Line, Attr.Value, Messages);
            end if;
         end;
      end loop;

      Text_IO.Close (F);
   end Read_Source_List;

   --------------------
   -- Register_Units --
   --------------------

   procedure Register_Units
     (Data     : View_Data_Ref;
      Messages : in out GPR2.Log.Object)
   is
      Sources : Basename_Source_Maps.Map;
   begin
      if Data.View.Is_Extended then
         return;
      end if;

      --  Clash handling can remove a source while registering its units
      Sources := Data.Basenames;
      for Src of Sources loop
         if Data.Sources.Contains (Src.Path_Name) then
            declare
               Owner : constant View_Data_Ref :=
                         Get_Data (Data.Tree_Db, Src.View);
               Info  : constant Src_Info_Maps.Constant_Reference_Type :=
                         Owner.Src_Infos.Constant_Reference (Src.Path_Name);
            begin
               if Info.Has_Units then
                  for U of Info.Units loop
                     for Root of Src.View.Namespace_Roots loop
                        if Root.Kind in With_View_Db
                          and then U.Kind /= S_No_Body
                        then
                           Add_Unit_Part
                             (Get_Data (Data.Tree_Db, Root),
                              U.Name, U.Kind, U.Separate_Name,
                              Data, Info.Path_Name, Src, U.Index, Messages);
                        end if;
                     end loop;
                  end loop;
               end if;
            end;
         end if;
      end loop;
   end Register_Units;

   -------------------
   -- Report_Source --
   -------------------

   procedure Report_Source
     (Src : Source_Proxy; Messages : in out GPR2.Log.Object) is
   begin
      Messages.Append
        (Message.Create
           (Message.Critical_Error, String (Src.Path_Name),
            Source_Reference.Create (Src.View.Path_Name.Value, 0, 0),
            Indent => 1));
   end Report_Source;

end Update_Sources_List;
