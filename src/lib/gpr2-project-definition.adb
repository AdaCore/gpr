--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with GNAT.Regexp;

with GNATCOLL.OS.Dir;
with GNATCOLL.OS.Stat;

with GPR2.Containers;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

package body GPR2.Project.Definition is

   use GNAT;

   package ACH renames Ada.Characters.Handling;
   package ASF renames Ada.Strings.Fixed;
   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;
   package SR  renames GPR2.Source_Reference;
   package SRI renames SR.Identifier;

   package Regexp_List is new Ada.Containers.Indefinite_Vectors
     (Positive, GNAT.Regexp.Regexp, "=" => GNAT.Regexp."=");

   ----------------------------------
   -- Check_Aggregate_Library_Dirs --
   ----------------------------------

   procedure Check_Aggregate_Library_Dirs (View : Project.View.Object) is
      procedure Process_Aggregate (Proj : Project.View.Object);
      --  Recursive procedure to check the aggregated projects, as they may
      --  also be aggregated library projects.

      -----------------------
      -- Process_Aggregate --
      -----------------------

      procedure Process_Aggregate (Proj : Project.View.Object) is
      begin
         if Proj.Kind = K_Aggregate_Library then
            for V of Get_RO (Proj).Aggregated loop
               if V.Kind not in K_Aggregate_Library | K_Configuration
                 | K_Abstract
                 and then View.Library_Ali_Directory = V.Object_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library ALI directory cannot be shared " &
                          "with object directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               elsif V.Is_Library
                 and then View.Library_Ali_Directory = V.Library_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library ALI directory cannot be shared " &
                          "with library directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               elsif V.Kind not in K_Aggregate_Library | K_Configuration
                 | K_Abstract
                 and then View.Library_Directory = V.Object_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library directory cannot be shared " &
                          "with object directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               elsif V.Is_Library
                 and then View.Library_Directory = V.Library_Directory
               then
                  View.Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library directory cannot be shared " &
                          "with library directory of aggregated project """ &
                          String (V.Path_Name.Base_Name) & """"));
               end if;

               Process_Aggregate (V);

            end loop;
         end if;
      end Process_Aggregate;
   begin
      Process_Aggregate (View);
   end Check_Aggregate_Library_Dirs;

   --------------------------------
   -- Check_Excluded_Source_Dirs --
   --------------------------------

   procedure Check_Excluded_Source_Dirs (View : Project.View.Object) is
   begin
      for V of View.Tree.Ordered_Views loop
         if V.Kind in With_Source_Dirs_Kind then
            declare
               V_Path : constant Path_Name.Object := V.Dir_Name;
               Attr   : constant Project.Attribute.Object :=
                          V.Attribute (PRA.Excluded_Source_Dirs);
            begin
               if Attr.Is_Defined then
                  for Val of Attr.Values loop
                     if not V_Path.Compose
                       (Filename_Type (Val.Text)).Exists
                     then
                        View.Tree.Log_Messages.Append
                          (Message.Create
                             (Level   => Message.Error,
                              Sloc    => Val,
                              Message =>
                                """" & Val.Text &
                                """ is not a valid directory"));
                     end if;
                  end loop;
               end if;
            end;
         end if;
      end loop;
   end Check_Excluded_Source_Dirs;

   --------------------------
   -- Check_Package_Naming --
   --------------------------

   procedure Check_Package_Naming (View : Project.View.Object) is
      procedure Check_View (View : Project.View.Object);
      --  Checks in View tree Casing, Dot_Replacement and Suffix attributes
      --  values.

      ----------------
      -- Check_View --
      ----------------

      procedure Check_View (View : Project.View.Object) is

         package Suffix_Lang_Maps is
           new Ada.Containers.Indefinite_Hashed_Maps
             (Value_Type, Language_Id, Ada.Strings.Hash, "=");

         Suffix_Lang_Map : Suffix_Lang_Maps.Map;
         --  key=suffix value; value=first language registering suffix use
         --  map used to detect/report multiple use of a suffix.

         procedure Log_Error
           (Level     : Message.Level_Value;
            Msg       : String;
            Attribute : Project.Attribute.Object);
         --  log naming package's  attribute problem at 'Attribute' source ref

         procedure Check_Casing;
         --  check casing is in expected range

         procedure Check_Dot_Replacement;
         --  check dot_replacement is not illegal

         use type Project.Attribute.Object;

         procedure Check_Illegal_Suffix
           (Attribute_Name : Q_Attribute_Id;
            Language       : Language_Id;
            Attribute      : Project.Attribute.Object)
           with Pre => Attribute /= Project.Attribute.Undefined;
         --  check Spec_Suffix, Body_Suffix or Separate_Suffix is not illegal

         ------------------
         -- Check_Casing --
         ------------------

         procedure Check_Casing is
            Casing : Project.Attribute.Object;
         begin
            if View.Check_Attribute (PRA.Naming.Casing, Result => Casing)
              and then ACH.To_Lower (Casing.Value.Text) not in
              "lowercase" | "uppercase" | "mixedcase"
            then
               Log_Error (Message.Error, "invalid value for casing", Casing);
            end if;
         end Check_Casing;

         ---------------------------
         -- Check_Dot_Replacement --
         ---------------------------

         procedure Check_Dot_Replacement is
            Dot_Replacement : constant Project.Attribute.Object :=
                                View.Attribute (PRA.Naming.Dot_Replacement);
            Value           : constant String :=
                                Dot_Replacement.Value.Text;
            Not_OK          : Boolean := False;
            subtype Printable_ASCII is Character range '!' .. '~';
         begin
            --  It must not be empty
            --  It cannot start or end with an alphanumeric character
            --  It cannot be a single underscore
            --  It cannot start with an underscore followed by an alphanumeric
            --  It cannot contain a dot '.' unless the entire string is "."
            --  It cannot include a space or a char that is not printable ASCII

            if ACH.Is_Alphanumeric (Value (Value'First))
              or else ACH.Is_Alphanumeric (Value (Value'Last))
              or else (Value (Value'First) = '_'
                       and then (Value'Length = 1
                                 or else ACH.Is_Alphanumeric
                                   (Value (Value'First + 1))))
              or else (Value'Length > 1
                       and then ASF.Index
                         (Source => Value, Pattern => ".") > 0)
            then
               Not_OK := True;

            else
               for J in Value'Range loop
                  if not (Value (J) in Printable_ASCII) then
                     Not_OK := True;
                     exit;
                  end if;
               end loop;
            end if;

            if Not_OK then
               Log_Error
                 (Message.Error,
                  """" & Value & """ is illegal for Dot_Replacement",
                  Dot_Replacement);
            end if;
         end Check_Dot_Replacement;

         --------------------------
         -- Check_Illegal_Suffix --
         --------------------------

         procedure Check_Illegal_Suffix
           (Attribute_Name : Q_Attribute_Id;
            Language       : Language_Id;
            Attribute      : Project.Attribute.Object)
         is
            Value    : constant Value_Type := Attribute.Value.Text;
            Dot_Repl : constant Value_Type :=
                         View.Attribute
                           (PRA.Naming.Dot_Replacement).Value.Text;
         begin
            if Value /= No_Value and then ASF.Index (Value, ".") = 0 then
               Log_Error
                 (Message.Error,
                  """" & Value & """ is illegal for "
                  & Image (Attribute_Name.Attr) & ": must have a dot",
                  Attribute);

               return;
            end if;

            --  Case of dot replacement is a single dot, and first character of
            --  suffix is also a dot.

            if Value'Length /= 0
              and then Dot_Repl'Length /= 0
              and then Dot_Repl = "."
              and then Value (Value'First) = '.'
            then
               for Index in Value'First + 1 .. Value'Last loop
                  --  If there are multiple dots in the name

                  if Value (Index) = '.' then
                     --  A letter is illegal following the initial dot

                     if ACH.Is_Letter (Value (Value'First + 1)) then
                        Log_Error
                          (Message.Error,
                           """" & Value & """ is illegal for "
                           & Image (Attribute_Name.Attr)
                           & ": ambiguous prefix when "
                           & "Dot_Replacement is a dot",
                           Attribute);
                     end if;

                     return;
                  end if;
               end loop;
            end if;

            --  detect/report multiple use of same suffix.
            --  Separate_Suffix = Body_Suffix ("Ada") is allowed.
            declare
               Associated_Lang : constant Suffix_Lang_Maps.Cursor :=
                                   Suffix_Lang_Map.Find (Value);
               Index           : constant Attribute_Index.Object :=
                                   Attribute_Index.Create (Ada_Language);
            begin
               if Suffix_Lang_Maps.Has_Element (Associated_Lang) then
                  if Suffix_Lang_Maps.Element (Associated_Lang) = Ada_Language
                    and then Attribute_Name = PRA.Naming.Separate_Suffix
                    and then View.Has_Attribute
                               (Name => PRA.Naming.Body_Suffix, Index => Index)
                    and then View.Attribute
                      (PRA.Naming.Body_Suffix, Index).Value.Text = Value
                  then
                     return;
                  end if;

                  if Language = Suffix_Lang_Maps.Element (Associated_Lang) then
                     Log_Error
                       (Message.Error,
                        Image (Attribute_Name.Attr) & " ("
                        & Image (Language) &
                        ") value already used for this language",
                        Attribute);
                  else
                     Log_Error
                       (Message.Error,
                        Image (Attribute_Name.Attr) & " ("
                        & Image (Language) &
                        ") value is already used for language " &
                        Image (Suffix_Lang_Maps.Element (Associated_Lang)),
                        Attribute);
                  end if;
               else
                  Suffix_Lang_Map.Include (Value, Language);
               end if;
            end;

         end Check_Illegal_Suffix;

         ---------------
         -- Log_Error --
         ---------------

         procedure Log_Error
           (Level     : Message.Level_Value;
            Msg       : String;
            Attribute : Project.Attribute.Object)
         is
         begin
            View.Tree.Log_Messages.Append
              (Message.Create
                 (Level   => Level,
                  Sloc    => Attribute,
                  Message => Msg));
         end Log_Error;

      begin
         if View.Has_Package (PRP.Naming) then
            Check_Casing;
            Check_Dot_Replacement;

            if View.Kind /= K_Aggregate and then View.Has_Languages then
               for L of View.Languages loop
                  declare
                     Language    : constant Language_Id := +Name_Type (L.Text);
                     Index       : constant Attribute_Index.Object :=
                                     Attribute_Index.Create (Language);
                     Spec_Suffix : constant Attribute.Object :=
                                     View.Attribute
                                       (PRA.Naming.Spec_Suffix, Index);
                     Body_Suffix : constant Attribute.Object :=
                                     View.Attribute
                                       (PRA.Naming.Body_Suffix, Index);
                  begin
                     if Spec_Suffix.Is_Defined
                       and then not Spec_Suffix.Is_Default
                     then
                        Check_Illegal_Suffix
                          (PRA.Naming.Spec_Suffix,
                           Language,
                           Spec_Suffix);
                     end if;

                     if Body_Suffix.Is_Defined
                       and then not Body_Suffix.Is_Default
                     then
                        Check_Illegal_Suffix
                          (PRA.Naming.Body_Suffix,
                           Language,
                           Body_Suffix);
                     end if;
                  end;
               end loop;
            end if;

            declare
               Sep_Suffix : constant Attribute.Object :=
                              View.Attribute (PRA.Naming.Separate_Suffix);
            begin
               if Sep_Suffix.Is_Defined and then not Sep_Suffix.Is_Default then
                  Check_Illegal_Suffix
                    (PRA.Naming.Separate_Suffix,
                     Ada_Language,
                     Sep_Suffix);
               end if;
            end;
         end if;
      end Check_View;

   begin
      for C in View.Tree.Iterate loop
         Check_View (Project.Tree.Element (C));
      end loop;
   end Check_Package_Naming;

   ------------------------------
   -- Check_Same_Name_Extended --
   ------------------------------

   procedure Check_Same_Name_Extended (View : Project.View.Object) is
      procedure Check_View (View : Project.View.Object);
      --  Checks in View tree (extended, aggregated, imported) that
      --  any extending list contains unique project name.

      ----------------
      -- Check_View --
      ----------------

      procedure Check_View (View : Project.View.Object) is
         OK    : Boolean;
         CN    : Containers.Name_Type_Set.Cursor;

         Names : Containers.Name_Set;
         --  set of already found extended's name.

         procedure Check_Extending (View : Project.View.Object);
         --  If View is extending, checks that extended projects list contains
         --  unique project's names.

         ---------------------
         -- Check_Extending --
         ---------------------

         procedure Check_Extending (View : Project.View.Object) is
         begin
            if View.Is_Extending then
               Names.Insert (View.Name, CN, OK);

               if not OK then
                  declare
                     Extending : constant Project.View.Object :=
                                   (if View.Is_Extended
                                    then View.Extending
                                    else View);
                  begin
                     View.Tree.Log_Messages.Append
                       (Message.Create
                          (Level   => Message.Error,
                           Sloc    => SR.Value.Create
                             (Filename => Extending.Path_Name.Value,
                              Line     => 0,
                              Column   => 0,
                              Text     => ""),
                           Message =>
                             "cannot extend a project with the same name"));
                  end;
               end if;

               Check_Extending (View.Extended_Root);
            end if;
         end Check_Extending;

         Def  : constant Const_Ref := Get_RO (View);

      begin
         Check_Extending (View);

         for V of Def.Imports loop
            Check_View (V);
         end loop;

         for V of Def.Aggregated loop
            Check_View (V);
         end loop;

      end Check_View;

   begin
      Check_View (View);
   end Check_Same_Name_Extended;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Def : in out Data)
   is
   begin
      Def.Cache.Clear_Cache;
      Def.Dir_Cache := (others => <>);
   end Clear_Cache;

   -------------------
   -- Disable_Cache --
   -------------------

   procedure Disable_Cache (Def : in out Data)
   is
   begin
      Def.Cache.Disable_Cache;
   end Disable_Cache;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (Def : in out Data)
   is
   begin
      Def.Cache.Enable_Cache;
   end Enable_Cache;

   -------------
   -- Foreach --
   -------------

   procedure Foreach
     (Base_Dir          : Path_Name.Object;
      Messages          : in out Log.Object;
      Directory_Pattern : Filename_Optional;
      Source            : Source_Reference.Value.Object;
      File_CB           : access procedure
                            (File      : Path_Name.Object;
                             Timestamp : Ada.Calendar.Time);
      Directory_CB      : access procedure
                            (Directory       : Path_Name.Object;
                             Is_Root_Dir     : Boolean;
                             Do_Dir_Visit    : in out Boolean;
                             Do_Subdir_Visit : in out Boolean) := null)
   is
      --  use GNAT.OS_Lib;
      use GNATCOLL.Utils;
      use GNATCOLL.OS.Dir;
      use GNATCOLL.OS.Stat;

      type Walk_State is record
         Do_Dir_Visit    : Boolean;
         Do_Subdir_Visit : Boolean;
         Is_Root_Dir     : Boolean;
         Dir             : Path_Name.Object;
         Handle          : Dir_Handle;
      end record;

      type Walk_State_List is array (Positive range <>) of Walk_State;
      type Walk_State_List_Access is access all Walk_State_List;

      procedure Free is new Ada.Unchecked_Deallocation
        (Walk_State_List, Walk_State_List_Access);

      States    : Walk_State_List_Access := new Walk_State_List (1 .. 4);
      New_States : Walk_State_List_Access;
      Current   : Natural := 0;
      Dir       : constant String :=
                    (if Directory_Pattern'Length = 0
                     then "."
                     else
                       (if Directory_Pattern = "**"
                        then "./**"
                        else String (Directory_Pattern)));
      --  Normalize dir part avoiding "" & "**"
      Recursive : constant Boolean :=
                    Dir'Length > 2
                    and then Dir (Dir'Last - 1 .. Dir'Last) = "**"
                    and then Is_Directory_Separator (Dir (Dir'Last - 2));
      Last      : constant Positive :=
                    Dir'Last - (if Recursive then 2 else 0);
      Root_Dir  : constant String :=
                    (if GNAT.OS_Lib.Is_Absolute_Path (Dir)
                     then Dir (Dir'First .. Last)
                     else Base_Dir.Compose
                       (Filename_Optional (Dir (Dir'First .. Last))).Value);
      D_Entry   : Dir_Entry;
      Stat      : File_Attributes;

      procedure Open_Directory
        (Dir         : Path_Name.Object;
         Is_Root_Dir : Boolean);
      --  Open a new directory for walk

      --------------------
      -- Open_Directory --
      --------------------

      procedure Open_Directory
        (Dir         : Path_Name.Object;
         Is_Root_Dir : Boolean)
      is
      begin
         if Current >= 512 then
            Messages.Append
              (Message.Create
                 (Message.Error,
                  "directory depth too big for """ &
                    String (Dir.Name) & """",
                  Source));
            return;

         elsif Current = States'Last then
            New_States := new Walk_State_List (1 .. 2 * States'Length);
            New_States (1 .. States'Length) := States.all;
            Free (States);
            States := New_States;
         end if;

         Current := Current + 1;
         States (Current) :=
           (Handle          => <>,
            Dir             => Dir,
            Do_Dir_Visit    => File_CB /= null,
            Do_Subdir_Visit => Recursive,
            Is_Root_Dir     => Is_Root_Dir);

         if Directory_CB /= null then
            Directory_CB
              (Dir,
               Is_Root_Dir,
               States (Current).Do_Dir_Visit,
               States (Current).Do_Subdir_Visit);
         end if;

         if not States (Current).Do_Dir_Visit
           and then not States (Current).Do_Subdir_Visit
         then
            Current := Current - 1;
            return;
         end if;

         begin
            States (Current).Handle := Open (String (Dir.Value));

         exception
            when GNATCOLL.OS.OS_Error =>
               Current := Current - 1;

               Messages.Append
                 (Message.Create
                    (Message.Error,
                     """" & String (Dir.Name) &
                       """ is not a valid directory",
                     Source));
               return;
         end;
      end Open_Directory;

   begin
      Open_Directory
        (Path_Name.Create_Directory
           (Filename_Type (Root_Dir), Path_Name.No_Resolution),
         Is_Root_Dir => True);

      while Current > 0 loop
         begin
            if States (Current).Do_Dir_Visit
              or else States (Current).Do_Subdir_Visit
            then
               loop
                  D_Entry := Read (States (Current).Handle);

                  exit when End_Of_Iteration (D_Entry);

                  Stat := Attributes (D_Entry);

                  if States (Current).Do_Dir_Visit and then Is_File (Stat) then
                     File_CB
                       (States (Current).Dir.Compose
                          (Filename_Type (Name (D_Entry))),
                        Modification_Time (Stat));

                  elsif States (Current).Do_Subdir_Visit
                    and then Is_Directory (Stat)
                    and then Name (D_Entry) not in "." | ".."
                  then
                     Open_Directory
                       (States (Current).Dir.Compose
                          (Filename_Type (Name (D_Entry)),
                           Directory => True),
                        Is_Root_Dir => False);
                  end if;
               end loop;
            end if;

            Close (States (Current).Handle);
            Current := Current - 1;

         exception
            when others =>
               for C in 1 .. Current loop
                  Close (States (C).Handle);
               end loop;

               Free (States);

               raise;
         end;
      end loop;

      Free (States);
   end Foreach;

   -----------------------
   -- Is_Sources_Loaded --
   -----------------------

   function Is_Sources_Loaded (View : Project.View.Object) return Boolean is
   begin
      return not Get_RO (View).Sources_Map.Is_Empty;
   end Is_Sources_Loaded;

   -----------------------------
   -- Source_Directories_Walk --
   -----------------------------

   procedure Source_Directories_Walk
     (View      : Project.View.Object;
      Source_CB : access procedure
                   (Dir_Reference : GPR2.Source_Reference.Value.Object;
                    Source        : GPR2.Path_Name.Object;
                    Timestamp     : Ada.Calendar.Time);
      Dir_CB    : access procedure (Dir_Name : GPR2.Path_Name.Object))
   is
      Visited_Dirs             : GPR2.Containers.Filename_Set;
      Dir_Ref                  : GPR2.Source_Reference.Value.Object;
      Ignored_Sub_Dirs         : constant GPR2.Project.Attribute.Object :=
                                   View.Attribute (PRA.Ignore_Source_Sub_Dirs);
      Ignored_Sub_Dirs_Regexps : Regexp_List.Vector;
      Excluded_Dirs            : constant GPR2.Project.Attribute.Object :=
                                   View.Attribute (PRA.Excluded_Source_Dirs);
      Excluded_Dirs_List       : GPR2.Path_Name.Set.Object;
      --  Ignore_Source_Sub_Dirs attribute regexps

      procedure On_Directory
        (Directory       : GPR2.Path_Name.Object;
         Is_Root_Dir     : Boolean;
         Do_Dir_Visit    : in out Boolean;
         Do_Subdir_Visit : in out Boolean);

      procedure On_File
        (File      : GPR2.Path_Name.Object;
         Timestamp : Ada.Calendar.Time);

      ------------------
      -- On_Directory --
      ------------------

      procedure On_Directory
        (Directory       : GPR2.Path_Name.Object;
         Is_Root_Dir     : Boolean;
         Do_Dir_Visit    : in out Boolean;
         Do_Subdir_Visit : in out Boolean)
      is
         Position : GPR2.Containers.Filename_Type_Set.Cursor;
         Inserted : Boolean;
      begin
         if Excluded_Dirs_List.Contains (Directory) then

            --  Do not visit this directory's files but still look for
            --  subdirectories.

            Do_Dir_Visit := False;

            return;
         end if;

         if not Is_Root_Dir then
            for Ignored_Sub_Dir of Ignored_Sub_Dirs_Regexps loop
               if GNAT.Regexp.Match
                 (String (Directory.Simple_Name), Ignored_Sub_Dir)
               then
                  --  Ignore this matching sub dir tree.
                  Do_Dir_Visit    := False;
                  Do_Subdir_Visit := False;

                  return;
               end if;
            end loop;
         end if;

         --  Do_Subdir_Visit is set to False if we already have visited
         --  this source directory:

         Visited_Dirs.Insert
           (Directory.Name, Position, Inserted);

         if not Inserted then
            --  Already visited
            Do_Dir_Visit    := False;

         elsif Dir_CB /= null then
            Dir_CB (Directory);
         end if;
      end On_Directory;

      -------------
      -- On_File --
      -------------

      procedure On_File
        (File      : GPR2.Path_Name.Object;
         Timestamp : Ada.Calendar.Time)
      is
      begin
         Source_CB (Dir_Ref, File, Timestamp);
      end On_File;

   begin
      if View.Kind not in With_Source_Dirs_Kind then
         return;
      end if;

      if Ignored_Sub_Dirs.Is_Defined then
         for V of Ignored_Sub_Dirs.Values loop
            if V.Text /= "" then
               Ignored_Sub_Dirs_Regexps.Append
                 (GPR2.Compile_Regexp (Filename_Optional (V.Text)));
            end if;
         end loop;
      end if;

      if Excluded_Dirs.Is_Defined then
         for V of Excluded_Dirs.Values loop
            Excluded_Dirs_List.Append
              (View.Dir_Name.Compose (Filename_Type (V.Text), True));
         end loop;
      end if;

      for S of View.Attribute (PRA.Source_Dirs).Values loop
         --  If S denotes the view's source dir corresponding to
         --  --src-subdir, just skip if the dir does not exist (it is
         --  optional).
         if not (View.Has_Source_Subdirectory
                 and then S.Text = View.Source_Subdirectory.Value
                 and then not Ada.Directories.Exists (S.Text))
         then
            Dir_Ref := S;
            Foreach
              (Base_Dir          => View.Dir_Name,
               Messages          => Get_RO (View).Tree.Log_Messages.all,
               Directory_Pattern => Filename_Optional (S.Text),
               Source            => S,
               File_CB           => (if Source_CB = null then null
                                     else On_File'Access),
               Directory_CB      => On_Directory'Access);
         end if;
      end loop;
   end Source_Directories_Walk;

   -----------------------
   -- Source_Map_Insert --
   -----------------------

   procedure Sources_Map_Insert
     (Def : in out Data;
      Src : Project.Source.Object;
      C   : Project.Source.Set.Cursor)
   is
      Position : Simple_Name_Source.Cursor;
      Inserted : Boolean;
   begin
      Def.Sources_Map.Insert
        (Src.Path_Name.Simple_Name, C, Position, Inserted);
   end Sources_Map_Insert;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean;
      Backends      : Source_Info.Backend_Set)
   is
   begin
      Update_Sources_List (Def, View, Stop_On_Error);
      Update_Sources_Parse
        (Def, Backends);
   end Update_Sources;

   -------------------------
   -- Update_Sources_List --
   -------------------------

   procedure Update_Sources_List
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean) is separate;

   --------------------------
   -- Update_Sources_Parse --
   --------------------------

   procedure Update_Sources_Parse
     (Def : in out Data; Backends : Source_Info.Backend_Set)
   is
      use type GPR2.Source_Info.Backend_Set;

      Repeat_Map  : Simple_Name_Source.Map; -- Second pass for subunits
      Position    : Simple_Name_Source.Cursor;
      Inserted    : Boolean;
      SW          : Project.Source.Object;

   begin
      Source_Info.Parser.Registry.Clear_Cache;
      Def.Units_Map.Clear;

      for C in Def.Sources.Iterate loop
         SW := Project.Source.Set.Element (C);

         --  If the view is extended, we will use the ALI from the extending
         --  project. We still need to call SW.Update to disambiguate
         --  Spec/Spec_Only and Body/Body_Only units.

         SW.Update
           (C,
            (if not Def.Is_Extended
             then Backends
             else Source_Info.No_Backends));

         if not SW.Is_Parsed (No_Index)
           and then not Def.Is_Extended
           and then SW.Language = Ada_Language
           and then Backends /= Source_Info.No_Backends
         then
            --  It can be subunit case in runtime krunched source names, need
            --  to repeat after all .ali files parsed.

            Repeat_Map.Insert
              (SW.Path_Name.Simple_Name, C, Position, Inserted);

            pragma Assert
              (Inserted,
               String (SW.Path_Name.Simple_Name) & " subunit duplicated");
         end if;
      end loop;

      for C of Repeat_Map loop
         SW := Project.Source.Set.Element (C);
         SW.Update (C, Backends);
      end loop;

      --  Check unit-based interface attributes

      if not Def.Interface_Units.Is_Empty then
         for C in Def.Interface_Units.Iterate loop
            declare
               Name : constant Name_Type := Unit_Name_To_Sloc.Key (C);
            begin
               if not Def.Units_Map.Contains ('S' & To_Lower (Name))
                 and then not Def.Units_Map.Contains ('B' & To_Lower (Name))
               then
                  Def.Tree.Append_Message
                    (Message.Create
                       (Message.Error,
                        "source for interface unit '" & String (Name)
                        & "' not found",
                        Unit_Name_To_Sloc.Element (C)));
               end if;
            end;
         end loop;
      end if;
   end Update_Sources_Parse;

end GPR2.Project.Definition;
