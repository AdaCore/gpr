--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;

with GNATCOLL.OS.Dir;
with GNATCOLL.OS.Stat;

with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Tree_Internal;

package body GPR2.View_Internal is

   package ACH renames Ada.Characters.Handling;
   package ASF renames Ada.Strings.Fixed;
   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;
   package SR  renames GPR2.Source_Reference;

   ----------------------------------
   -- Check_Aggregate_Library_Dirs --
   ----------------------------------

   procedure Check_Aggregate_Library_Dirs (Tree : Tree_Internal.Object) is
   begin
      for V of Tree.Ordered_Views loop
         if V.Is_Aggregated_In_Library then
            for View of V.Aggregate_Libraries loop
               if V.Kind in With_Object_Dir_Kind
                 and then View.Library_Directory = V.Object_Directory
               then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library directory cannot be shared "
                        & "with object directory of aggregated project """
                        & String (V.Path_Name.Base_Name) & """"));

               elsif V.Kind in With_Object_Dir_Kind
                 and then View.Library_Ali_Directory = V.Object_Directory
               then
                  Tree.Log_Messages.Append
                    (Message.Create
                       (Level   => Message.Error,
                        Sloc    => SR.Value.Create
                          (Filename => View.Path_Name.Value,
                           Line     => 0,
                           Column   => 0,
                           Text     => ""),
                        Message =>
                          "aggregate library ALI directory cannot be shared "
                        & "with object directory of aggregated project """
                        & String (V.Path_Name.Base_Name) & """"));
               end if;
            end loop;
         end if;
      end loop;
   end Check_Aggregate_Library_Dirs;

   --------------------------------
   -- Check_Excluded_Source_Dirs --
   --------------------------------

   procedure Check_Excluded_Source_Dirs (Tree : Tree_Internal.Object) is

      function Exists
        (View_Path : Path_Name.Object;
         Val       : Value_Type) return Boolean;
      --  Check if Excluded_Source_Dirs attribute value exists on the path.
      --  The attribute value is considered as a relative path from the view
      --  directory. If there is no match we try to process it as an
      --  absolute value and we check the existence of the relative path
      --  between the two absolute paths.

      ------------
      -- Exists --
      ------------

      function Exists
        (View_Path : Path_Name.Object;
         Val       : Value_Type) return Boolean
      is
         Recursive : constant Boolean :=
                       Val'Length >= 2
                           and then Val (Val'Last - 1 .. Val'Last) = "**";
         Last      : constant Natural :=
                       (if Recursive then Val'Last - 2 else Val'Last);
         Dir_Val   : constant Value_Type := Val (Val'First .. Last);
      begin
         if Dir_Val'Length = 0 then
            return True;

         else
            declare
               Dir_Name     : constant GPR2.Path_Name.Object :=
                                GPR2.Path_Name.Create_Directory
                                  (Filename_Type (Dir_Val),
                                   View_Path.Name);
               Relative_Dir : constant Filename_Type :=
                                Dir_Name.Relative_Path (From => View_Path);
            begin
               return View_Path.Compose (Relative_Dir, True).Exists;
            end;
         end if;
      end Exists;

   begin
      for V of Tree.Ordered_Views loop
         if V.Kind in With_Source_Dirs_Kind then
            declare
               V_Path : constant Path_Name.Object := V.Dir_Name;
               Attr   : constant Project.Attribute.Object :=
                          V.Attribute (PRA.Excluded_Source_Dirs);
            begin
               if Attr.Is_Defined then
                  for Val of Attr.Values loop
                     if not Exists (V_Path, Val.Text) then
                        Tree.Log_Messages.Append
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

   procedure Check_Package_Naming (Tree : Tree_Internal.Object) is

      procedure Check_View (View : Project.View.Object);
      --  Checks in View tree Casing, Dot_Replacement and Suffix attributes
      --  values.

      ----------------
      -- Check_View --
      ----------------

      procedure Check_View (View : Project.View.Object) is

         package Suffix_Lang_Maps is new Ada.Containers.Indefinite_Hashed_Maps
           (Value_Type, Language_Id, Ada.Strings.Hash, "=");

         Suffix_Lang_Map : Suffix_Lang_Maps.Map;
         --  key=suffix value; value=first language registering suffix use
         --  map used to detect/report multiple use of a suffix.

         procedure Log_Error
           (Level : Message.Level_Value;
            Msg   : String;
            Sloc  : Source_Reference.Object'Class);
         --  log naming package's  attribute problem at 'Attribute' source ref

         procedure Check_Casing;
         --  check casing is in expected range

         procedure Check_Dot_Replacement;
         --  check dot_replacement is not illegal

         procedure Check_Naming_Exceptions;
         --  check the same source + index is not used for spec and body

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
              and then ACH.To_Lower (Casing.Value.Text)
                         not in "lowercase" | "uppercase" | "mixedcase"
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

         -----------------------------
         -- Check_Naming_Exceptions --
         -----------------------------

         procedure Check_Naming_Exceptions is
            procedure Internal (Attr_Id : Q_Attribute_Id);

            Sources : Containers.Filename_Source_Reference;

            --------------
            -- Internal --
            --------------

            procedure Internal (Attr_Id : Q_Attribute_Id) is
               C  : Containers.Filename_Source_Reference_Package.Cursor;
               Ok : Boolean;
            begin
               for Attr of View.Attributes (Attr_Id) loop
                  for V of Attr.Values loop
                     Sources.Insert
                       (Filename_Type (V.Text), V, C, Ok);
                     if not Ok then
                        Log_Error
                          (Message.Warning,
                           "file """ & String (V.Text) & """ "
                           &  "specified in naming exception more than once",
                           V);
                     end if;
                  end loop;
               end loop;
            end Internal;

         begin
            Internal (PRA.Naming.Specification_Exceptions);
            Internal (PRA.Naming.Implementation_Exceptions);
         end Check_Naming_Exceptions;

         ---------------
         -- Log_Error --
         ---------------

         procedure Log_Error
           (Level : Message.Level_Value;
            Msg   : String;
            Sloc  : Source_Reference.Object'Class) is
         begin
            Tree.Log_Messages.Append
              (Message.Create
                 (Level   => Level,
                  Sloc    => Sloc,
                  Message => Msg));
         end Log_Error;

      begin
         if View.Has_Package (PRP.Naming) then
            Check_Casing;
            Check_Dot_Replacement;
            Check_Naming_Exceptions;

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
      for View of Tree.Ordered_Views loop
         Check_View (View);
      end loop;
   end Check_Package_Naming;

   -----------------
   -- Clear_Cache --
   -----------------

   procedure Clear_Cache (Def : in out Data) is
   begin
      Def.Cache.Clear_Cache;
      Def.Dir_Cache := (others => <>);
   end Clear_Cache;

   -------------------
   -- Disable_Cache --
   -------------------

   procedure Disable_Cache (Def : in out Data) is
   begin
      Def.Cache.Disable_Cache;
   end Disable_Cache;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (Def : in out Data) is
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
                            (File      : Path_Name.Full_Name;
                             Timestamp : Ada.Calendar.Time);
      Directory_CB      : access procedure
                            (Directory       : Path_Name.Object;
                             Is_Root_Dir     : Boolean;
                             Do_Dir_Visit    : in out Boolean;
                             Do_Subdir_Visit : in out Boolean) := null)
   is
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

      States     : Walk_State_List_Access := new Walk_State_List (1 .. 4);
      New_States : Walk_State_List_Access;
      Current    : Natural := 0;
      Dir        : constant Filename_Optional :=
                     (if Directory_Pattern'Length = 0
                      then "."
                      else
                        (if Directory_Pattern = "**"
                         then "./**"
                         else Directory_Pattern));
      --  Normalize dir part avoiding "" & "**"
      Recursive : constant Boolean :=
                    Dir'Length > 2
                    and then Dir (Dir'Last - 1 .. Dir'Last) = "**"
                    and then Is_Directory_Separator (Dir (Dir'Last - 2));
      Last      : constant Positive :=
                    Dir'Last - (if Recursive then 2 else 0);
      Root_Dir  : constant String :=
                    (if GNAT.OS_Lib.Is_Absolute_Path (String (Dir))
                     then String (Dir (Dir'First .. Last))
                     else String (Base_Dir.Compose
                       (Dir (Dir'First .. Last)).Name));
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
         Is_Root_Dir : Boolean) is
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
            States (Current).Handle := Open (Dir.String_Value);
         exception
            when GNATCOLL.OS.OS_Error =>
               Current := Current - 1;

               Messages.Append
                 (Message.Create
                    (Message.Error,
                     """" & Dir.String_Value &
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
                     --  Don't create a Path_Name object here as this is
                     --  very costly and we can manipulate a pretty great
                     --  number of files here. Using a Filename_Type (e.g. a
                     --  string) is much faster.

                     File_CB
                       (States (Current).Dir.Dir_Name &
                          Filename_Type (Name (D_Entry)),
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

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
     (View : Project.View.Object) return Context.Object
   is (Get_RO (View).Tree.Contexts (View.Context));

end GPR2.View_Internal;
