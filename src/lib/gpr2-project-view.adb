--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNATCOLL.Utils;

with GPR2.Build.Compilation_Input.Sets;
with GPR2.Build.Source;
with GPR2.Build.Source.Sets;
with GPR2.Build.View_Db;
with GPR2.Project.Attribute_Cache;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Project.View.Set;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Pack;

package body GPR2.Project.View is

   package PRP renames GPR2.Project.Registry.Pack;
   use GNAT;
   use type GPR2.View_Ids.View_Id;

   package Regexp_List is new Ada.Containers.Indefinite_Vectors
     (Positive, GNAT.Regexp.Regexp, "=" => GNAT.Regexp."=");

   function Get_Ref (View : Object) return Definition.Ref is
     (Definition.Data (View.Get.Element.all)'Unchecked_Access);

   function Get_RO (View : Object) return Definition.Const_Ref is
     (Definition.Data (View.Get.Element.all)'Unchecked_Access);

   function Get_DB (View : Object) return Build.View_Db.Object is
      (View.Tree.Artifacts_Database.View_Database (View));

   function Get_RW (View : in out Object) return Definition.Ref is
     (Definition.Data (View.Get.Element.all)'Unchecked_Access);

   function Refcount (Self : Object) return Natural is
     (Definition_References.Get_Refcount (Self));
   --  Get view refcount

   procedure Set_Def (Ref : out View.Object; Def : Definition_Base'Class);
   --  Convert definition to view

   function Weak (View : Object) return Weak_Reference is
     (Definition_References.Weak (View));

   function Strong (Weak : Weak_Reference) return Object;

   function Binder_Prefix
     (Self : Object; Language : Language_Id) return Filename_Optional
     with Pre => Self.Is_Defined;
   --  Prefix to be used for the binder exchange file name for the language.
   --  Used to have different binder exchange file names when binding different
   --  languages.

   function Remove_Body_Suffix
     (Self : Object; Name : Simple_Name) return Value_Not_Empty;
   --  Remove body suffix from Name

   function Attributes_Internal
     (Self          : Object;
      Name          : Q_Attribute_Id;
      With_Defaults : Boolean := True;
      With_Config   : Boolean := True)
      return Project.Attribute.Set.Object
   with Inline;

   type Source_Filter_Data is new Build.Source.Sets.Filter_Data with record
      View            : Object;
      Interface_Only  : Boolean := False;
      Compilable_Only : Boolean := False;
   end record;
   --  Options used to filter a list of sources

   function Source_Filter
     (S    : Build.Source.Object;
      Data : Build.Source.Sets.Filter_Data'Class) return Boolean;
   --  Function used to filter the sources in the Sources subprogram

   -------------------------
   -- Aggregate_Libraries --
   -------------------------

   function Aggregate_Libraries (Self : Object) return Set.Object is
      Result : Set.Object;
   begin
      for Id of Definition.Get_RO (Self).Agg_Libraries loop
         Result.Include (Self.Tree.Instance_Of (Id));
      end loop;

      return Result;
   end Aggregate_Libraries;

   ----------------
   -- Aggregated --
   ----------------

   function Aggregated (Self      : Object;
                        Recursive : Boolean := True) return Set.Object is
   begin
      return Set : GPR2.Project.View.Set.Object do
         for Agg of Definition.Get_RO (Self).Aggregated loop
            if Agg.Kind /= K_Aggregate or else not Recursive then
               Set.Insert (Agg);
            else
               Set.Union (Agg.Aggregated);
            end if;
         end loop;
      end return;
   end Aggregated;

   ----------------------------
   -- Apply_Root_And_Subdirs --
   ----------------------------

   function Apply_Root_And_Subdirs
     (Self : Object; Dir_Attr : Q_Attribute_Id) return GPR2.Path_Name.Object
   is
      function Compute return GPR2.Path_Name.Object;

      -------------
      -- Compute --
      -------------

      function Compute return GPR2.Path_Name.Object is
         Dir      : constant Value_Type :=
                      Self.Attribute (Dir_Attr).Value.Text;
         Subdirs  : constant Filename_Optional := Self.Tree.Subdirs;
         Dir_Name : constant Filename_Type :=
                      (if Dir = "" then "." else Filename_Type (Dir));
         Result   : GPR2.Path_Name.Object;
      begin
         if OS_Lib.Is_Absolute_Path (Dir) then
            Result := GPR2.Path_Name.Create_Directory (Dir_Name);

         elsif Self.Tree.Build_Path.Is_Defined then
            Result := GPR2.Path_Name.Create_Directory
              (Self.Dir_Name.Relative_Path
                 (Self.Tree.Root_Project.Dir_Name).Name,
               Filename_Type (Self.Tree.Build_Path.Value));

            Result := GPR2.Path_Name.Create_Directory
              (Dir_Name, Filename_Type (Result.Value));

         else
            Result := GPR2.Path_Name.Create_Directory
              (Dir_Name, Filename_Type (Self.Dir_Name.Value));
         end if;

         if Subdirs = No_Filename then
            return Result;
         end if;

         return GPR2.Path_Name.Create_Directory
           (Subdirs, Filename_Type (Result.Value));
      end Compute;

      Def      : Definition.Ref renames Self.Get_Ref;
      Def_Attr : Definition.Cacheable_Dir_Attrs;

   begin
      if Dir_Attr = PRA.Object_Dir then
         Def_Attr := Definition.Object_Dir;
      elsif Dir_Attr = PRA.Library_Ali_Dir then
         Def_Attr := Definition.Library_Ali_Dir;
      elsif Dir_Attr = PRA.Library_Dir then
         Def_Attr := Definition.Library_Dir;
      elsif Dir_Attr = PRA.Exec_Dir then
         Def_Attr := Definition.Exec_Dir;
      elsif Dir_Attr = PRA.Library_Src_Dir then
         Def_Attr := Definition.Library_Src_Dir;
      end if;

      if not Def.Dir_Cache (Def_Attr).Is_Set then
         Def.Dir_Cache (Def_Attr) :=
           (Is_Set => True,
            Value  => Compute);
      end if;

      return Def.Dir_Cache (Def_Attr).Value;
   end Apply_Root_And_Subdirs;

   ---------------
   -- Artifacts --
   ---------------

   function Artifacts (Self : Object) return GPR2.Path_Name.Set.Object is
      Result : GPR2.Path_Name.Set.Object;

      procedure Result_Append
        (Dir : GPR2.Path_Name.Object; Attr : Q_Attribute_Id);
      --  Append files created from directory name and filenames from list of
      --  attributes.

      -------------------
      -- Result_Append --
      -------------------

      procedure Result_Append
        (Dir : GPR2.Path_Name.Object; Attr : Q_Attribute_Id)
      is
         use Ada.Directories;
         Item : Directory_Entry_Type;
         Find : Search_Type;
      begin
         if not Exists (Dir.Value) then
            return;
         end if;

         for Name of Self.Clean_Attribute_List (Attr, No_Language) loop
            Start_Search
              (Search    => Find,
               Directory => Dir.Value,
               Pattern   => Name,
               Filter    => (Ordinary_File => True, others => False));

            while More_Entries (Find) loop
               Get_Next_Entry (Find, Item);

               Result.Append
                 (GPR2.Path_Name.Create_File
                    (Filename_Type (Full_Name (Item))));
            end loop;
         end loop;
      end Result_Append;

   begin
      Result_Append (Self.Object_Directory, PRA.Clean.Artifacts_In_Object_Dir);

      if Self.Kind = K_Standard then
         Result_Append (Self.Executable_Directory,
                        PRA.Clean.Artifacts_In_Exec_Dir);
      end if;

      return Result;
   end Artifacts;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Self   : Object;
      Name   : Q_Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index)
      return Project.Attribute.Object
   is
      use type Project.Attribute_Index.Object;
      use type PRA.Index_Value_Type;
      use type PRA.Inherit_From_Extended_Type;

      --  Compute the attribute qualified name

      Alias     : constant Q_Optional_Attribute_Id := PRA.Alias (Name);
      Has_Index : constant Boolean := Index /= Attribute_Index.Undefined;
      PRA_Def   : PRA.Def;
      Result    : Project.Attribute.Object;

      function Found (Attribute : Project.Attribute.Object) return Boolean
        with Inline => True;

      function Get_Attribute_From_View
        (View : Object) return Project.Attribute.Object;
      --  Internal function to get attribute from a view

      procedure Check_Matching_Index
        (Pattern : Project.Attribute.Object;
         Result  : in out Project.Attribute.Object);

      procedure Get_Config_Attribute;
      --  Returns the config attribute value for Name if given on the command
      --  line.

      function Get_Default_Index return Value_Type;

      --------------------------
      -- Check_Matching_Index --
      --------------------------

      procedure Check_Matching_Index
        (Pattern : Project.Attribute.Object;
         Result  : in out Project.Attribute.Object)
      is
         use type Source_Reference.Object;
      begin
         if GNATCOLL.Utils.Match  (Str     => Index.Text,
                                   Pattern => Pattern.Index.Text)
         then
            if not Found (Result)
              or else
                Source_Reference.Object (Result)
                  < Source_Reference.Object (Pattern)
            then
               Result := Pattern;
            end if;
         end if;
      end Check_Matching_Index;

      -----------
      -- Found --
      -----------

      function Found (Attribute : Project.Attribute.Object) return Boolean is
      begin
         --  Found if attribute is defined and not a default value, or is
         --  read_only (builtin value).

         return Attribute.Is_Defined and then
           (not Attribute.Is_Default or else PRA_Def.Builtin);
      end Found;

      -----------------------------
      -- Get_Attribute_From_View --
      -----------------------------

      function Get_Attribute_From_View
        (View : Object) return Project.Attribute.Object
      is
         function Get_Pack return Project.Pack.Set.Cursor with Inline;
         function Get_Attrs return Project.Attribute.Set.Object with Inline;

         ---------------
         -- Get_Attrs --
         ---------------

         function Get_Attrs return Project.Attribute.Set.Object is
         begin
            if Name.Pack = Project_Level_Scope then
               return View.Get_RO.Attrs;
            end if;

            declare
               Cursor : Project.Pack.Set.Cursor renames Get_Pack;
            begin
               if Project.Pack.Set.Has_Element (Cursor) then
                  return Project.Pack.Set.Element (Cursor).Attrs;
               else
                  return Project.Attribute.Set.Empty_Set;
               end if;
            end;
         end Get_Attrs;

         --------------
         -- Get_Pack --
         --------------

         function Get_Pack return Project.Pack.Set.Cursor is
            Def    : Definition.Const_Ref := View.Get_RO;
            Result : Project.Pack.Set.Cursor;
         begin
            loop
               Result := Def.Packs.Find (Name.Pack);
               if Project.Pack.Set.Has_Element (Result) then
                  return Result;
               end if;

               exit when not Def.Extended_Root.Is_Defined;

               Def := Def.Extended_Root.Get_RO;
            end loop;

            return Result;
         end Get_Pack;

         Result : Project.Attribute.Object;
         Attrs  : Project.Attribute.Set.Object renames Get_Attrs;

      begin
         --  First try to find an exact match

         Result := Attrs.Element (Name.Attr, Index, At_Pos);

         if not Found (Result) and then Alias /= No_Attribute_Id then
            Result := Attrs.Element (Alias.Attr, Index, At_Pos);
         end if;

         --  Checks for special index matching

         if not Found (Result)
           and then Index.Is_Defined
           and then Index.Value'Length > 0
         then
            if PRA_Def.Index_Type = PRA.FileGlob_Index
              or else
                (PRA_Def.Index_Type = PRA.FileGlob_Or_Language_Index
                 and then not Self.Has_Language (Name_Type (Index.Value)))
            then
               --  The index might match a globbing pattern. In that case
               --  iterate over the attributes to find one that match the
               --  index.

               for Attr of Attrs loop
                  if Attr.Name.Id = Name
                    or else (Alias /= No_Attribute_Id
                             and then Attr.Name.Id = Alias)
                  then
                     --  We should have a file name. Let's do pattern
                     --  matching.

                     Check_Matching_Index
                       (Pattern => Attr,
                        Result  => Result);

                     --  Note: we need to keep going here as last
                     --  match will be the one to take into account.
                  end if;
               end loop;
            end if;

            --  Now if we have a source as index, and haven't found any result
            --  and the attribute is defined for the source's language, let's
            --  return it.
            --  Depending on when this is called, be careful that the view may
            --  not be fully loaded yet (so of course no source available, but
            --  worse: check for source will blow because Root_View is not
            --  set at parse time but a bit later).

            if not Found (Result)
              and then PRA_Def.Index_Type = PRA.FileGlob_Or_Language_Index
              and then Self.Has_Source (GPR2.Simple_Name (Index.Value))
            then
               declare
                  Src : Build.Source.Object renames
                          Self.Source (GPR2.Simple_Name (Index.Value));
               begin
                  Result :=
                    Attribute (View, Name, PAI.Create (Src.Language));
               end;
            end if;

            --  Finally, check if an attribute with the "others" index is
            --  defined.

            if not Found (Result)
              and then PRA_Def.Index_Optional
            then
               Result :=
                 Attrs.Element (Name.Attr, Project.Attribute_Index.I_Others);

               if not Found (Result) and then Alias.Attr /= No_Attribute then
                  Result :=
                    Attrs.Element (Alias.Attr,
                                   Project.Attribute_Index.I_Others);
               end if;
            end if;
         end if;

         if Name.Pack = Project_Level_Scope
           and then PRA_Def.Inherit_From_Extended /= PRA.Not_Inherited
           and then View.Is_Extending
         then
            if not Found (Result) then
               return Get_Attribute_From_View (View.Extended_Root);

            elsif PRA_Def.Inherit_From_Extended = PRA.Concatenated then
               --  We need to fetch to concatenate the attribute value with
               --  the value from the extended project.

               declare
                  Result2 : Project.Attribute.Object;
               begin
                  Result2 := Get_Attribute_From_View (View.Extended_Root);

                  if Found (Result2) then
                     Result.Prepend_Vector (Result2);
                  end if;
               end;
            end if;
         end if;

         return Result;
      end Get_Attribute_From_View;

      --------------------------
      -- Get_Config_Attribute --
      --------------------------

      procedure Get_Config_Attribute is

         package SR renames Source_Reference;
         package SRA renames Source_Reference.Attribute;
         package SRV renames Source_Reference.Value;

         function From_Command_Line return Value_Type;

         -----------------------
         -- From_Command_Line --
         -----------------------

         function From_Command_Line return Value_Type is
         begin
            if Name = PRA.Target
              or else Name = PRA.Canonical_Target
            then
               if Self.Tree.Target_From_Command_Line /= "all"
                 and then Self.Tree.Target_From_Command_Line /= ""
               then
                  return Value_Type
                    (Self.Tree.Target_From_Command_Line
                       (Normalized => Name = PRA.Canonical_Target));
               end if;

            elsif Name = PRA.Runtime
              and then Index.Is_Defined
            then
               declare
                  Lang : constant Language_Id :=
                           +Optional_Name_Type (Index.Value);
               begin
                  return
                    Value_Type (Self.Tree.Runtime_From_Command_Line (Lang));
               end;
            end if;

            return "";
         end From_Command_Line;

         Cmd_Line : constant Value_Type := From_Command_Line;

      begin
         if Cmd_Line'Length > 0 then
            --  Value from command line always have priority over all other
            --  considerations.

            Result := PA.Create
              (Name  => SRA.Object (SRA.Create (SR.Builtin, Name)),
               Index => Index,
               Value => SRV.Object (SRV.Create (SR.Builtin, Cmd_Line)));

            return;
         end if;

         --  Return the value from the configuration project when it exists.
         --  It takes priority over any explicitly defined value for the
         --  view: the user may override such value from the command line
         --  (--RTS) or via an explicit config project.

         if Self.Tree.Has_Configuration
           and then Self.Tree.Configuration.Corresponding_View /= Self
         then
            Result := Get_Attribute_From_View
                        (Self.Tree.Configuration.Corresponding_View);

            if Result.Is_Defined and then not Result.Is_Default then
               --  Set the From_Config flag for the attribute
               Result.Set_From_Config (True);
            end if;

            if not Result.Is_Defined
              and then Name = PRA.Runtime
              and then Index.Is_Defined
            then
               --  Runtime names are not defined in the configuration project.
               --  However, we have them in our config parameters when run in
               --  autoconfig mode. Let's use that.

               declare
                  Value : constant Value_Type :=
                            Value_Type
                              (Self.Tree.Configuration.Runtime
                                 (+Optional_Name_Type (Index.Value)));
               begin
                  if Value'Length > 0 then
                     Result := PA.Create
                       (Name  => SRA.Object (SRA.Create (SR.Builtin, Name)),
                        Index => Index,
                        Value => SRV.Object (SRV.Create (SR.Builtin, Value)));
                     Result.Set_From_Config (True);
                  end if;
               end;
            end if;
         end if;

         --  At this point, if Result is undefined, we are in autoconf mode
         --  and don't have config yet. Let's use the regular attribute
         --  resolution.
      end Get_Config_Attribute;

      -----------------------
      -- Get_Default_Index --
      -----------------------

      function Get_Default_Index return Value_Type is
      begin
         if not Has_Index then
            return Registry.Attribute.Any_Index;
         else
            return Index.Value (Preserve_Case => Index.Is_Case_Sensitive);
         end if;
      end Get_Default_Index;

      Cache_Cursor : constant Project.Attribute_Cache.Cursor :=
         Definition.Get_RO (Self).Cache.Check_Cache
            (Name   => Name,
             Index  => Index,
             At_Pos => At_Pos);
   begin
      if Project.Attribute_Cache.Has_Element (Cache_Cursor) then
         return Project.Attribute_Cache.Element (Cache_Cursor);
      end if;

      Definition.Get_RO (Self).Cache.Schedule_Update_Cache;

      --  First check if the attribute is defined in the registry

      if not PRA.Exists (Name) then
         raise Attribute_Error
            with Image (Name) & " attribute does not exist";
      end if;

      --  Fetch the attribute definition

      PRA_Def := PRA.Get (Name);

      --  Check if index is used correctly

      if PRA_Def.Index_Type = PRA.No_Index and then Has_Index then
         raise Attribute_Error
           with Image (Name) & " attribute does not accept index";
      end if;

      --  Attributes that denote toolchain configuration need special
      --  handling: in particular they may be overwritten by the command
      --  line, via --target or --RTS switches.

      if PRA_Def.Is_Toolchain_Config then
         Get_Config_Attribute;
      end if;

      --  Try to fetch the attribute from the view

      if not Result.Is_Defined then
         Result := Get_Attribute_From_View (View => Self);
      end if;

      --  Handle configuration project

      if GPR2.Project.Tree.Has_Configuration (Self.Tree.all)
        and then Self.Tree.Configuration.Corresponding_View /= Self
      then
         if not Found (Result) then
            --  If at this stage Result is not defined try to fetch the value
            --  from the configuration project.

            Result := Get_Attribute_From_View
              (View => Self.Tree.Configuration.Corresponding_View);

            if Result.Is_Defined then
               --  Set the From_Config flag for the attribute
               Result.Set_From_Config (True);
            end if;

         elsif PRA_Def.Config_Concatenable then
            --  If the attribute value has been found, prepend if necessary
            --  the value from the configuration project.

            declare
               Result2 : Project.Attribute.Object;
            begin
               Result2 := Get_Attribute_From_View
                 (View => Self.Tree.all.Configuration.Corresponding_View);

               if Found (Result2) then
                  Result.Prepend_Vector (Result2);
               end if;
            end;
         end if;
      end if;

      --  The "Found" subprogram rejects default values: so here we just check
      --  if result is defined: no need to re-create a default value if we
      --  already have one.

      if not Result.Is_Defined and then PRA_Def.Has_Default_In (Self.Kind) then
         --  Finally use the default value if defined

         declare
            use type PRA.Value_Kind;
            use type PRA.Default_Value_Kind;
            package SR renames Source_Reference;

            Default : PRA.Default_Value renames PRA_Def.Default;

         begin
            case Default.Kind is
               when PRA.D_Attribute_Reference =>
                  declare
                     Project_SRef : constant SR.Object :=
                                      SR.Object
                                        (SR.Create (Self.Path_Name.Value,
                                         0, 0));
                     Attr_Name    : constant SR.Attribute.Object :=
                                      SR.Attribute.Object
                                        (SR.Attribute.Create
                                           (Project_SRef, Name));

                     --  ??? Separate_Suffix has no index and defaults to an
                     --  indexed attribute. We need some way to show that in
                     --  the attribute definition. In the mean time, let's
                     --  hardcode this behavior.

                     Ref          : constant Project.Attribute.Object :=
                                      Self.Attribute
                                        (Name   => (Name.Pack, Default.Attr),
                                         Index  =>
                                           (if Name =
                                              PRA.Naming.Separate_Suffix
                                            then Attribute_Index.Create
                                              (Ada_Language)
                                            else Index),
                                         At_Pos => At_Pos);
                  begin
                     if Ref.Is_Defined then
                        Result := Ref.Rename (Attr_Name);

                        --  See note above about Separate_Suffix...
                        if Name = PRA.Naming.Separate_Suffix then
                           Result.Set_Index (Attribute_Index.Undefined);
                        end if;
                     end if;
                  end;

               when PRA.D_Value =>
                  --  Retrieve the value. Note that not all indexes may be
                  --  referenced here, so we can still return the Undefined
                  --  attribute.
                  declare
                     Def_Index : Value_Type renames Get_Default_Index;
                  begin
                     if PRA.Exists (Default.Values, Def_Index) then
                        Result := Project.Attribute.Create
                          (Name    => Name,
                           Index   => Index,
                           Source  => Self.Path_Name,
                           Default => PRA.Get (Default.Values, Def_Index),
                           As_List => PRA_Def.Value /= PRA.Single);
                        Result.Set_Case (PRA_Def.Value_Case_Sensitive);
                     end if;
                  end;

               when PRA.D_Callback =>
                  --  Create the default value for the attribute from a
                  --  callback call.

                  Result := Project.Attribute.Create
                    (Name    => Name,
                     Index   => Index,
                     Source  => Self.Path_Name,
                     Default => Default.Callback (Self),
                     As_List => PRA_Def.Value /= PRA.Single);
                  Result.Set_Case (PRA_Def.Value_Case_Sensitive);
            end case;
         end;
      end if;

      if Alias.Attr /= No_Attribute
        and then Result.Is_Defined
        and then Result.Name.Id = Alias
      then
         --  We found the alternative name for the attribute. Let's rename
         --  it to the requested name.
         Result := GPR2.Project.Attribute.Get_Alias (Result, Name);
      end if;

      if PRA_Def.Value_Is_Set and then Result.Is_Defined then
         Result.Ensure_Set;
      end if;

      --  Finally return the result
      Definition.Get_RO (Self).Cache.Update_Cache
         (Name   => Name,
          Index  => Index,
          At_Pos => At_Pos,
          Attr   => Result);
      return Result;
   end Attribute;

   ------------------------
   -- Attribute_Location --
   ------------------------

   function Attribute_Location
     (Self  : Object;
      Name  : Q_Attribute_Id;
      Index : Attribute_Index.Object := Attribute_Index.Undefined)
      return Source_Reference.Object'Class
   is
      Attr : Project.Attribute.Object;
   begin
      Attr := Self.Attribute (Name => Name, Index => Index);

      if Attr.Is_Defined then
         return Attr;
      else
         return Source_Reference.Create (Self.Path_Name.Value, 0, 0);
      end if;
   end Attribute_Location;

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (Self          : Object;
      Name          : Q_Attribute_Id;
      With_Defaults : Boolean := True;
      With_Config   : Boolean := True)
      return Project.Attribute.Set.Object is
   begin
      return Attributes_Internal
        (Self, Name, With_Defaults, With_Config);
   end Attributes;

   function Attributes
     (Self          : Object;
      Pack          : Package_Id := Project_Level_Scope;
      With_Defaults : Boolean    := True;
      With_Config   : Boolean    := True)
      return Project.Attribute.Set.Object
   is
      Result : Project.Attribute.Set.Object;
   begin
      for Attr_Id of PRA.All_Attributes (Pack) loop
         for Attr of Self.Attributes_Internal
           (Name          => Attr_Id,
            With_Defaults => With_Defaults,
            With_Config   => With_Config)
         loop
            --  Do not include alias values, as they would duplicate their
            --  aliased attribute.
            if not Attr.Is_Alias then
               Result.Insert (Attr);
            end if;
         end loop;
      end loop;

      return Result;
   end Attributes;

   -------------------------
   -- Attributes_Internal --
   -------------------------

   function Attributes_Internal
     (Self          : Object;
      Name          : Q_Attribute_Id;
      With_Defaults : Boolean := True;
      With_Config   : Boolean := True)
      return Project.Attribute.Set.Object
   is
      Alias  : constant Q_Optional_Attribute_Id := PRA.Alias (Name);
      Def    : constant PRA.Def        := PRA.Get (Name);
      Result : Project.Attribute.Set.Object;

      use type PRA.Inherit_From_Extended_Type;
      use type PRA.Index_Value_Type;

      procedure Add_Attr (Attr   : Project.Attribute.Object;
                          Concat : Boolean);

      function Config return Object is
        (Self.Tree.Configuration.Corresponding_View);
      --  Configuration View. To be used only when the tree has a configuration

      --------------
      -- Add_Attr --
      --------------

      procedure Add_Attr
        (Attr   : Project.Attribute.Object;
         Concat : Boolean)
      is
         Cursor : constant GPR2.Project.Attribute.Set.Cursor :=
                    Result.Find (Name.Attr, Attr.Index);
      begin
         --  Check if we already have the same attribute in the main view

         if not Project.Attribute.Set.Has_Element (Cursor) then
            --  Nope, so just inherit
            Result.Insert (Attr);

         elsif Concat then
            --  If we have it, and we need to concatenate, then amend the
            --  value in Result.

            declare
               New_Attr : Project.Attribute.Object :=
                            Project.Attribute.Set.Element (Cursor);
            begin
               New_Attr.Prepend_Vector (Attr);
               Result.Include (New_Attr);
            end;
         end if;
      end Add_Attr;

   begin
      --  If the attribute has no index, then just call Attribute, as at most
      --  one result can be returned,

      if Def.Index_Type = PRA.No_Index then
         declare
            Attr : constant Project.Attribute.Object :=
                     Self.Attribute (Name => Name);
         begin
            if Attr.Is_Defined
              and then (With_Defaults or else not Attr.Is_Default)
              and then (With_Config or else not Attr.Is_From_Config)
            then
               Result.Include (Attr);
            end if;

            return Result;
         end;
      end if;

      if Name.Pack = Project_Level_Scope then
         Result := Get_RO (Self).Attrs.Filter (Name.Attr);

         if Alias.Attr /= No_Attribute then
            for Attr of Get_RO (Self).Attrs.Filter (Alias.Attr) loop
               --  Return the attributes with the requested name

               if not Result.Contains (Name.Attr, Attr.Index) then
                  Result.Include (Attr.Get_Alias (Name));
               end if;
            end loop;
         end if;

         --  Query extended views

         if Def.Inherit_From_Extended /= PRA.Not_Inherited
           and then Self.Is_Extending
         then
            for Attr of Self.Extended_Root.Attributes_Internal
              (Name, False, False)
            loop
               Add_Attr (Attr, Def.Inherit_From_Extended = PRA.Concatenated);
            end loop;
         end if;

      else
         declare
            --  Self.Pack resolves inheritance
            Pack_Inst : Project.Pack.Object renames Self.Pack (Name.Pack);
         begin
            if not Pack_Inst.Attrs.Is_Empty then
               Result := Pack_Inst.Attrs.Filter (Name.Attr);

               if Alias.Attr /= No_Attribute then
                  for Attr of Pack_Inst.Attrs.Filter (Alias.Attr) loop
                     if not Result.Contains (Name.Attr, Attr.Index) then
                        Result.Insert (Attr.Get_Alias (Name));
                     end if;
                  end loop;
               end if;
            end if;
         end;
      end if;

      --  Query configuration project

      if With_Config
        and then Self.Tree.Has_Configuration
      then
         for Attr of Config.Attributes_Internal (Name, False, False)
         loop
            Add_Attr (Attr, Def.Config_Concatenable);
         end loop;
      end if;

      --  Finally check the default value

      if With_Defaults
        and then Def.Has_Default_In (Self.Kind)
      then
         declare
            use GPR2.Project.Attribute.Set;

            Cursor : GPR2.Project.Attribute.Set.Cursor;
            Attr   : Project.Attribute.Object;
         begin
            case Def.Default.Kind is
               when PRA.D_Callback =>
                  null;

               when PRA.D_Attribute_Reference =>
                  for Attr of Self.Attributes_Internal
                    ((Name.Pack, Def.Default.Attr))
                  loop
                     Cursor := Result.Find (Name.Attr, Attr.Index);

                     if not Has_Element (Cursor) then
                        Result.Insert
                          (Attr.Rename
                             (GPR2.Source_Reference.Attribute.Object
                                  (GPR2.Source_Reference.Attribute.Create
                                       (GPR2.Source_Reference.Builtin,
                                        Name))));
                     end if;
                  end loop;

               when PRA.D_Value =>
                  for C in Def.Default.Values.Iterate loop
                     declare
                        Val_Index  : constant Value_Type :=
                                       PRA.Value_Map.Key (C);
                        Attr_Index : Attribute_Index.Object;
                     begin
                        if Val_Index /= PRA.Any_Index
                          and then Val_Index'Length > 0
                        then
                           Attr_Index := Attribute_Index.Create (Val_Index);
                           Attr_Index.Set_Case
                             (PRA.Is_Case_Sensitive
                                (Val_Index, Def.Index_Type));

                           Cursor := Result.Find (Name.Attr, Attr_Index);

                           if not Has_Element (Cursor) then
                              --  Create the value
                              Attr := Project.Attribute.Create
                                (Name    => Source_Reference.Attribute.Object
                                             (Source_Reference.Attribute.Create
                                               (Source_Reference.Builtin,
                                                Name)),
                                 Index   => Attr_Index,
                                 Value   => Source_Reference.Value.Object
                                              (Source_Reference.Value.Create
                                                (Source_Reference.Builtin,
                                                 PRA.Value_Map.Element (C))),
                                 Default => True);
                              Attr.Set_Case (Def.Value_Case_Sensitive);
                              Result.Insert (Attr);
                           end if;
                        end if;
                     end;
                  end loop;
            end case;
         end;
      end if;

      return Result;
   end Attributes_Internal;

   ----------------------
   -- Binder_Artifacts --
   ----------------------

   function Binder_Artifacts
     (Self     : Object;
      Name     : Simple_Name;
      Language : Language_Id := No_Language)
      return GPR2.Path_Name.Set.Object
   is
      use Ada.Text_IO;
      use GNATCOLL.Utils;

      Result  : GPR2.Path_Name.Set.Object;
      Obj_Dir : constant GPR2.Path_Name.Object := Self.Object_Directory;
      BP      : constant Filename_Optional :=
                  (if Language = No_Language then No_Filename
                   else Self.Binder_Prefix (Language));
      BF      : constant GPR2.Path_Name.Object :=
                  Obj_Dir.Compose
                    (BP & Name
                     & (if Self.Is_Library then ".lexch" else ".bexch"));

      File    : File_Type;
      Obj_Ext : constant Filename_Optional :=
                  (if Language = No_Language then No_Filename
                   else Self.Tree.Object_Suffix (Language));

      Generated : Boolean := False;
      Gen_Src   : Boolean := False;

   begin
      if GNAT.OS_Lib.Is_Regular_File (BF.Value) then
         Open (File, Mode => In_File, Name => BF.Value);

         while not End_Of_File (File) loop
            declare
               Line : constant String := Get_Line (File);
            begin
               if Line (Line'First) = '[' then
                  Generated := Starts_With (Line, "[GENERATED ");
                  if Generated then
                     Gen_Src := Line = "[GENERATED SOURCE FILES]";
                  end if;

               elsif Generated then
                  Result.Append (Obj_Dir.Compose (Filename_Type (Line)));

                  if Gen_Src and then Self.Has_Languages then
                     for L of Self.Languages loop
                        declare
                           A : constant Project.Attribute.Object :=
                                 Self.Attribute
                                   (PRA.Naming.Body_Suffix,
                                    Attribute_Index.Create (L.Text));
                        begin
                           if A.Is_Defined
                             and then Ends_With (Line, A.Value.Text)
                           then
                              for E of Self.Source_Artifact_Extensions
                                (Language => +Name_Type (L.Text))
                              loop
                                 Result.Append
                                   (Obj_Dir.Compose
                                      (Filename_Type (Line & E)));
                              end loop;
                           end if;
                        end;
                     end loop;

                  elsif Obj_Ext /= ""
                    and then Ends_With (Line, String (Obj_Ext))
                  then
                     for E of Self.Object_Artifact_Extensions (Language) loop
                        Result.Append
                          (Obj_Dir.Compose
                             (Filename_Type
                                (Line (Line'First
                                       .. Line'Last - Obj_Ext'Length) & E)));
                     end loop;
                  end if;
               end if;
            end;
         end loop;

         Close (File);
         Result.Append (BF);
      end if;

      return Result;
   end Binder_Artifacts;

   -------------------
   -- Binder_Prefix --
   -------------------

   function Binder_Prefix
     (Self : Object; Language : Language_Id) return Filename_Optional
   is
      Index : constant Attribute_Index.Object :=
                Attribute_Index.Create (Value_Type (Name (Language)));
   begin
      return Filename_Optional
        (Self.Attribute (PRA.Binder.Prefix, Index).Value.Text);
   end Binder_Prefix;

   ---------------------
   -- Check_Attribute --
   ---------------------

   function Check_Attribute
     (Self   : Object;
      Name   : Q_Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index;
      Result : out Project.Attribute.Object) return Boolean is
   begin
      Result := Self.Attribute (Name, Index, At_Pos);
      return Result.Is_Defined;
   exception
      when Attribute_Error =>
         Result := Project.Attribute.Undefined;
         return False;
   end Check_Attribute;

   ------------------
   -- Check_Parent --
   ------------------

   function Check_Parent (Self : Object; Parent : out Object) return Boolean is
      use Ada.Strings;

      Ref  : constant Definition.Const_Ref := Definition.Get_RO (Self);
      Name : constant Name_Type := Ref.Trees.Project.Name;
      Dot  : constant Natural := Fixed.Index (String (Name), ".", Backward);
   begin
      if Dot > 0 then
         Parent := Ref.Imports (Name (Name'First .. Dot - 1));
         return True;
      end if;

      return False;
   end Check_Parent;

   --------------------------
   -- Clean_Attribute_List --
   --------------------------

   function Clean_Attribute_List
     (Self     : Object;
      Name     : Q_Attribute_Id;
      Language : Language_Id) return Containers.Value_Set
   is
      Index  : constant Attribute_Index.Object :=
                 (if Language = No_Language
                  then Attribute_Index.Undefined
                  else Attribute_Index.Create
                    (Value_Type (GPR2.Name (Language))));
      Attr   : constant Project.Attribute.Object :=
                 Self.Attribute ((PRP.Clean, Name.Attr), Index);
      Result : Containers.Value_Set;

   begin
      if Attr.Is_Defined then
         for Val of Attr.Values loop
            Result.Include (Val.Text);
         end loop;
      end if;

      return Result;
   end Clean_Attribute_List;

   -------------
   -- Closure --
   -------------

   function Closure
     (Self         : Object;
      Include_Self : Boolean := False) return GPR2.Project.View.Set.Object
   is
      Closure_Views : GPR2.Project.View.Set.Object;
   begin
      if Include_Self then
         Closure_Views.Insert (Self);
      end if;

      for V of Get_RO (Self).Closure loop
         Closure_Views.Insert (V);
      end loop;

      return Closure_Views;
   end Closure;

   ------------------------
   -- Compilation_Inputs --
   ------------------------

   function Compilation_Inputs (Self : Object)
                                return GPR2.Build.Compilation_Input.Sets.Object
   is
   begin
      return GPR2.Build.Compilation_Input.Sets.Create (Self);
   end Compilation_Inputs;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is
   begin
      return Definition.Get_Context (Self);
   end Context;

   function Context (Self : Object) return Context_Kind is
   begin
      return Definition.Get_RO (Self).Context;
   end Context;

   ----------------
   -- Executable --
   ----------------

   function Executable
     (Self   : Object;
      Source : Simple_Name;
      At_Pos : Unit_Index) return GPR2.Path_Name.Object
   is
      BN       : constant  Value_Not_Empty :=
                   Remove_Body_Suffix (Self, Source);
      BN_Index : constant Attribute_Index.Object :=
                   Attribute_Index.Create (BN);
      Index    : constant Attribute_Index.Object :=
                   Attribute_Index.Create (Value_Not_Empty (Source));
      Attr     : GPR2.Project.Attribute.Object;

      function Executable
        (Base_Name : Value_Not_Empty) return GPR2.Path_Name.Object;
      --  Full executable path for base name

      ----------------
      -- Executable --
      ----------------

      function Executable
        (Base_Name : Value_Not_Empty) return GPR2.Path_Name.Object
      is
         Suffix : constant Value_Type :=
                    (if GNATCOLL.Utils.Ends_With
                       (Base_Name, String (Self.Executable_Suffix))
                     then ""
                     else Value_Type (Self.Executable_Suffix));
      begin
         return GPR2.Path_Name.Create_File
                  (Filename_Type (Base_Name & Suffix),
                   Filename_Optional (Self.Executable_Directory.Dir_Name));
      end Executable;

   begin
      if (Self.Check_Attribute (PRA.Builder.Executable, Index, At_Pos, Attr)
          or else
            (Source /= Simple_Name (BN)
             and then Self.Check_Attribute
               (PRA.Builder.Executable,
                BN_Index,
                At_Pos,
                Attr)))
        and then At_Pos = At_Pos_Or (Attr.Index, 0)
      then
         return Executable (Attr.Value.Text);
      else
         return Executable (BN);
      end if;
   end Executable;

   --------------------------
   -- Executable_Directory --
   --------------------------

   function Executable_Directory
     (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Apply_Root_And_Subdirs (PRA.Exec_Dir);
   end Executable_Directory;

   -----------------------
   -- Executable_Suffix --
   -----------------------

   function Executable_Suffix (Self : Object) return Filename_Optional is
   begin
      return Filename_Optional
        (Self.Attribute (PRA.Builder.Executable_Suffix).Value.Text);
   end Executable_Suffix;

   -----------------
   -- Executables --
   -----------------

   function Executables (Self : Object) return GPR2.Path_Name.Set.Object is
      Attr : constant Project.Attribute.Object := Self.Attribute (PRA.Main);
   begin
      return Set : GPR2.Path_Name.Set.Object do
         if Attr.Is_Defined then
            for Main of Attr.Values loop
               Set.Append (Self.Executable (Simple_Name (Main.Text),
                           At_Pos_Or (Main, 0)));
            end loop;
         end if;
      end return;
   end Executables;

   --------------
   -- Extended --
   --------------

   function Extended (Self : Object) return Set.Object is
   begin
      return Definition.Get_RO (Self).Extended;
   end Extended;

   -------------------
   -- Extended_Root --
   -------------------

   function Extended_Root (Self : Object) return Object is
   begin
      return Definition.Get_RO (Self).Extended_Root;
   end Extended_Root;

   ---------------
   -- Extending --
   ---------------

   function Extending (Self : Object) return Object is
   begin
      return Definition.Strong (Definition.Get_RO (Self).Extending);
   end Extending;

   ---------------------------
   -- Has_Aggregate_Context --
   ---------------------------

   function Has_Aggregate_Context (Self : Object) return Boolean is
   begin
      return Definition.Get_RO (Self).Context = Aggregate;
   end Has_Aggregate_Context;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute
     (Self   : Object;
      Name   : Q_Attribute_Id;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index) return Boolean is
   begin
      return Self.Attribute (Name, Index, At_Pos).Is_Defined;
   exception
      when Attribute_Error =>
         return False;
   end Has_Attribute;

   -----------------
   -- Has_Context --
   -----------------

   function Has_Context (Self : Object) return Boolean is
   begin
      return not Definition.Get_Context (Self).Is_Empty;
   end Has_Context;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Self : Object) return Boolean is
   begin
      return not Definition.Get_RO (Self).Trees.Imports.Is_Empty;
   end Has_Imports;

   ------------------
   -- Has_Language --
   ------------------

   function Has_Language (Self : Object; Name : Name_Type) return Boolean is
      Lang_Attr : GPR2.Project.Attribute.Object;
   begin
      Lang_Attr := Self.Attribute (PRA.Languages);

      if Lang_Attr.Is_Defined then
         for Val of Lang_Attr.Values loop
            if Name_Type (Val.Text) = Name then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Has_Language;

   function Has_Language (Self : Object; Name : Language_Id) return Boolean is
   begin
      return Self.Has_Language (GPR2.Name (Name));
   end Has_Language;

   -------------------
   -- Has_Languages --
   -------------------

   function Has_Languages (Self : Object) return Boolean is
   begin
      return Self.Has_Attribute (PRA.Languages);
   end Has_Languages;

   ---------------
   -- Has_Mains --
   ---------------

   function Has_Mains (Self : Object) return Boolean is
      Attr : constant Project.Attribute.Object := Self.Attribute (PRA.Main);
   begin
      if not Attr.Is_Defined then
         return False;
      else
         return Attr.Count_Values > 0;
      end if;
   end Has_Mains;

   -----------------
   -- Has_Package --
   -----------------

   function Has_Package
     (Self           : Object;
      Name           : Package_Id;
      Check_Extended : Boolean := True;
      With_Defaults  : Boolean := True;
      With_Config    : Boolean := True) return Boolean
   is
      View        : Object := Self;
      Def         : GPR2.Project.Registry.Attribute.Default_Rules;
      Has_Default : Boolean := False;

      procedure For_Rule (Attribute : Q_Attribute_Id; Definition : PRA.Def);
      --  Check if the definition applies to Name in Self's context

      --------------
      -- For_Rule --
      --------------

      procedure For_Rule (Attribute : Q_Attribute_Id; Definition : PRA.Def) is
         pragma Unreferenced (Attribute);
      begin
         if not Has_Default
           and then Definition.Is_Allowed_In (Self.Kind)
           and then Definition.Has_Default_In (Self.Kind)
         then
            Has_Default := True;
         end if;
      end For_Rule;

   begin
      if Definition.Get_RO (Self).Has_Packages (Name) then
         return True;
      end if;

      if With_Config
        and then Self.Tree.Has_Configuration
        and then Self.Tree.Configuration.Corresponding_View.
                   Get_RO.Has_Packages (Name)
      then
         return True;
      end if;

      --  Check if the package has default values

      if With_Defaults then
         Def := PRA.Get_Default_Rules (Name);

         --  Check if we can create a default value for package Name

         PRA.For_Each_Default (Def, For_Rule'Access);

         if Has_Default then
            return True;
         end if;
      end if;

      --  Finally, check extended

      loop
         exit when not Check_Extended or else not View.Is_Extending;

         View := View.Extended_Root;

         if Definition.Get_RO (View).Has_Packages (Name) then
            return True;
         end if;
      end loop;

      --  Should also check configuration ???

      return False;
   end Has_Package;

   ----------------
   -- Has_Source --
   ----------------

   function Has_Source
     (Self : Object; Filename : GPR2.Simple_Name) return Boolean
   is
   begin
      if Self.Kind in With_Object_Dir_Kind then
         return Get_DB (Self).Has_Source (Filename);
      else
         return False;
      end if;
   end Has_Source;

   -----------------------------
   -- Has_Source_Subdirectory --
   -----------------------------

   function Has_Source_Subdirectory (Self : Object) return Boolean is
   begin
      return Self.Tree.all.Has_Src_Subdirs;
   end Has_Source_Subdirectory;

   -----------------
   -- Has_Sources --
   -----------------

   function Has_Sources (Self : Object) return Boolean is
   begin
      if Self.Kind in With_Object_Dir_Kind then
         return Get_DB (Self).Sources.Is_Empty;
      else
         return False;
      end if;
   end Has_Sources;

   ---------------
   -- Has_Types --
   ---------------

   function Has_Types
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean is
   begin
      return Definition.Get_RO (Self).Has_Types (Name);
   end Has_Types;

   -------------------
   -- Has_Variables --
   -------------------

   function Has_Variables
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean is
   begin
      if Name = No_Name then
         return not Definition.Get_RO (Self).Vars.Is_Empty;
      else
         return Definition.Get_RO (Self).Vars.Contains (Name);
      end if;
   end Has_Variables;

   function Has_Variables
     (Self : Object;
      Pack : Package_Id;
      Name : Optional_Name_Type := No_Name) return Boolean is
   begin
      if not Self.Has_Package (Pack,
                               With_Defaults => False,
                               With_Config   => False)
      then
         return False;
      end if;

      if Name = No_Name then
         return not Self.Pack (Pack).Vars.Is_Empty;
      else
         return Self.Pack (Pack).Vars.Contains (Name);
      end if;
   end Has_Variables;

   --------
   -- Id --
   --------

   function Id (Self : Object) return GPR2.View_Ids.View_Id is
   begin
      return Definition.Get_RO (Self).Unique_Id;
   end Id;

   -------------
   -- Imports --
   -------------

   function Imports
     (Self : Object; Recursive : Boolean := False) return Set.Object
   is
      Result : GPR2.Project.View.Set.Object;

      procedure Add (Self : Object);
      --  Add Self imported projects

      ---------
      -- Add --
      ---------

      procedure Add (Self : Object) is
         Position : Set.Set.Cursor;
         Inserted : Boolean;
      begin
         for Import of Definition.Get_RO (Self).Imports loop
            Result.Insert (Import, Position, Inserted);

            if Inserted and then Recursive then
               Add (Import);
            end if;
         end loop;
      end Add;

   begin
      Add (Self);
      return Result;
   end Imports;

   ------------------------
   -- Invalidate_Sources --
   ------------------------

   procedure Invalidate_Sources (Self : in out Object) is
   begin
      Definition.Get_RW (Self).Sources_Signature :=
        GPR2.Context.Default_Signature;
   end Invalidate_Sources;

   ------------------------------
   -- Is_Aggregated_In_Library --
   ------------------------------

   function Is_Aggregated_In_Library (Self : Object) return Boolean is
      Ref : constant Definition.Const_Ref := Definition.Get_RO (Self);
   begin
      return not Ref.Agg_Libraries.Is_Empty;
   end Is_Aggregated_In_Library;

   -----------------
   -- Is_Extended --
   -----------------

   function Is_Extended (Self : Object) return Boolean is
   begin
      return Definition.Get_RO (Self).Is_Extended;
   end Is_Extended;

   --------------------
   -- Is_Extended_By --
   --------------------

   function Is_Extended_By (Self : Object; View : Object) return Boolean is
   begin
      return View.Is_Extension_Of (Self);
   end Is_Extended_By;

   ------------------
   -- Is_Extending --
   ------------------

   function Is_Extending
     (Self : Object; Parent : Object'Class := Undefined) return Boolean
   is
      Def : constant Definition.Const_Ref := Definition.Get_RO (Self);
   begin
      if not Def.Extended_Root.Is_Defined then
         return False;
      end if;

      if not Parent.Is_Defined then
         return True;
      end if;

      for Ext of Definition.Get_RO (Self).Extended loop
         if Ext = Object (Parent) then
            return True;
         end if;

         if Ext.Is_Extending then
            if Is_Extending (Ext, Parent) then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Is_Extending;

   ----------------------
   -- Is_Extending_All --
   ----------------------

   function Is_Extending_All (Self : Object) return Boolean is
   begin
      return Definition.Get_RO (Self).Trees.Project.Is_Extending_All;
   end Is_Extending_All;

   ---------------------
   -- Is_Extension_Of --
   ---------------------

   function Is_Extension_Of (Self : Object; View : Object) return Boolean is
      use GPR2.View_Ids;
   begin
      if Self.Id = View.Id then
         return True;
      elsif not Self.Is_Extending then
         return False;
      else
         return View.Is_Extending (Self);
      end if;
   end Is_Extension_Of;

   -------------------------
   -- Is_Externally_Built --
   -------------------------

   function Is_Externally_Built (Self : Object) return Boolean is
      Attr : constant Project.Attribute.Object :=
               Self.Attribute (PRA.Externally_Built);
   begin
      return Attr.Is_Defined and then Attr.Value_Equal ("true");
   end Is_Externally_Built;

   -------------
   -- Is_Main --
   -------------

   function Is_Main
     (Self : Object; Source : Build.Source.Object) return Boolean
   is
      Path  : constant GPR2.Path_Name.Object := Source.Path_Name;
      Mains : constant Project.Attribute.Object := Self.Attribute (PRA.Main);
   begin
      return Mains.Is_Defined
        and then
          (Mains.Has_Value (Value_Type (Path.Base_Name))
           or else Mains.Has_Value (Value_Type (Path.Simple_Name)));
   end Is_Main;

   -----------------------
   -- Is_Namespace_Root --
   -----------------------

   function Is_Namespace_Root (Self : Object) return Boolean is
     (for some Id of Get_RO (Self).Root_Views => Self.Id = Id);

   ----------------
   -- Is_Runtime --
   ----------------

   function Is_Runtime (Self : Object) return Boolean is
      Tree : constant not null access Project.Tree.Object := Self.Tree;
   begin
      return Tree.Has_Runtime_Project and then Tree.Runtime_Project = Self;
   end Is_Runtime;

   -----------------------
   -- Is_Shared_Library --
   -----------------------

   function Is_Shared_Library (Self : Object) return Boolean is
      LK : constant Name_Type := Self.Library_Kind;
   begin
      return LK = "dynamic" or else LK = "relocatable";
   end Is_Shared_Library;

   -----------------------
   -- Is_Static_Library --
   -----------------------

   function Is_Static_Library (Self : Object) return Boolean is
      LK : constant Name_Type := Self.Library_Kind;
   begin
      return LK = "static" or else LK = "static-pic";
   end Is_Static_Library;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object) return Project_Kind is
   begin
      return Definition.Get_RO (Self).Kind;
   end Kind;

   ------------------
   -- Language_Ids --
   ------------------

   function Language_Ids (Self : Object) return Containers.Language_Set is
      Def : constant Definition.Ref := Get_Ref (Self);
   begin
      if Def.Kind in K_Standard | K_Library then
         if Def.Languages.Is_Empty then
            for Val of Self.Languages loop
               Def.Languages.Include (+Name_Type (Val.Text));
            end loop;
         end if;
      end if;

      return Def.Languages;
   end Language_Ids;

   ---------------
   -- Languages --
   ---------------

   function Languages (Self : Object) return Containers.Source_Value_List is
      Attr : constant GPR2.Project.Attribute.Object :=
               Self.Attribute (PRA.Languages);
   begin
      if Attr.Is_Defined then
         return Attr.Values;
      else
         return Containers.Source_Value_Type_List.Empty;
      end if;
   end Languages;

   ---------------------------
   -- Library_Ali_Directory --
   ---------------------------

   function Library_Ali_Directory
     (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Apply_Root_And_Subdirs (PRA.Library_Ali_Dir);
   end Library_Ali_Directory;

   -----------------------
   -- Library_Directory --
   -----------------------

   function Library_Directory (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Apply_Root_And_Subdirs (PRA.Library_Dir);
   end Library_Directory;

   ----------------------
   -- Library_Filename --
   ----------------------

   function Library_Filename (Self : Object) return GPR2.Path_Name.Object is
      File_Name : Unbounded_String;
   begin
      --  Library prefix

      Append (File_Name,
              Self.Attribute (PRA.Shared_Library_Prefix).Value.Text);

      --  Library name

      Append (File_Name,
              Self.Attribute (PRA.Library_Name).Value.Text);

      --  Library suffix

      if Self.Is_Static_Library then
         Append (File_Name, String (Self.Tree.Archive_Suffix));

      else
         Append
           (File_Name,
            String (Self.Attribute (PRA.Shared_Library_Suffix).Value.Text));
      end if;

      return GPR2.Path_Name.Create_File
        (Filename_Type (To_String (File_Name)),
         Directory => Filename_Optional (Self.Library_Directory.Dir_Name));
   end Library_Filename;

   ------------------
   -- Library_Kind --
   ------------------

   function Library_Kind (Self : Object) return Name_Type is
      Attr : constant Project.Attribute.Object :=
               Self.Attribute (PRA.Library_Kind);
   begin
      return Name_Type (Attr.Value.Text);
   end Library_Kind;

   --------------------------------
   -- Library_Major_Version_Name --
   --------------------------------

   function Library_Major_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
   is

      function Major_Version_Name
        (Lib_Version : Filename_Type) return Filename_Type;
      --  Returns the major version name

      ------------------------
      -- Major_Version_Name --
      ------------------------

      function Major_Version_Name
        (Lib_Version : Filename_Type) return Filename_Type is
      begin
         for J in reverse Lib_Version'Range loop
            if Lib_Version (J) = '.' then
               return Lib_Version (Lib_Version'First .. J - 1);
            end if;
         end loop;

         --  Impossible if project view was validated just after parse

         raise Program_Error with "cannot get major version";
      end Major_Version_Name;

      LV : constant Project.Attribute.Object :=
             Self.Attribute (PRA.Library_Version);

   begin
      return GPR2.Path_Name.Create_File
        (Major_Version_Name (Filename_Type (LV.Value.Text)),
         Directory => Filename_Optional (Self.Library_Filename.Dir_Name));
   end Library_Major_Version_Filename;

   ---------------------------
   -- Library_Src_Directory --
   ---------------------------

   function Library_Src_Directory
     (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Apply_Root_And_Subdirs (PRA.Library_Src_Dir);
   end Library_Src_Directory;

   ------------------------
   -- Library_Standalone --
   ------------------------

   function Library_Standalone
     (Self : Object) return Standalone_Library_Kind
   is
      Attr : constant Project.Attribute.Object :=
        Self.Attribute (PRA.Library_Standalone);
   begin
      return Standalone_Library_Kind'Value (Attr.Value.Text);
   end Library_Standalone;

   ------------------------------
   -- Library_Version_Filename --
   ------------------------------

   function Library_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object is
   begin
      return GPR2.Path_Name.Create_File
        (Filename_Type (Self.Attribute (PRA.Library_Version).Value.Text),
         Directory => Filename_Optional (Self.Library_Directory.Dir_Name));
   end Library_Version_Filename;

   ---------------------
   -- Limited_Imports --
   ---------------------

   function Limited_Imports
     (Self : Object; Recursive : Boolean := False) return Set.Object
   is
      Result : GPR2.Project.View.Set.Object;

      procedure Add (Self : Object);
      --  Add Self imported projects

      ---------
      -- Add --
      ---------

      procedure Add (Self : Object) is
         Position : Set.Set.Cursor;
         Inserted : Boolean;
      begin
         for Import of Definition.Get_RO (Self).Limited_Imports loop
            Result.Insert (Import, Position, Inserted);

            if Inserted and then Recursive then
               Add (Import);
            end if;
         end loop;
      end Add;

   begin
      Add (Self);
      return Result;
   end Limited_Imports;

   ----------
   -- Main --
   ----------

   function Main
     (Self       : Object;
      Executable : Simple_Name) return Build.Compilation_Unit.Unit_Location
   is
      Src  : GPR2.Build.View_Db.Source_Context;
   begin
      --  Check executable attribute
      for Attr of Self.Attributes (Name => PRA.Builder.Executable) loop
         if Simple_Name (Attr.Value.Text) = Executable then
            Src :=
              Get_DB (Self).Visible_Source (Simple_Name (Attr.Index.Value));

            if Src.Source.Is_Defined then
               return
                 (Src.Owning_View.Id,
                  Src.Source.Path_Name,
                  Attr.Index.At_Pos);
            end if;
         end if;
      end loop;

      --  Try the Project'Main attributes
      for Value of Self.Attribute (PRA.Main).Values loop
         declare
            BN   : constant  Simple_Name :=
                     Simple_Name
                       (Remove_Body_Suffix (Self, Simple_Name (Value.Text)));
            Exec : constant Simple_Name := BN & Self.Executable_Suffix;
         begin
            if Exec = Executable or else BN = Executable then
               Src := Get_DB (Self).Visible_Source (Simple_Name (Value.Text));

               if Src.Source.Is_Defined then
                  return (Src.Owning_View.Id,
                          Src.Source.Path_Name,
                          Value.At_Pos);
               end if;
            end if;
         end;
      end loop;

      return Build.Compilation_Unit.No_Unit;
   end Main;

   -----------
   -- Mains --
   -----------

   function Mains
     (Self : Object)
      return GPR2.Build.Compilation_Unit.Unit_Location_Vector
   is
      Attr : constant Project.Attribute.Object := Self.Attribute (PRA.Main);
      Db   : constant Build.View_Db.Object := Get_DB (Self);
      Src  : GPR2.Build.View_Db.Source_Context;

   begin
      return Set : GPR2.Build.Compilation_Unit.Unit_Location_Vector do
         if Attr.Is_Defined then
            for Main of Attr.Values loop
               Src := Db.Visible_Source (Simple_Name (Main.Text));

               if Src.Source.Is_Defined then
                  Set.Append
                    ((Src.Owning_View.Id, Src.Source.Path_Name, Main.At_Pos));
               end if;
            end loop;
         end if;
      end return;
   end Mains;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Name_Type is
   begin
      return Definition.Get_RO (Self).Trees.Project.Name;
   end Name;

   --------------------
   -- Namespace_Root --
   --------------------

   function Namespace_Roots (Self : Object) return Set.Object is
   begin
      return Result : Set.Object do
         for Id of Get_RO (Self).Root_Views loop
            Result.Include (Self.Tree.Instance_Of (Id));
         end loop;
      end return;
   end Namespace_Roots;

   ----------------------
   -- Object_Directory --
   ----------------------

   function Object_Directory (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Apply_Root_And_Subdirs (PRA.Object_Dir);
   end Object_Directory;

   ----------
   -- Pack --
   ----------

   function Pack
     (Self : Object;
      Name : Package_Id) return Project.Pack.Object
   is
      View   : Object := Self;
      Cursor : Project.Pack.Set.Cursor;
   begin
      loop
         Cursor := Definition.Get_RO (View).Packs.Find (Name);

         if Project.Pack.Set.Has_Element (Cursor) then
            return Project.Pack.Set.Element (Cursor);
         end if;

         exit when not View.Is_Extending;

         View := View.Extended_Root;
      end loop;

      return Project.Pack.Object'
        (Source_Reference.Pack.Object
           (Source_Reference.Pack.Create (Source_Reference.Builtin, Name)) with
         Project.Attribute.Set.Empty_Set,
         Project.Variable.Set.Empty_Set);
   end Pack;

   --------------
   -- Packages --
   --------------

   function Packages
     (Self          : Object;
      With_Defaults : Boolean := True;
      With_Config   : Boolean := True) return GPR2.Containers.Package_Id_List
   is
      Result : Containers.Package_Id_List;
   begin
      if Self.Is_Extending then
         Result := Self.Extended_Root.Packages (With_Defaults => False);
      end if;

      for Pack of Definition.Get_RO (Self).Packs loop
         Result.Include (Pack.Id);
      end loop;

      if With_Config
        and then Self.Tree.Has_Configuration
      then
         for Pack of Definition.Get_RO
           (Self.Tree.Configuration.Corresponding_View).Packs
         loop
            Result.Include (Pack.Id);
         end loop;
      end if;

      --  Check packages with default values

      if With_Defaults then
         for Pack of PRA.Get_Packages_With_Default loop
            --  Self.Has_Packages will check if the default values defined in
            --  the package apply to Self.Kind.

            if not Result.Contains (Pack)
              and then Self.Has_Package (Pack,
                                         Check_Extended => False,
                                         With_Defaults  => True,
                                         With_Config    => False)
            then
               Result.Include (Pack);
            end if;
         end loop;
      end if;

      return Result;
   end Packages;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Definition.Get_RO (Self).Trees.Project.Path_Name;
   end Path_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Self : Object) return Project_Kind is
   begin
      return Definition.Get_RO (Self).Trees.Project.Qualifier;
   end Qualifier;

   ------------------------
   -- Remove_Body_Suffix --
   ------------------------

   function Remove_Body_Suffix
     (Self : Object; Name : Simple_Name) return Value_Not_Empty
   is
      Last     : Positive := Name'First;
      Src      : constant GPR2.Build.View_Db.Source_Context :=
                   Get_DB (Self).Visible_Source (Name);
      Lang     : constant Language_Id :=
                   (if Src.Source.Is_Defined
                    then Src.Source.Language
                    else No_Language);
      Suffix   : constant String :=
                   (if Lang /= No_Language
                    and then Src.Owning_View.Has_Body_Suffix (Lang)
                    then Src.Owning_View.Body_Suffix (Lang).Value.Text
                    else "");
   begin
      if Suffix'Length > 0
        and then Name'Length > Suffix'Length
        and then GPR2.Path_Name.To_OS_Case (Suffix) =
                   GPR2.Path_Name.To_OS_Case
                     (Strings.Fixed.Tail (String (Name), Suffix'Length))
      then
         Last := Name'Last - Suffix'Length;
      else
         while Last < Name'Last
           and then Name (Last + 1) /= '.'
         loop
            Last := Last + 1;
         end loop;
      end if;

      return Value_Not_Empty (Name (Name'First .. Last));
   end Remove_Body_Suffix;

   -------------
   -- Set_Def --
   -------------

   procedure Set_Def (Ref : out View.Object; Def : Definition_Base'Class) is
   begin
      Definition_References.Set (Ref, Def);
      pragma Assert (Ref.Get_Refcount = 1);
   end Set_Def;

   ---------------
   -- Signature --
   ---------------

   function Signature (Self : Object) return GPR2.Context.Binary_Signature is
   begin
      return Definition.Get_RO (Self).Signature;
   end Signature;

   ---------------------
   -- Skipped_Sources --
   ---------------------

   function Skipped_Sources
     (View : Project.View.Object) return Containers.Filename_Source_Reference
   is (Get_RO (View).Trees.Project.Skip_Sources);

   ------------
   -- Source --
   ------------

   function Source (Self : Object; Filename : GPR2.Simple_Name)
      return Build.Source.Object
   is
   begin
      if Self.Kind not in With_Object_Dir_Kind then
         return Build.Source.Undefined;
      end if;

      return Get_DB (Self).Source (Filename);
   end Source;

   ------------------------
   -- Source_Directories --
   ------------------------

   function Source_Directories
     (Self : Object) return GPR2.Path_Name.Set.Object
   is
      procedure Dir_Cb (Dir_Name : GPR2.Path_Name.Object);

      Result                   : GPR2.Path_Name.Set.Object;

      procedure Dir_Cb (Dir_Name : GPR2.Path_Name.Object) is
      begin
         Result.Append (Dir_Name);
      end Dir_Cb;

   begin
      Self.Source_Directories_Walk
        (Source_CB => null,
         Dir_CB    => Dir_Cb'Unrestricted_Access);

      return Result;
   end Source_Directories;

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
      Visited_Dirs               : GPR2.Containers.Filename_Set;
      Dir_Ref                    : GPR2.Source_Reference.Value.Object;
      Ignored_Sub_Dirs           : constant GPR2.Project.Attribute.Object :=
                                   View.Attribute (PRA.Ignore_Source_Sub_Dirs);
      Ignored_Sub_Dirs_Regexps   : Regexp_List.Vector;
      Excluded_Dirs              : constant GPR2.Project.Attribute.Object :=
                                     View.Attribute (PRA.Excluded_Source_Dirs);
      Excluded_Dirs_List         : GPR2.Path_Name.Set.Object;
      Excluded_Recurse_Dirs_List : GPR2.Path_Name.Set.Object;
      --  Ignore_Source_Sub_Dirs attribute values. In case the directory ends
      --  with a recursive indication "**", the dir is placed in
      --  Excluded_Recursive_Dirs_List.

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

         elsif Excluded_Recurse_Dirs_List.Contains (Directory) then
            --  Do not visit directory and subdirectories

            Do_Dir_Visit := False;
            Do_Subdir_Visit := False;

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
            declare
               Val          : constant Value_Type := V.Text;
               Recursive    : constant Boolean :=
                                Val'Length >= 2
                                    and then
                                      Val (Val'Last - 1 .. Val'Last) = "**";
               Last         : constant Natural :=
                                (if Recursive then Val'Last - 2 else Val'Last);
               Dir_Val      : constant Value_Type := Val (Val'First .. Last);
            begin
               if Dir_Val'Length = 0 then
                  if Recursive then
                     Excluded_Recurse_Dirs_List.Append (View.Dir_Name);
                  else
                     Excluded_Dirs_List.Append (View.Dir_Name);
                  end if;
               else
                  declare
                     Dir_Name     : constant GPR2.Path_Name.Object :=
                                      GPR2.Path_Name.Create_Directory
                                        (Filename_Type (Dir_Val),
                                         View.Dir_Name.Name);
                     Relative_Dir : constant Filename_Type :=
                                      Dir_Name.Relative_Path
                                        (From => View.Dir_Name).Name;
                  begin
                     if Recursive then
                        Excluded_Recurse_Dirs_List.Append
                          (View.Dir_Name.Compose (Relative_Dir, True));
                     else
                        Excluded_Dirs_List.Append
                          (View.Dir_Name.Compose (Relative_Dir, True));
                     end if;
                  end;
               end if;
            end;
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
            Definition.Foreach
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

   -------------------
   -- Source_Filter --
   -------------------

   function Source_Filter
     (S    : Build.Source.Object;
      Data : Build.Source.Sets.Filter_Data'Class) return Boolean
   is
      CU     : Build.Compilation_Unit.Object;
      Result : Boolean;
      Opt    : constant Source_Filter_Data := Source_Filter_Data (Data);
      use type Build.Unit_Kind;

   begin
      if Opt.Compilable_Only then
         if not S.Is_Compilable then
            return False;
         end if;

         --  Further check if the source can be input of gcc

         if S.Has_Units then
            Result := False;

            Unit_Loop :
            for Unit of S.Units loop
               if Unit.Kind = Build.S_Body then
                  Result := True;

                  exit Unit_Loop;

               elsif Unit.Kind = Build.S_Spec then
                  for NS of Opt.View.Namespace_Roots loop
                     CU := NS.Unit (Unit.Unit_Name);

                     if not CU.Has_Part (Build.S_Body) then
                        Result := True;

                        exit Unit_Loop;
                     end if;
                  end loop;
               end if;
            end loop Unit_Loop;

            if not Result then
               return False;
            end if;

         elsif S.Kind /= Build.S_Body then
            return False;
         end if;
      end if;

      if Opt.Interface_Only then
         --  ??? TODO
         null;
      end if;

      return True;
   end Source_Filter;

   -----------------
   -- Source_Path --
   -----------------

   function Source_Path
     (Self : Object; Filename : GPR2.Simple_Name) return GPR2.Path_Name.Object
   is
      Src : constant GPR2.Build.Source.Object := Self.Source (Filename);
   begin
      if Src.Is_Defined then
         return Src.Path_Name;
      else
         return GPR2.Path_Name.Undefined;
      end if;
   end Source_Path;

   function Source_Path
     (Self            : Object;
      Name            : GPR2.Simple_Name;
      Allow_Spec_File : Boolean) return GPR2.Path_Name.Object
   is
      Src : GPR2.Build.Source.Object;
   begin
      Src := Self.Source (Name);

      if Src.Is_Defined then
         return Src.Path_Name;
      end if;

      for Language of Self.Languages loop
         declare
            L  : constant Language_Id := +Name_Type (Language.Text);
            BS : constant Value_Type :=
                   (if Self.Has_Body_Suffix (L)
                    then Self.Body_Suffix (L).Value.Text
                    else No_Value);
         begin
            if BS /= No_Value then
               Src := Self.Source (Name & Simple_Name (BS));

               if Src.Is_Defined then
                  return Src.Path_Name;
               end if;
            end if;

            if Allow_Spec_File then
               declare
                  SS : constant Value_Type :=
                         (if Self.Has_Spec_Suffix (L)
                          then Self.Spec_Suffix (L).Value.Text
                          else No_Value);
               begin
                  if SS /= No_Value then
                     Src := Self.Source (Name & Simple_Name (SS));

                     if Src.Is_Defined then
                        return Src.Path_Name;
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;

      return GPR2.Path_Name.Undefined;
   end Source_Path;

   -------------------------
   -- Source_Subdirectory --
   -------------------------

   function Source_Subdirectory (Self : Object) return GPR2.Path_Name.Object is
   begin
      --  First check for <obj>/<project.lowercase_name>-<src_subdirs>

      declare
         P : constant GPR2.Path_Name.Object :=
               Self.Object_Directory.Compose
                 (Filename_Type (To_Lower (Self.Name))
                  & "-" & Self.Tree.Src_Subdirs, Directory => True);
      begin
         if P.Exists then
            return P;
         end if;
      end;

      --  Then default to <obj>/<src_subdirs>

      return Self.Object_Directory.Compose
        (Self.Tree.Src_Subdirs, Directory => True);
   end Source_Subdirectory;

   -------------
   -- Sources --
   -------------

   function Sources
     (Self            : Object;
      Interface_Only  : Boolean := False;
      Compilable_Only : Boolean := False) return Build.Source.Sets.Object
   is
      Db     : constant Build.View_Db.Object := Get_DB (Self);
      F_Data : constant Source_Filter_Data :=
                 (View            => Self,
                  Interface_Only  => Interface_Only,
                  Compilable_Only => Compilable_Only);

   begin
      return Build.Source.Sets.Create
        (Db, Build.Source.Sets.Sorted, Source_Filter'Access, F_Data);
   end Sources;

   ------------
   -- Strong --
   ------------

   function Strong (Weak : Weak_Reference) return Object is
      Result : Object;
   begin
      Definition_References.Set (Result, Weak);
      return Result;
   end Strong;

   ----------
   -- Tree --
   ----------

   function Tree (Self : Object) return not null access Project.Tree.Object is
   begin
      return Definition.Get_RO (Self).Tree;
   end Tree;

   ---------
   -- Typ --
   ---------

   function Typ (Self : Object; Name : Name_Type) return Project.Typ.Object is
   begin
      return Definition.Get_RO (Self).Types (Name);
   end Typ;

   -----------
   -- Types --
   -----------

   function Types (Self : Object) return Project.Typ.Set.Object is
   begin
      return Definition.Get_RO (Self).Types;
   end Types;

   -----------
   -- Units --
   -----------

   function Unit (Self : Object;
                  Name : Name_Type) return Build.Compilation_Unit.Object
   is
      Db : constant Build.View_Db.Object := Get_DB (Self);
   begin
      if Db.Has_Compilation_Unit (Name) then
         return Db.Compilation_Unit (Name);
      else
         return Build.Compilation_Unit.Undefined;
      end if;
   end Unit;

   -----------
   -- Units --
   -----------

   function Units (Self : Object) return GPR2.Build.Compilation_Unit.Maps.Map
   is
   begin
      return Get_DB (Self).Compilation_Units;
   end Units;

   --------------
   -- Variable --
   --------------

   function Variable
     (Self : Object; Name : Name_Type) return Project.Variable.Object is
   begin
      return Definition.Get_RO (Self).Vars (Name);
   end Variable;

   function Variable (Self : Object;
                      Pack : Package_Id;
                      Name : Name_Type) return Project.Variable.Object is
   begin
      return Self.Pack (Pack).Vars.Element (Name);
   end Variable;

   ---------------
   -- Variables --
   ---------------

   function Variables (Self : Object) return Project.Variable.Set.Object is
   begin
      return Definition.Get_RO (Self).Vars;
   end Variables;

   function Variables
     (Self : Object; Pack : Package_Id) return Project.Variable.Set.Object is
   begin
      return Self.Pack (Pack).Vars;
   end Variables;

   --------------
   -- View_For --
   --------------

   function View_For (Self : Object; Name : Name_Type) return View.Object is
      Data : constant Definition.Const_Ref := Definition.Get_RO (Self);
      Dad  : Object := Data.Extended_Root;
      C    : Definition.Project_View_Store.Cursor;
   begin
      --  Lookup in the ancestors first

      while Dad.Is_Defined loop
         if Dad.Name = Name then
            return Dad;
         end if;

         Dad := Definition.Get_RO (Dad).Extended_Root;
      end loop;

      --  Lookup in the imported next

      C := Data.Imports.Find (Name);

      if Definition.Project_View_Store.Has_Element (C) then
         return Definition.Project_View_Store.Element (C);
      end if;

      --  Try configuration project

      declare
         CV : constant Project.View.Object :=
                (if Data.Tree.Has_Configuration
                 then Data.Tree.Configuration.Corresponding_View
                 else Project.View.Undefined);
      begin
         --  If not found let's check if it is the configuration or runtime
         --  project. Note that this means that any Runtime or Config user's
         --  project name will have precedence.

         if CV.Is_Defined and then CV.Name = Name then
            return CV;

         --  Try runtime project

         elsif Data.Tree.Has_Runtime_Project
           and then Data.Tree.Runtime_Project.Name = Name
         then
            return Data.Tree.Runtime_Project;
         end if;
      end;

      return Undefined;
   end View_For;

begin
   Definition.Get_RO   := Get_RO'Access;
   Definition.Get_RW   := Get_RW'Access;
   Definition.Get      := Get_Ref'Access;
   Definition.Set      := Set_Def'Access;
   Definition.Refcount := Refcount'Access;
   Definition.Weak     := Weak'Access;
   Definition.Strong   := Strong'Access;
end GPR2.Project.View;
