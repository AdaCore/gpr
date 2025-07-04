--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Fixed;

with GNAT.OS_Lib;
with GNAT.String_Split;
with GNATCOLL.Utils;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Compilation_Unit.Maps;
with GPR2.Build.Source_Base;
with GPR2.Build.Source.Sets;
with GPR2.Build.View_Db;
with GPR2.Message;
with GPR2.Project.Attribute_Cache;
with GPR2.Project.Tree;
with GPR2.Project.View.Set;
with GPR2.Project.View.Vector;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Pack;
with GPR2.Tree_Internal;
with GPR2.View_Internal;

package body GPR2.Project.View is

   use GNAT;
   use type GPR2.View_Ids.View_Id;

   package Regexp_List is new Ada.Containers.Indefinite_Vectors
     (Positive, GNAT.Regexp.Regexp, "=" => GNAT.Regexp."=");

   function Get_Ref (View : Object) return View_Internal.Ref is
     (View_Internal.Data (View.Get.Element.all)'Unchecked_Access);

   function Get_RO (View : Object) return View_Internal.Const_Ref is
     (View_Internal.Data (View.Get.Element.all)'Unchecked_Access);

   function Get_RW (View : Object) return View_Internal.Ref is
     (View_Internal.Data (View.Get.Element.all)'Unchecked_Access);

   function Refcount (Self : Object) return Natural is
     (Definition_References.Get_Refcount (Self));
   --  Get view refcount

   function Tree_Int (Self : Object) return access GPR2.Tree_Internal.Object is
     (Get_RO (Self).Tree);

   function Tree (Self : Object) return GPR2.Project.Tree.Object is
     (Tree_Internal.Set (Get_RO (Self).Tree));

   function View_Db (Self : Object) return Build.View_Db.Object is
     (if Self.Tree_Int.Has_Artifacts_Database
      then Self.Tree_Int.Artifacts_Database.View_Database (Self.Id)
      else Build.View_Db.Undefined);

   procedure Set_Def (Ref : out View.Object; Def : View_Internal.Data);
   --  Convert definition to view

   function Weak (View : Object) return Weak_Reference is
     (Definition_References.Weak (View));

   function Strong (Weak : Weak_Reference) return Object;

   function Remove_Body_Suffix
     (Self : Object; Name : Simple_Name) return Value_Not_Empty;
   --  Remove body suffix from Name

   function Interface_Units
     (Self : Object) return GPR2.Containers.Unit_Name_To_Sloc.Map is
     (Get_RO (Self).Interface_Units);

   function Interface_Sources
     (Self : Object) return GPR2.Containers.Source_Path_To_Sloc.Map is
     (Get_RO (Self).Interface_Sources);

   function Attributes_Internal
     (Self          : Object;
      Name          : Q_Attribute_Id;
      With_Defaults : Boolean := True;
      With_Config   : Boolean := True)
      return Project.Attribute.Set.Object
   with Inline;

   function Has_Language (Self : Object; Name : Name_Type) return Boolean;
   --  Whether Name is a language used by Self

   function Library_Filename_Internal
     (Self : Object) return Simple_Name;
   --  The library filename computed from the various library prefixes and
   --  suffixes. Library_Version is ignored here.

   type Source_Filter_Data is new Build.Source.Sets.Filter_Data with record
      View            : Object;
      Interface_Only  : Boolean := False;
      Compilable_Only : Boolean := False;
   end record;
   --  Options used to filter a list of sources

   function Source_Filter
     (Self : Object;
      S    : Build.Source_Base.Object'Class;
      Data : Build.Source.Sets.Filter_Data'Class) return Boolean;
   --  Function used to filter the sources in the Sources subprogram

   -------------------------
   -- Aggregate_Libraries --
   -------------------------

   function Aggregate_Libraries (Self : Object) return Set.Object is
      Result : Set.Object;
   begin
      for Id of View_Internal.Get_RO (Self).Agg_Libraries loop
         Result.Include (Self.Tree_Int.Instance_Of (Id));
      end loop;

      return Result;
   end Aggregate_Libraries;

   ----------------
   -- Aggregated --
   ----------------

   function Aggregated
     (Self      : Object;
      Recursive : Boolean := True) return Set.Object
   is
      Analyzed : Set.Object;
   begin
      return Set : GPR2.Project.View.Set.Object do
         Analyzed.Insert (Self);

         for Agg of View_Internal.Get_RO (Self).Aggregated loop
            if Agg.Kind /= Self.Kind or else not Recursive then
               Set.Insert (Agg);
            elsif not Analyzed.Contains (Agg) then
               Analyzed.Insert (Agg);
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
         Subdirs  : constant Filename_Optional :=
                      (if Self.Is_Externally_Built then ""
                       else Self.Tree_Int.Subdirs);
         Dir_Name : constant Filename_Type :=
                      (if Dir = "" then "." else Filename_Type (Dir));
         Root     : GPR2.Path_Name.Object;
         Result   : GPR2.Path_Name.Object;

      begin
         if OS_Lib.Is_Absolute_Path (Dir) then
            Result := GPR2.Path_Name.Create_Directory (Dir_Name);

         elsif Self.Tree_Int.Build_Path.Is_Defined
           and then not Self.Is_Externally_Built
         then
            if Self.Tree_Int.Root_Path.Is_Defined then
               Root := Self.Tree_Int.Root_Path;
            else
               Root := Self.Tree_Int.Root_Project.Dir_Name;
            end if;

            Result := GPR2.Path_Name.Create_Directory
              (Self.Dir_Name.Relative_Path (Root),
               Self.Tree_Int.Build_Path.Value);

            Result := GPR2.Path_Name.Create_Directory
              (Dir_Name, Result.Value);

         else
            Result := GPR2.Path_Name.Create_Directory
              (Dir_Name, Self.Dir_Name.Value);
         end if;

         if Subdirs = No_Filename then
            return Result;
         end if;

         return GPR2.Path_Name.Create_Directory
           (Subdirs, Result.Value);
      end Compute;

      Def      : View_Internal.Ref renames Self.Get_Ref;
      Def_Attr : View_Internal.Cacheable_Dir_Attrs;

   begin
      if Dir_Attr = PRA.Object_Dir then
         Def_Attr := View_Internal.Object_Dir;
      elsif Dir_Attr = PRA.Library_Ali_Dir then
         Def_Attr := View_Internal.Library_Ali_Dir;
      elsif Dir_Attr = PRA.Library_Dir then
         Def_Attr := View_Internal.Library_Dir;
      elsif Dir_Attr = PRA.Exec_Dir then
         Def_Attr := View_Internal.Exec_Dir;
      elsif Dir_Attr = PRA.Library_Src_Dir then
         Def_Attr := View_Internal.Library_Src_Dir;
      end if;

      if not Def.Dir_Cache (Def_Attr).Is_Set then
         Def.Dir_Cache (Def_Attr) :=
           (Is_Set => True,
            Value  => Compute);
      end if;

      return Def.Dir_Cache (Def_Attr).Value;
   end Apply_Root_And_Subdirs;

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
         function Get_Pack return Pack_Internal.Set.Cursor with Inline;
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
               Cursor : Pack_Internal.Set.Cursor renames Get_Pack;
            begin
               if Pack_Internal.Set.Has_Element (Cursor) then
                  return Pack_Internal.Set.Element (Cursor).Attrs;
               else
                  return Project.Attribute.Set.Empty_Set;
               end if;
            end;
         end Get_Attrs;

         --------------
         -- Get_Pack --
         --------------

         function Get_Pack return Pack_Internal.Set.Cursor is
            Def    : View_Internal.Const_Ref := View.Get_RO;
            Result : Pack_Internal.Set.Cursor;
         begin
            loop
               Result := Def.Packs.Find (Name.Pack);
               if Pack_Internal.Set.Has_Element (Result) then
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
                  Result := Attribute (View, Name, PAI.Create (Src.Language));
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
               if Self.Tree_Int.Target_From_Command_Line /= "all"
                 and then Self.Tree_Int.Target_From_Command_Line /= ""
               then
                  return Value_Type
                    (Self.Tree_Int.Target_From_Command_Line
                       (Normalized => Name = PRA.Canonical_Target));
               end if;

            elsif Name = PRA.Runtime
              and then Index.Is_Defined
            then
               declare
                  Lang : constant Language_Id :=
                           +Optional_Name_Type (Index.Value);
               begin
                  return Value_Type
                      (Self.Tree_Int.Runtime_From_Command_Line (Lang));
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

         if Self.Tree_Int.Has_Configuration
           and then Self.Tree_Int.Configuration.Corresponding_View /= Self
         then
            Result := Get_Attribute_From_View
                        (Self.Tree_Int.Configuration.Corresponding_View);

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
                              (Self.Tree_Int.Configuration.Runtime
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
                       View_Internal.Get_RO (Self).Cache.Check_Cache
                         (Name   => Name,
                          Index  => Index,
                          At_Pos => At_Pos);
   begin
      if Project.Attribute_Cache.Has_Element (Cache_Cursor) then
         return Project.Attribute_Cache.Element (Cache_Cursor);
      end if;

      View_Internal.Get_RO (Self).Cache.Schedule_Update_Cache;

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

      if GPR2.Tree_Internal.Has_Configuration (Self.Tree_Int.all)
        and then Self.Tree_Int.Configuration.Corresponding_View /= Self
      then
         if not Found (Result) then
            --  If at this stage Result is not defined try to fetch the value
            --  from the configuration project.

            Result := Get_Attribute_From_View
              (View => Self.Tree_Int.Configuration.Corresponding_View);

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
                 (View => Self.Tree_Int.all.Configuration.Corresponding_View);

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
      View_Internal.Get_RO (Self).Cache.Update_Cache
         (Name   => Name,
          Index  => Index,
          At_Pos => At_Pos,
          Attr   => Result);
      return Result;
   end Attribute;

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
      use type GPR2.Project.Registry.Attribute.Value_Kind;

      use type PRA.Inherit_From_Extended_Type;
      use type PRA.Index_Value_Type;

      procedure Add_Attr (Attr   : Project.Attribute.Object;
                          Concat : Boolean);

      function Config return Object is
        (Self.Tree_Int.Configuration.Corresponding_View);
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

            pragma Annotate
              (XCov, Exempt_On,
               "Currently, no indexed attribute uses Concatenated as " &
                 "inheritance policy");

            declare
               New_Attr : Project.Attribute.Object :=
                            Project.Attribute.Set.Element (Cursor);
            begin
               New_Attr.Prepend_Vector (Attr);
               Result.Include (New_Attr);
            end;

            pragma Annotate (Xcov, Exempt_Off);
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

         --  Query extended views

         if Def.Inherit_From_Extended /= PRA.Not_Inherited
           and then Self.Is_Extending
         then
            pragma Annotate
              (Xcov, Exempt_On, "no inherited attribute has index, for now");

            for Attr of Self.Extended_Root.Attributes_Internal
              (Name, False, False)
            loop
               Add_Attr (Attr, Def.Inherit_From_Extended = PRA.Concatenated);
            end loop;

            pragma Annotate (Xcov, Exempt_Off);
         end if;

      else
         declare
            --  Self.Pack resolves inheritance
            Pack_Inst : Pack_Internal.Object renames Self.Pack (Name.Pack);
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
        and then Self.Tree_Int.Has_Configuration
      then
         for Attr of Config.Attributes_Internal (Name, False, False) loop
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
                  pragma Annotate
                    (Xcov, Exempt_On,
                     "No indexed attribute with D_Callback default");
                  null;
                  pragma Annotate (Xcov, Exempt_Off);

               when PRA.D_Attribute_Reference =>
                  pragma Annotate
                    (Xcov, Exempt_On,
                     "No indexed attribute with D_Attribute_Ref default");
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
                  pragma Annotate (Xcov, Exempt_Off);

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

      --  Filter out empty value lists
      if Def.Value = Project.Registry.Attribute.List then
         declare
            To_Remove : Project.Attribute.Set.Object;
         begin
            for Attr of Result loop
               if Attr.Values.Is_Empty then
                  To_Remove.Include (Attr);
               end if;
            end loop;

            for Attr of To_Remove loop
               Result.Delete (Attr);
            end loop;
         end;
      end if;

      return Result;
   end Attributes_Internal;

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

   -----------------
   -- Check_Mains --
   -----------------

   procedure Check_Mains
     (Self     : Object;
      Messages : in out Log.Object)
   is
      Attr                 : Project.Attribute.Object;
      Src                  : GPR2.Build.Source.Object;
      Found                : Boolean;
      Has_Ambiguous_Result : Boolean := False;

   begin
      Attr := Self.Attribute (PRA.Main);

      if not Attr.Is_Defined then
         return;
      end if;

      for Value of Attr.Values loop
         Found := False;

         Lang_Loop : for Lang of Self.Language_Ids loop
            declare
               Main      : constant Simple_Name :=
                             Suffixed_Simple_Name (Self, Value.Text, Lang);
               Db        : constant GPR2.Build.View_Db.Object := Self.View_Db;
               Ambiguous : Boolean;
            begin
               Src := Db.Visible_Source (Main, Ambiguous);

               if Src.Is_Defined and then not Ambiguous then
                  Found := True;
                  exit Lang_Loop;
               end if;

               Has_Ambiguous_Result := Has_Ambiguous_Result or Ambiguous;
            end;
         end loop Lang_Loop;

         if not Found then
            if Has_Ambiguous_Result then
               Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Message => "multiple sources were found for " &
                       String (Value.Text) &
                       " from project " & String (Self.Name),
                     Sloc    => Value));
            else
               Messages.Append
                 (Message.Create
                    (Level   => Message.Error,
                     Message => String (Value.Text) &
                       " is not a source of project " & String (Self.Name),
                     Sloc    => Value));
            end if;
         end if;
      end loop;
   end Check_Mains;

   ------------------
   -- Check_Parent --
   ------------------

   function Check_Parent (Self : Object; Parent : out Object) return Boolean is
      use Ada.Strings;

      Ref  : constant View_Internal.Const_Ref := View_Internal.Get_RO (Self);
      Name : constant Name_Type := Ref.Trees.Project.Name;
      Dot  : constant Natural := Fixed.Index (String (Name), ".", Backward);
   begin
      if Dot > 0 then
         declare
            P_Name : constant Name_Type := Name (Name'First .. Dot - 1);
         begin
            if Ref.Imports.Contains (P_Name) then
               Parent := Ref.Imports (P_Name);
               return True;
            end if;
         end;
      end if;

      return False;
   end Check_Parent;

   -------------
   -- Closure --
   -------------

   function Closure
     (Self               : Object;
      Include_Self       : Boolean := False;
      Include_Extended   : Boolean := False;
      Include_Aggregated : Boolean := False)
      return GPR2.Project.View.Vector.Object
   is
      procedure Add (V : GPR2.Project.View.Object);

      Closure_Views : GPR2.Project.View.Vector.Object;
      Todo          : GPR2.Project.View.Vector.Object;
      Done          : GPR2.Project.View.Set.Object;
      V             : GPR2.Project.View.Object;

      ---------
      -- Add --
      ---------

      procedure Add (V : GPR2.Project.View.Object) is
         C             : GPR2.Project.View.Set.Set.Cursor;
         Inserted      : Boolean;
      begin
         Done.Insert (V, C, Inserted);

         if Inserted then
            Todo.Append (V);
         end if;
      end Add;
   begin
      Add (Self);

      while not Todo.Is_Empty loop
         V := Todo.First_Element;
         Todo.Delete_First;

         if Include_Extended or else not V.Is_Extended then
            Closure_Views.Append (V);
         end if;

         for Imp of V.Imports loop
            Add (Imp);
         end loop;

         for Imp of V.Limited_Imports loop
            Add (Imp);
         end loop;

         if V.Is_Extending then
            --  Even when Included_Extended is set, we need to walk through
            --  them to retrieve their imports.

            for Ext of V.Extended loop
               Add (Ext);
            end loop;
         end if;

         if Include_Aggregated and then V.Kind = K_Aggregate_Library then
            for Agg of V.Aggregated loop
               Add (Agg);
            end loop;
         end if;
      end loop;

      if not Include_Self then
         Closure_Views.Delete_First;
      end if;

      return Closure_Views;
   end Closure;


   ---------------------
   -- Compiler_Prefix --
   ---------------------

   function Compiler_Prefix (Self : Object) return String is
   begin
      for Driver of Self.Attributes (Name => PRA.Compiler.Driver) loop
         if GNATCOLL.Utils.Ends_With (Driver.Value.Text, "gcc.exe")
           or else GNATCOLL.Utils.Ends_With (Driver.Value.Text, "gcc")
         then
            declare
               Basename : constant String :=
                 String
                   (GPR2.Path_Name.Base_Name
                      (Filename_Type (Driver.Value.Text)));
            begin
               return Basename (Basename'First .. Basename'Last - 3);
            end;
         end if;
      end loop;

      return "";
   end Compiler_Prefix;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is
   begin
      return View_Internal.Get_Context (Self);
   end Context;

   function Context (Self : Object) return Context_Kind is
   begin
      return View_Internal.Get_RO (Self).Context;
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
         return Self.Executable_Directory.Compose
                  (Filename_Type (Base_Name & Suffix));
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
               Set.Append
                 (Self.Executable
                    (Simple_Name (Main.Text),
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
      return View_Internal.Get_RO (Self).Extended;
   end Extended;

   -------------------
   -- Extended_Root --
   -------------------

   function Extended_Root (Self : Object) return Object is
   begin
      return View_Internal.Get_RO (Self).Extended_Root;
   end Extended_Root;

   ---------------
   -- Extending --
   ---------------

   function Extending (Self : Object) return Object is
   begin
      return View_Internal.Strong (View_Internal.Get_RO (Self).Extending);
   end Extending;

   -----------------------
   -- Filename_For_Unit --
   -----------------------

   function Filename_For_Unit
     (Self      : Object;
      Unit_Name : Name_Type;
      Kind      : Valid_Unit_Kind) return Simple_Name
   is
      package ACH renames Ada.Characters.Handling;

      SN : Ada.Strings.Unbounded.Unbounded_String;
   begin
      pragma Assert
        (Build.Compilation_Unit.Check_Name_Validity
           (Unit_Name,
            Source_Reference.Undefined,
            True,
            Self.Tree.Log_Messages.all),
         "invalid name for unit '" & String (Unit_Name) & "'");

      Naming_Exception : declare
         Unit_Value : constant Value_Type := Value_Type (Unit_Name);
         Has_NE     : constant Boolean :=
                        (if Kind = S_Spec
                         then Self.Has_Specification (Unit_Value)
                         else Self.Has_Implementation (Unit_Value));
      begin
         if Has_NE then
            declare
               NE : constant String :=
                      (if Kind = S_Spec
                       then Self.Specification (Unit_Value).Value.Text
                       else Self.Implementation (Unit_Value).Value.Text);
            begin
               return Simple_Name (NE);
            end;
         end if;
      end Naming_Exception;

      Append_Filename : declare
         Sub_Units : String_Split.Slice_Set;
         First     : Boolean := True;

         Dot_Repl  : constant String :=
                       Self.Attribute
                         (PRA.Naming.Dot_Replacement).Value.Text;

         Casing    : constant String :=
                       ACH.To_Lower (Self.Casing.Value.Text);
      begin
         Sub_Units := String_Split.Create
           (String (Unit_Name),
            (1 => '.'),
            String_Split.Multiple);

         for Unit of Sub_Units loop
            if not First then
               Append (SN, Dot_Repl);
            end if;

            declare
               Part_Name : constant String := (if Casing = "uppercase"
                                               then ACH.To_Upper (Unit)
                                               elsif Casing = "mixedcase"
                                               then To_Mixed (Unit)
                                               else ACH.To_Lower (Unit));
            begin
               Append (SN, Part_Name);
            end;

            First := False;
         end loop;
      end Append_Filename;

      Append_Suffix : declare
         Suffix : constant String :=
                    (if Kind = S_Body
                     then (if Self.Has_Body_Suffix (Ada_Language)
                       then Self.Body_Suffix (Ada_Language).Value.Text
                       else ".adb")
                     elsif Kind = S_Spec
                     then (if Self.Has_Spec_Suffix (Ada_Language)
                       then Self.Spec_Suffix (Ada_Language).Value.Text
                       else ".ads")
                     else (if Self.Has_Separate_Suffix
                       then Self.Separate_Suffix.Value.Text
                       else ".adb"));
      begin
         Append (SN, Suffix);
      end Append_Suffix;

      return -SN;
   end Filename_For_Unit;

   ---------------------------
   -- Has_Aggregate_Context --
   ---------------------------

   function Has_Aggregate_Context (Self : Object) return Boolean is
   begin
      return View_Internal.Get_RO (Self).Context = Aggregate;
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
      return not View_Internal.Get_Context (Self).Is_Empty;
   end Has_Context;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Self : Object) return Boolean is
   begin
      return not View_Internal.Get_RO (Self).Trees.Imports.Is_Empty;
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
      Attr      : constant Project.Attribute.Object :=
                    Self.Attribute (PRA.Main);
      Src       : GPR2.Build.Source.Object;
      Db        : GPR2.Build.View_Db.Object;
      Ambiguous : Boolean := False;

   begin
      if Self.Kind in K_Standard
        and then Self.Is_Namespace_Root
        and then (Attr.Is_Defined and then Attr.Count_Values > 0)
      then
         Db := Self.View_Db;

         for Value of Attr.Values loop
            for Lang of Self.Language_Ids loop
               if Attr.Is_Defined then
                  declare
                     Main : constant Simple_Name :=
                       Suffixed_Simple_Name
                         (Self, Value.Text, Lang);
                  begin
                     Src := Db.Visible_Source (Main, Ambiguous);

                     if Src.Is_Defined and then not Ambiguous then
                        return True;
                     end if;
                  end;
               end if;
            end loop;
         end loop;
      end if;

      return False;
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
      if View_Internal.Get_RO (Self).Has_Packages (Name) then
         return True;
      end if;

      if With_Config
        and then Self.Tree_Int.Has_Configuration
        and then Self.Tree_Int.Configuration.Corresponding_View.
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

         if View_Internal.Get_RO (View).Has_Packages (Name) then
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
      Db : Build.View_Db.Object;
   begin
      if Self.Kind in With_Object_Dir_Kind then
         Db := Self.View_Db;

         if Db.Is_Defined
           and then Db.Source_Option > No_Source
         then
            return Db.Has_Source (Filename);
         else
            return False;
         end if;

      else
         return False;
      end if;
   end Has_Source;

   -----------------------------
   -- Has_Source_Subdirectory --
   -----------------------------

   function Has_Source_Subdirectory (Self : Object) return Boolean is
   begin
      return Self.Tree_Int.all.Has_Src_Subdirs;
   end Has_Source_Subdirectory;

   ---------------
   -- Has_Types --
   ---------------

   function Has_Types
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean is
   begin
      return View_Internal.Get_RO (Self).Has_Types (Name);
   end Has_Types;

   -------------------
   -- Has_Variables --
   -------------------

   function Has_Variables
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean is
   begin
      if Name = No_Name then
         return not View_Internal.Get_RO (Self).Vars.Is_Empty;
      else
         return View_Internal.Get_RO (Self).Vars.Contains (Name);
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
      return View_Internal.Get_RO (Self).Unique_Id;
   end Id;

   -------------
   -- Imports --
   -------------

   function Imports
     (Self : Object; Recursive : Boolean := False) return Set.Object
   is
      procedure Add (Self : Object);
      --  Add Self imported projects

      Result : GPR2.Project.View.Set.Object;

      ---------
      -- Add --
      ---------

      procedure Add (Self : Object) is
         Position : Set.Set.Cursor;
         Inserted : Boolean;
      begin
         for Import of View_Internal.Get_RO (Self).Imports loop
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

   ------------------
   -- Include_Path --
   ------------------

   function Include_Path
     (Self : Object; Language : Language_Id) return GPR2.Path_Name.Set.Object
   is
      Result    : GPR2.Path_Name.Set.Object;
      Attr      : GPR2.Project.Attribute.Object;
      Languages : GPR2.Containers.Language_Set;
      Found     : Boolean := False;

   begin
      Languages.Include (Language);

      Attr := Self.Attribute (PRA.Inherit_Source_Path, PAI.Create (Language));

      if Attr.Is_Defined then
         for V of Attr.Values loop
            Languages.Include (+Name_Type (V.Text));
         end loop;
      end if;

      for V of Self.Closure (True, True, True) loop
         if V.Kind in With_Source_Dirs_Kind then
            --  Check that the view has some sources of the language

            Found := False;

            for L of V.Language_Ids loop
               if Languages.Contains (L) then
                  Found := True;

                  exit;
               end if;
            end loop;

            if Found then
               for Src_Dir of V.Source_Directories loop
                  if not Result.Contains (Src_Dir) then
                     Result.Append (Src_Dir);
                  end if;
               end loop;
            end if;
         end if;
      end loop;

      return Result;
   end Include_Path;

   -----------------------
   -- Interface_Closure --
   -----------------------

   function Interface_Closure
     (Self : Object) return GPR2.Build.Compilation_Unit.Maps.Map
   is
      CU : Build.Compilation_Unit.Object;
   begin
      return Result : GPR2.Build.Compilation_Unit.Maps.Map do
         if Self.Is_Library then
            for C in Self.Interface_Units.Iterate loop
               declare
                  U_Name : constant Name_Type :=
                            Containers.Unit_Name_To_Sloc.Key (C);
               begin
                  if Self.Kind = K_Aggregate_Library then
                     for V of Self.Aggregated loop
                        CU := V.Own_Unit (U_Name);
                        exit when CU.Is_Defined;
                     end loop;

                  else
                     CU := Self.Own_Unit (U_Name);
                  end if;

                  --  ??? Handle properly the error case

                  pragma Assert (CU.Is_Defined);

                  Result.Insert (U_Name, CU);
               end;
            end loop;
         end if;

         for C in Self.Interface_Sources.Iterate loop
            declare
               BN     : constant Filename_Type :=
                          Containers.Source_Path_To_Sloc.Key (C);
               Src    : constant GPR2.Build.Source.Object :=
                          (if Self.Kind = K_Aggregate_Library
                           then Self.Visible_Source (BN)
                           else Self.Source (BN));

            begin
               if Src.Has_Units then
                  for U of Src.Units loop
                     if Self.Kind = K_Aggregate_Library then
                        for V of Self.Aggregated loop
                           CU := V.Own_Unit (U.Name);
                           exit when CU.Is_Defined;
                        end loop;

                     else
                        CU := Self.Own_Unit (U.Name);
                     end if;

                     pragma Assert (CU.Is_Defined);

                     Result.Include (U.Name, CU);
                  end loop;
               end if;
            end;
         end loop;
      end return;
   end Interface_Closure;

   ------------------------------
   -- Is_Aggregated_In_Library --
   ------------------------------

   function Is_Aggregated_In_Library (Self : Object) return Boolean is
      Ref : constant View_Internal.Const_Ref := View_Internal.Get_RO (Self);
   begin
      return not Ref.Agg_Libraries.Is_Empty;
   end Is_Aggregated_In_Library;

   -------------------
   -- Is_Compilable --
   -------------------

   function Is_Compilable
     (Self : Object;
      Lang : Language_Id) return Boolean
   is
      Ref  : constant View_Internal.Ref := View_Internal.Get_RW (Self);
      C    : constant View_Internal.Lang_Boolean_Map.Cursor :=
               Ref.Compilable_Languages.Find (Lang);
      Attr : GPR2.Project.Attribute.Object;
      Res  : Boolean;
   begin
      if not View_Internal.Lang_Boolean_Map.Has_Element (C) then
         Attr := Self.Attribute
           (PRA.Compiler.Driver, Project.Attribute_Index.Create (Lang));
         Res := Attr.Is_Defined
           and then Length (Attr.Value.Unchecked_Text) > 0;

         Ref.Compilable_Languages.Insert (Lang, Res);

         return Res;

      else
         return View_Internal.Lang_Boolean_Map.Element (C);
      end if;
   end Is_Compilable;

   -----------------
   -- Is_Extended --
   -----------------

   function Is_Extended (Self : Object) return Boolean is
      Def : constant View_Internal.Const_Ref := View_Internal.Get_RO (Self);
      use type View_Internal.Const_Ref;
      use View_Internal;
   begin
      --  Note: need to use the low level values here, else pre-conditions
      --  lead to infinite recursion.
      return Def.Is_Extended
        and then
          (not Self.Is_Externally_Built
           or else Get_RO (Strong (Def.Extending)).Extended_Root = Self);
   end Is_Extended;

   ------------------
   -- Is_Extending --
   ------------------

   function Is_Extending
     (Self : Object; Parent : Object'Class := Undefined) return Boolean
   is
      Def : constant View_Internal.Const_Ref := View_Internal.Get_RO (Self);
   begin
      if not Def.Extended_Root.Is_Defined then
         return False;
      end if;

      if not Parent.Is_Defined then
         return True;
      end if;

      for Ext of Def.Extended loop
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
      return View_Internal.Get_RO (Self).Trees.Project.Is_Extending_All;
   end Is_Extending_All;

   -------------------------
   -- Is_Externally_Built --
   -------------------------

   function Is_Externally_Built (Self : Object) return Boolean is
      Attr : constant Project.Attribute.Object :=
               Self.Attribute (PRA.Externally_Built);
   begin
      return Attr.Is_Defined and then Attr.Value_Equal ("true");
   end Is_Externally_Built;

   -----------------------
   -- Is_Namespace_Root --
   -----------------------

   function Is_Namespace_Root (Self : Object) return Boolean is
     (for some Id of Get_RO (Self).Root_Views => Self.Id = Id);

   ----------------
   -- Is_Runtime --
   ----------------

   function Is_Runtime (Self : Object) return Boolean is
   begin
      return Self.Id = View_Ids.Runtime_View_Id;
   end Is_Runtime;

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
      return View_Internal.Get_RO (Self).Kind;
   end Kind;

   ------------------
   -- Language_Ids --
   ------------------

   function Language_Ids (Self : Object) return Containers.Language_Set is
      Def : constant View_Internal.Ref := Get_Ref (Self);
   begin
      if Def.Kind in K_Standard | K_Library then
         if Def.Languages.Is_Empty then
            for Val of Self.Languages loop
               Def.Languages.Include (+Name_Type (Val.Text));
            end loop;
         end if;
      elsif Def.Kind = K_Aggregate_Library then
         if Def.Languages.Is_Empty then
            for V of Self.Aggregated loop
               Def.Languages.Union (V.Language_Ids);
            end loop;
         end if;
      end if;

      return Def.Languages;
   end Language_Ids;

   ---------------
   -- Languages --
   ---------------

   function Languages (Self : Object) return Containers.Source_Value_List is
      Attr : constant Project.Attribute.Object :=
               Self.Attribute (PRA.Languages);
   begin
      if Attr.Is_Defined then
         return Attr.Values;
      else
         return Containers.Source_Value_Type_List.Empty_Vector;
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

   function Library_Filename
     (Self            : Object;
      Without_Version : Boolean := False) return GPR2.Path_Name.Object
   is
      function Get_Simple_Name return Simple_Name;

      ---------------------
      -- Get_Simple_Name --
      ---------------------

      function Get_Simple_Name return Simple_Name is
         Shared_Ext   : constant String :=
                          Self.Attribute
                            (PRA.Shared_Library_Suffix).Value.Text;
         SE_Len       : constant Positive := Shared_Ext'Length;
         Attr_Version : GPR2.Project.Attribute.Object;
      begin
         --  Attribute library_version is only available on unix for shared
         --  libraries.

         if not Self.Is_Static_Library and then Shared_Ext /= ".dll" then
            Attr_Version := Self.Attribute (PRA.Library_Version);

            if Attr_Version.Is_Defined then
               declare
                  V : constant String := Attr_Version.Value.Text;
               begin
                  if Without_Version then
                     --  Remove the version part
                     for J in V'Range loop
                        if V (J) = '.' then
                           --  The dot could separate the base name from
                           --  the shared library extension (.so, .dylib)
                           --  that we want to keep.
                           if J + SE_Len - 1 > V'Last
                             or else
                               V (J .. J + SE_Len - 1) /= Shared_Ext
                           then
                              return Simple_Name (V (V'First .. J - 1));
                           end if;
                        end if;
                     end loop;

                  else
                     return Simple_Name (V);
                  end if;
               end;
            end if;
         end if;

         return Self.Library_Filename_Internal;
      end Get_Simple_Name;

   begin
      return Self.Library_Directory.Compose (Get_Simple_Name);
   end Library_Filename;

   -------------------------------
   -- Library_Filename_Internal --
   -------------------------------

   function Library_Filename_Internal
     (Self : Object) return Simple_Name
   is
      File_Name    : Unbounded_String;
   begin
      --  Library prefix

      if not Self.Is_Static_Library then
         Append (File_Name,
                 Self.Attribute (PRA.Shared_Library_Prefix).Value.Text);
      else
         Append (File_Name,
                 Self.Attribute (PRA.Archive_Prefix).Value.Text);
      end if;

      --  Library name

      Append (File_Name,
              Self.Attribute (PRA.Library_Name).Value.Text);

      --  Library suffix

      if Self.Is_Static_Library then
         Append (File_Name, String (Self.Tree_Int.Archive_Suffix));

      else
         Append
           (File_Name,
            String (Self.Attribute (PRA.Shared_Library_Suffix).Value.Text));
      end if;

      return Simple_Name (To_String (File_Name));
   end Library_Filename_Internal;

   -------------------------------
   -- Library_Filename_Variants --
   -------------------------------

   function Library_Filename_Variants
     (Self : Object) return GPR2.Containers.Filename_Set
   is
      Result : Containers.Filename_Set;
      Attr_Version : GPR2.Project.Attribute.Object;
   begin
      --  Library version
      if not Self.Is_Static_Library
        and then not GPR2.On_Windows
      then
         pragma Warnings (Off, "this code can never be executed*");
         Attr_Version := Self.Attribute (PRA.Library_Version);
         pragma Warnings (On, "this code can never be executed*");
      end if;

      if not Attr_Version.Is_Defined then
         return Containers.Filename_Type_Set.Empty_Set;
      end if;

      declare
         Version : constant Simple_Name :=
                     Simple_Name (Attr_Version.Value.Text);
         Last    : Natural := Version'Last;
      begin
         for K in reverse Version'First .. Version'Last loop
            if Version (K) = '.' then
               exit when
                 Version (K .. Last) =
                   Simple_Name
                     (Self.Attribute (PRA.Shared_Library_Suffix).Value.Text);
               Result.Include (Version (Version'First .. K - 1));
               Last := K - 1;
            end if;
         end loop;
      end;

      return Result;
   end Library_Filename_Variants;

   ------------------
   -- Library_Kind --
   ------------------

   function Library_Kind (Self : Object) return Name_Type is
      Attr : constant Project.Attribute.Object :=
               Self.Attribute (PRA.Library_Kind);
   begin
      return Name_Type (Attr.Value.Text);
   end Library_Kind;

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
      return Self.Library_Directory.Compose
        ((if On_Windows
         then Self.Library_Filename_Internal
         else Simple_Name (Self.Attribute (PRA.Library_Version).Value.Text)));
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
         for Import of View_Internal.Get_RO (Self).Limited_Imports loop
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
      Src       : GPR2.Build.Source.Object;
      Db        : constant GPR2.Build.View_Db.Object := Self.View_Db;
      Exc_BN    : constant Simple_Name :=
                    Simple_Name (Remove_Body_Suffix (Self, Executable));
      Ambiguous : Boolean;

   begin
      --  Check executable attribute

      for Attr of Self.Attributes (Name => PRA.Builder.Executable) loop
         if Simple_Name (Attr.Value.Text) = Executable
           or else Simple_Name (Attr.Value.Text) = Exc_BN
         then
            for Lang of Self.Language_Ids loop
               Src := Db.Visible_Source
                        (Suffixed_Simple_Name (Self, Attr.Index.Value, Lang),
                         Ambiguous);

               if Src.Is_Defined and then not Ambiguous then
                  return
                    (Src.Owning_View,
                     Src.Path_Name,
                     Attr.Index.At_Pos);
               end if;
            end loop;
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
               for Lang of Self.Language_Ids loop
                  Src := Db.Visible_Source
                    (Suffixed_Simple_Name (Self, Value.Text, Lang), Ambiguous);

                  if Src.Is_Defined and then not Ambiguous then
                     return (Src.Owning_View,
                             Src.Path_Name,
                             Value.At_Pos);
                  end if;
               end loop;
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
      Attr      : constant Project.Attribute.Object :=
                    Self.Attribute (PRA.Main);
      Db        : constant Build.View_Db.Object := Self.View_Db;
      Src       : GPR2.Build.Source.Object;
      Ambiguous : Boolean;

   begin
      return Set : GPR2.Build.Compilation_Unit.Unit_Location_Vector do
         if Attr.Is_Defined then
            for Value of Attr.Values loop
               Lang_Loop :
               for Lang of Self.Language_Ids loop
                  Src := Db.Visible_Source
                    (Suffixed_Simple_Name (Self, Value.Text, Lang),
                     Ambiguous);

                  if Src.Is_Defined and then not Ambiguous then
                     Set.Append
                       (Build.Compilation_Unit.Unit_Location'
                          (Src.Owning_View,
                           Src.Path_Name,
                           Value.At_Pos));
                     exit Lang_Loop;
                  end if;
               end loop Lang_Loop;
            end loop;
         end if;
      end return;
   end Mains;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Name_Type is
   begin
      return View_Internal.Get_RO (Self).Trees.Project.Name;
   end Name;

   --------------------
   -- Namespace_Root --
   --------------------

   function Namespace_Roots (Self : Object) return Set.Object is
      Result : Set.Object;
   begin
      for Id of Get_RO (Self).Root_Views loop
         Result.Include (Self.Tree_Int.Instance_Of (Id));
      end loop;

      return Result;
   end Namespace_Roots;

   ----------------------
   -- Object_Directory --
   ----------------------

   function Object_Directory (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Apply_Root_And_Subdirs (PRA.Object_Dir);
   end Object_Directory;

   --------------
   -- Own_Unit --
   --------------

   function Own_Unit
     (Self : Object; Name : Name_Type) return Build.Compilation_Unit.Object
   is
      Db : constant Build.View_Db.Object := Self.View_Db;
   begin
      if Self.Kind in With_Object_Dir_Kind then
         return Db.Own_Unit (Name);
      else
         return Build.Compilation_Unit.Undefined;
      end if;
   end Own_Unit;

   ---------------
   -- Own_Units --
   ---------------

   function Own_Units
     (Self : Object) return GPR2.Build.Compilation_Unit.Maps.Map
   is
      Db : Build.View_Db.Object;
   begin
      if Self.Kind in With_Object_Dir_Kind then
         Db := Self.View_Db;
         return Db.Own_Units;
      else
         return Build.Compilation_Unit.Maps.Empty_Map;
      end if;
   end Own_Units;

   ----------
   -- Pack --
   ----------

   function Pack
     (Self : Object;
      Name : Package_Id) return Pack_Internal.Object
   is
      View   : Object := Self;
      Cursor : Pack_Internal.Set.Cursor;
   begin
      loop
         Cursor := View_Internal.Get_RO (View).Packs.Find (Name);

         if Pack_Internal.Set.Has_Element (Cursor) then
            return Pack_Internal.Set.Element (Cursor);
         end if;

         exit when not View.Is_Extending;

         View := View.Extended_Root;
      end loop;

      return Pack_Internal.Object'
        (Source_Reference.Pack.Object
           (Source_Reference.Pack.Create (Source_Reference.Builtin, Name))
         with Project.Attribute.Set.Empty_Set, Project.Variable.Set.Empty_Set);
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

      for Pack of View_Internal.Get_RO (Self).Packs loop
         Result.Include (Pack.Id);
      end loop;

      if With_Config
        and then Self.Tree_Int.Has_Configuration
      then
         for Pack of View_Internal.Get_RO
           (Self.Tree_Int.Configuration.Corresponding_View).Packs
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
      return View_Internal.Get_RO (Self).Trees.Project.Path_Name;
   end Path_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Self : Object) return Project_Kind is
   begin
      return View_Internal.Get_RO (Self).Trees.Project.Qualifier;
   end Qualifier;

   ------------------------
   -- Remove_Body_Suffix --
   ------------------------

   function Remove_Body_Suffix
     (Self : Object; Name : Simple_Name) return Value_Not_Empty
   is
      Last      : Positive := Name'First;
      Db        : constant GPR2.Build.View_Db.Object := Self.View_Db;
      Ambiguous : Boolean;
      Src       : constant GPR2.Build.Source.Object :=
                    Db.Visible_Source (Name, Ambiguous);
      Lang      : constant Language_Id :=
                    (if Src.Is_Defined
                     then Src.Language
                     else No_Language);
      Suffix    : constant Filename_Optional :=
                    (if Lang /= No_Language
                     and then Src.Owning_View.Has_Body_Suffix (Lang)
                     then Filename_Optional
                       (Src.Owning_View.Body_Suffix (Lang).Value.Text)
                     else "");
   begin
      if Suffix'Length > 0
        and then Name'Length > Suffix'Length
        and then GPR2.Path_Name.To_OS_Case (Suffix) =
                   GPR2.Path_Name.To_OS_Case
                     (Filename_Optional
                       (Strings.Fixed.Tail (String (Name), Suffix'Length)))
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

   procedure Set_Def (Ref : out View.Object; Def : View_Internal.Data) is
   begin
      Definition_References.Set (Ref, Def);
      pragma Assert (Ref.Get_Refcount = 1);
   end Set_Def;

   ---------------------
   -- Skipped_Sources --
   ---------------------

   function Skipped_Sources
     (Self : Object) return Containers.Filename_Source_Reference
   is (Get_RO (Self).Trees.Project.Skip_Sources);

   ------------
   -- Source --
   ------------

   function Source
     (Self : Object; Filename : GPR2.Simple_Name)
      return Build.Source.Object
   is
      Db : GPR2.Build.View_Db.Object;
   begin
      if Self.Kind not in With_Object_Dir_Kind then
         return Build.Source.Undefined;
      end if;

      Db := Self.View_Db;

      return Db.Source (Filename);
   end Source;

   ------------------------
   -- Source_Directories --
   ------------------------

   function Source_Directories
     (Self : Object) return GPR2.Path_Name.Set.Object
   is
      procedure Dir_Cb (Dir_Name : GPR2.Path_Name.Full_Name);

      Result : GPR2.Path_Name.Set.Object;
      Ign    : GPR2.Log.Object;

      ------------
      -- Dir_Cb --
      ------------

      procedure Dir_Cb (Dir_Name : GPR2.Path_Name.Full_Name) is
      begin
         Result.Append (GPR2.Path_Name.Create_Directory (Dir_Name));
      end Dir_Cb;

   begin
      --  Ignore at this stage messages about incorrect source directories,
      --  since those messages are reported when the sources are loaded.

      Self.Source_Directories_Walk
        (Source_CB => null,
         Dir_CB    => Dir_Cb'Unrestricted_Access,
         Messages  => Ign);

      return Result;
   end Source_Directories;

   -----------------------------
   -- Source_Directories_Walk --
   -----------------------------

   procedure Source_Directories_Walk
     (Self      : Object;
      Source_CB : access procedure
                    (Dir_Index : Natural;
                     Source    : GPR2.Path_Name.Full_Name;
                     Timestamp : Ada.Calendar.Time);
      Dir_CB    : access procedure (Dir_Name : GPR2.Path_Name.Full_Name);
      Messages  : in out GPR2.Log.Object)
   is
      Visited_Dirs               : GPR2.Containers.Filename_Set;
      Dir_Index                  : Natural;
      Ignored_Sub_Dirs           : constant GPR2.Project.Attribute.Object :=
                                   Self.Attribute (PRA.Ignore_Source_Sub_Dirs);
      Ignored_Sub_Dirs_Regexps   : Regexp_List.Vector;
      Excluded_Dirs              : constant GPR2.Project.Attribute.Object :=
                                     Self.Attribute (PRA.Excluded_Source_Dirs);
      Excluded_Dirs_List         : GPR2.Containers.Filename_Set;
      Excluded_Recurse_Dirs_List : GPR2.Containers.Filename_Set;
      --  Ignore_Source_Sub_Dirs attribute values. In case the directory ends
      --  with a recursive indication "**", the dir is placed in
      --  Excluded_Recursive_Dirs_List.

      procedure On_Directory
        (Directory       : GPR2.Path_Name.Object;
         Is_Root_Dir     : Boolean;
         Do_Dir_Visit    : in out Boolean;
         Do_Subdir_Visit : in out Boolean);

      procedure On_File
        (File      : GPR2.Path_Name.Full_Name;
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
         if Excluded_Dirs_List.Contains (Directory.Value) then
            --  Do not visit this directory's files but still look for
            --  subdirectories.

            Do_Dir_Visit := False;

            return;

         elsif Excluded_Recurse_Dirs_List.Contains (Directory.Value) then
            --  Do not visit directory and subdirectories

            Do_Dir_Visit := False;
            Do_Subdir_Visit := False;

            return;
         end if;

         if not Is_Root_Dir then
            for Ignored_Sub_Dir of Ignored_Sub_Dirs_Regexps loop
               if GNAT.Regexp.Match
                 (String (GPR2.Path_Name.Simple_Name (Directory)),
                  Ignored_Sub_Dir)
               then
                  --  Ignore this matching sub dir tree
                  Do_Dir_Visit    := False;
                  Do_Subdir_Visit := False;

                  return;
               end if;
            end loop;
         end if;

         --  Do_Subdir_Visit is set to False if we already have visited
         --  this source directory:

         Visited_Dirs.Insert (Directory.Value, Position, Inserted);

         if not Inserted then
            --  Already visited
            Do_Dir_Visit := False;
         elsif Dir_CB /= null then
            Dir_CB (Directory.Value);
         end if;
      end On_Directory;

      -------------
      -- On_File --
      -------------

      procedure On_File
        (File      : GPR2.Path_Name.Full_Name;
         Timestamp : Ada.Calendar.Time) is
      begin
         Source_CB (Dir_Index, File, Timestamp);
      end On_File;

   begin
      if Self.Kind not in With_Source_Dirs_Kind then
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
               Val       : constant Value_Type := V.Text;
               Recursive : constant Boolean :=
                             Val'Length >= 2
                                 and then
                                   Val (Val'Last - 1 .. Val'Last) = "**";
               Last      : constant Natural :=
                             (if Recursive then Val'Last - 2 else Val'Last);
               Dir_Val   : constant Value_Type := Val (Val'First .. Last);
            begin
               if Dir_Val'Length = 0 then
                  if Recursive then
                     Excluded_Recurse_Dirs_List.Include (Self.Dir_Name.Value);
                  else
                     Excluded_Dirs_List.Include (Self.Dir_Name.Value);
                  end if;

               else
                  declare
                     Dir_Name     : constant GPR2.Path_Name.Object :=
                                      GPR2.Path_Name.Create_Directory
                                        (Filename_Type (Dir_Val),
                                         Self.Dir_Name.Name);
                     Relative_Dir : constant Filename_Type :=
                                      Dir_Name.Relative_Path
                                        (From => Self.Dir_Name);
                  begin
                     if Recursive then
                        Excluded_Recurse_Dirs_List.Include
                          (Self.Dir_Name.Compose (Relative_Dir, True).Value);
                     else
                        Excluded_Dirs_List.Include
                          (Self.Dir_Name.Compose (Relative_Dir, True).Value);
                     end if;
                  end;
               end if;
            end;
         end loop;
      end if;

      Dir_Index := 1;

      for S of Self.Attribute (PRA.Source_Dirs).Values loop
         --  If S denotes the view's source dir corresponding to
         --  --src-subdir, just skip if the dir does not exist (it is
         --  optional).
         if not (Self.Has_Source_Subdirectory
                 and then S.Text = Self.Source_Subdirectory.String_Value
                 and then not Self.Source_Subdirectory.Exists)
         then
            View_Internal.Foreach
              (Base_Dir          => Self.Dir_Name,
               Messages          => Messages,
               Directory_Pattern => Filename_Optional (S.Text),
               Source            => S,
               File_CB           => (if Source_CB = null
                                     then null
                                     else On_File'Access),
               Directory_CB      => On_Directory'Access);
            Dir_Index := Dir_Index + 1;
         end if;
      end loop;
   end Source_Directories_Walk;

   -------------------
   -- Source_Filter --
   -------------------

   function Source_Filter
     (Self : Object;
      S    : Build.Source_Base.Object'Class;
      Data : Build.Source.Sets.Filter_Data'Class) return Boolean
   is
      CU     : Build.Compilation_Unit.Object;
      Result : Boolean;
      Opt    : constant Source_Filter_Data := Source_Filter_Data (Data);

   begin
      if Opt.Compilable_Only then
         if not Self.Is_Compilable (S.Language) then
            return False;
         end if;

         --  Further check if the source can be input of gcc

         if S.Has_Units then
            Result := False;

            Unit_Loop :
            for Unit of S.Units loop
               if Unit.Kind = S_Body then
                  Result := True;

                  exit Unit_Loop;

               elsif Unit.Kind = S_Spec then
                  for NS of Opt.View.Namespace_Roots loop
                     CU := NS.Unit (Unit.Name);

                     if not CU.Has_Part (S_Body) then
                        Result := True;

                        exit Unit_Loop;
                     end if;
                  end loop;
               end if;
            end loop Unit_Loop;

            if not Result then
               return False;
            end if;

         elsif S.Kind /= S_Body then
            return False;
         end if;
      end if;

      if Opt.Interface_Only then
         Result := False;

         declare
            Def : constant View_Internal.Const_Ref :=
                    View_Internal.Get_RO (Self);
         begin
            if Def.Interface_Sources.Contains (S.Path_Name.Simple_Name) then

               Result := True;

            elsif S.Has_Units then
               for CU of S.Units loop
                  if Def.Interface_Units.Contains (CU.Name) then
                     Result := True;
                  end if;
               end loop;
            end if;

            if not Result then
               return False;
            end if;
         end;
      end if;

      return True;
   end Source_Filter;

   -----------------
   -- Source_Path --
   -----------------

   function Source_Path
     (Self            : Object;
      Name            : GPR2.Simple_Name;
      Allow_Spec_File : Boolean;
      Allow_Unit_Name : Boolean) return GPR2.Path_Name.Object
   is
      Src : GPR2.Build.Source.Object;
   begin
      Src := Self.Source (Name);

      if Src.Is_Defined then
         return Src.Path_Name;
      end if;

      if Allow_Unit_Name then
         declare
            Unit : constant Build.Compilation_Unit.Object :=
                     Self.Own_Unit (Name_Type (Name));
         begin
            if Unit.Is_Defined then
               if Allow_Spec_File or else Unit.Has_Part (S_Body) then
                  return Unit.Main_Part.Source;
               end if;
            end if;
         end;
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
      P : constant GPR2.Path_Name.Object :=
            Self.Object_Directory.Compose
              (Filename_Type (To_Lower (Self.Name))
               & "-" & Self.Tree_Int.Src_Subdirs, Directory => True);
   begin
      --  First check for <obj>/<project.lowercase_name>-<src_subdirs>

      if P.Exists then
         return P;
      else
         --  Then default to <obj>/<src_subdirs>

         return Self.Object_Directory.Compose
           (Self.Tree_Int.Src_Subdirs, Directory => True);
      end if;
   end Source_Subdirectory;

   -------------
   -- Sources --
   -------------

   function Sources
     (Self            : Object;
      Interface_Only  : Boolean := False;
      Compilable_Only : Boolean := False) return Build.Source.Sets.Object is
   begin
      if Self.Kind in With_Object_Dir_Kind then
         declare
            F_Data : constant Source_Filter_Data :=
                       (View            => Self,
                        Interface_Only  => Interface_Only,
                        Compilable_Only => Compilable_Only);
            Db     : constant GPR2.Build.View_Db.Object := Self.View_Db;

         begin
            return Build.Source.Sets.Create
              (Db, Build.Source.Sets.Sorted, Source_Filter'Access, F_Data);
         end;

      else
         return Build.Source.Sets.Empty_Set;
      end if;
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

   --------------------------
   -- Suffixed_Simple_Name --
   --------------------------

   function Suffixed_Simple_Name
     (Self : Object;
      Name : String;
      Lang : Language_Id := Ada_Language) return Simple_Name
   is
      use GNATCOLL.Utils;

      Default_Ada_MU_BS : constant String := ".ada";
      Index             : constant Attribute_Index.Object :=
                            Attribute_Index.Create (Lang);

      function Ends_With_One_Language (Name : String) return Boolean;
      --  Check if Name ends with any Self language naming convention suffix

      function Is_An_Exception (Name : String) return Boolean;
      --  Check if Name is an exception and should not be matched
      --  with classical language naming convention.

      ----------------------------
      -- Ends_With_One_Language --
      ----------------------------

      function Ends_With_One_Language (Name : String) return Boolean is
         Body_Attr : constant Project.Attribute.Object :=
                       Self.Attribute (PRA.Naming.Body_Suffix, Index);
         Spec_Attr : constant Project.Attribute.Object :=
                       Self.Attribute (PRA.Naming.Spec_Suffix, Index);
      begin
         return (Body_Attr.Is_Defined
                 and then Ends_With (Name, Body_Attr.Value.Text))
           or else (Spec_Attr.Is_Defined
                    and then Ends_With (Name, Spec_Attr.Value.Text));
      end Ends_With_One_Language;

      ---------------------
      -- Is_An_Exception --
      ---------------------

      function Is_An_Exception (Name : String) return Boolean is
      begin
         for Attr of Self.Attributes (PRA.Naming.Implementation) loop
            if Attr.Value.Text = Name then
               return True;
            end if;
         end loop;

         for Attr of Self.Attributes (PRA.Naming.Specification) loop
            if Attr.Value.Text = Name then
               return True;
            end if;
         end loop;

         for Lang of Self.Language_Ids loop
            declare
               Index    : constant Attribute_Index.Object :=
                            Attribute_Index.Create (Lang);
               IEs_Attr : constant Project.Attribute.Object :=
                            Self.Attribute
                              (PRA.Naming.Implementation_Exceptions,
                               Index);
               SEs_Attr : constant Project.Attribute.Object :=
                            Self.Attribute
                              (PRA.Naming.Specification_Exceptions,
                               Index);
            begin
               if IEs_Attr.Is_Defined then
                  for Value of IEs_Attr.Values loop
                     if Name = Value.Text then
                        return True;
                     end if;
                  end loop;
               end if;

               if SEs_Attr.Is_Defined then
                  for Value of SEs_Attr.Values loop
                     if Name = Value.Text then
                        return True;
                     end if;
                  end loop;
               end if;
            end;
         end loop;

         return False;
      end Is_An_Exception;

   begin
      if Is_An_Exception (Name)
        or else Ends_With_One_Language (Name)
        or else Ends_With (Name, Default_Ada_MU_BS)
      then
         return Simple_Name (Name);
      else
         declare
            Attr   : constant Project.Attribute.Object :=
                        Self.Attribute (PRA.Naming.Body_Suffix, Index);
         begin
            if Attr.Is_Defined then
               return Simple_Name (Name & Attr.Value.Text);
            else
               return Simple_Name (Name);
            end if;
         end;
      end if;
   end Suffixed_Simple_Name;

   -----------
   -- Types --
   -----------

   function Types (Self : Object) return Project.Typ.Set.Object is
   begin
      return View_Internal.Get_RO (Self).Types;
   end Types;

   -----------
   -- Units --
   -----------

   function Unit
     (Self : Object;
      Name : Name_Type) return Build.Compilation_Unit.Object
   is
      Db : constant Build.View_Db.Object := Self.View_Db;
   begin
      if Db.Has_Compilation_Unit (Name) then
         return Db.Compilation_Unit (Name);
      else
         return Build.Compilation_Unit.Undefined;
      end if;
   end Unit;

   ---------------
   -- Unit_Part --
   ---------------

   function Unit_Part
     (Self : Object; Name : Name_Type; Is_Spec : Boolean)
      return Build.Compilation_Unit.Unit_Location
   is
      Db : constant Build.View_Db.Object := Self.View_Db;
      CU : Build.Compilation_Unit.Object;
   begin
      if Db.Has_Compilation_Unit (Name) then
         CU := Db.Compilation_Unit (Name);
      else
         return Build.Compilation_Unit.No_Unit;
      end if;

      if Is_Spec then
         if CU.Has_Part (S_Spec) then
            return CU.Spec;
         else
            return Build.Compilation_Unit.No_Unit;
         end if;

      elsif CU.Name = Name then
         if CU.Has_Part (S_Body) then
            return CU.Main_Body;
         else
            return Build.Compilation_Unit.No_Unit;
         end if;

      else
         --  Queried unit is a separate. We need to extract the separate name
         --  from the fully qualified name.
         declare
            S_Name : constant Name_Type :=
                       Name (Name'First + 1 + CU.Name'Length .. Name'Last);
         begin
            return CU.Get (S_Separate, S_Name);
         end;
      end if;
   end Unit_Part;

   -----------
   -- Units --
   -----------

   function Units
     (Self : Object;
      With_Externally_Built : Boolean := False)
      return GPR2.Build.Compilation_Unit.Maps.Map
   is
      Db : constant GPR2.Build.View_Db.Object := Self.View_Db;
   begin
      return Db.Compilation_Units (With_Externally_Built);
   end Units;

   --------------
   -- Variable --
   --------------

   function Variable
     (Self : Object; Name : Name_Type) return Project.Variable.Object is
   begin
      return View_Internal.Get_RO (Self).Vars (Name);
   end Variable;

   function Variable
     (Self : Object;
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
      return View_Internal.Get_RO (Self).Vars;
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
      Data : constant View_Internal.Const_Ref := View_Internal.Get_RO (Self);
      Dad  : Object := Data.Extended_Root;
      C    : View_Internal.Project_View_Store.Cursor;
   begin
      --  First, check the name of the project itself

      if Self.Name = Name then
         return Self;
      end if;

      --  Lookup in the ancestors next

      while Dad.Is_Defined loop
         if Dad.Name = Name then
            return Dad;
         end if;

         Dad := View_Internal.Get_RO (Dad).Extended_Root;
      end loop;

      --  Lookup in the imported next

      C := Data.Imports.Find (Name);

      if View_Internal.Project_View_Store.Has_Element (C) then
         return View_Internal.Project_View_Store.Element (C);
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
         end if;
      end;

      return Undefined;
   end View_For;

   --------------------
   -- Visible_Source --
   --------------------

   function Visible_Source
     (Self      : Object;
      Filename  : GPR2.Simple_Name;
      Ambiguous : out Boolean)
      return Build.Source.Object
   is (Self.View_Db.Visible_Source (Filename, Ambiguous));

   function Visible_Source
     (Self : Object;
      Path : GPR2.Path_Name.Object)
      return Build.Source.Object
   is (Self.View_Db.Visible_Source (Path));

   function Visible_Source
     (Self      : Object;
      Filename  : GPR2.Simple_Name)
      return Build.Source.Object
   is
      Ambiguous : Boolean;
   begin
      return Self.Visible_Source (Filename, Ambiguous);
   end Visible_Source;

   ---------------------
   -- Visible_Sources --
   ---------------------

   function Visible_Sources
     (Self : Object) return GPR2.Build.Source.Sets.Object
   is (Self.View_Db.Visible_Sources);

begin
   View_Internal.Get_RO   := Get_RO'Access;
   View_Internal.Get_RW   := Get_RW'Access;
   View_Internal.Get      := Get_Ref'Access;
   View_Internal.Set      := Set_Def'Access;
   View_Internal.Refcount := Refcount'Access;
   View_Internal.Weak     := Weak'Access;
   View_Internal.Strong   := Strong'Access;
end GPR2.Project.View;
