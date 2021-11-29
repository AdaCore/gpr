------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2021, AdaCore                      --
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

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;
with GNATCOLL.Utils;
with GNAT.Regexp; use GNAT.Regexp;

with GPR2.Message;
with GPR2.Project.Attribute_Cache;
with GPR2.Project.Definition;
with GPR2.Project.Pack;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View.Set;
with GPR2.Source_Info;
with GPR2.Project.Unit_Info;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Pack;
with System.Assertions;

package body GPR2.Project.View is

   use GNAT;

   function Get_Ref (View : Object) return Definition.Ref is
     (Definition.Data (View.Get.Element.all)'Unchecked_Access);

   function Get_RO (View : Object) return Definition.Const_Ref is
     (Definition.Data (View.Get.Element.all)'Unchecked_Access);

   function Get_RW (View : in out Object) return Definition.Ref is
     (Definition.Data (View.Get.Element.all)'Unchecked_Access);

   function Pack
      (Self : Object;
       Name : Package_Id) return Project.Pack.Object;
   --  Get the package with the given Name.

   function Refcount (Self : Object) return Natural is
     (Definition_References.Get_Refcount (Self));
   --  Get view refcount

   procedure Set_Def (Ref : out View.Object; Def : Definition_Base'Class);
   --  Convert definition to view

   function Apply_Root_And_Subdirs
     (Self : Object; Dir_Attr : Attribute_Id) return GPR2.Path_Name.Object;
   --  Apply project path and subdir option for library, object and executable
   --  directories defined in attribute Dir_Attr.

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

   function Aggregated (Self : Object) return Set.Object is
   begin
      return Set : GPR2.Project.View.Set.Object do
         for Agg of Definition.Get_RO (Self).Aggregated loop
            Set.Insert (Agg);
         end loop;
      end return;
   end Aggregated;

   ----------------------------
   -- Apply_Root_And_Subdirs --
   ----------------------------

   function Apply_Root_And_Subdirs
     (Self : Object; Dir_Attr : Attribute_Id) return GPR2.Path_Name.Object
   is
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
   end Apply_Root_And_Subdirs;

   ---------------
   -- Artifacts --
   ---------------

   function Artifacts (Self : Object) return GPR2.Path_Name.Set.Object is
      Result : GPR2.Path_Name.Set.Object;

      procedure Result_Append
        (Dir : GPR2.Path_Name.Object; Attr : Attribute_Id);
      --  Append files created from directory name and filenames from list of
      --  attributes.

      -------------------
      -- Result_Append --
      -------------------

      procedure Result_Append
        (Dir : GPR2.Path_Name.Object; Attr : Attribute_Id)
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
      Result_Append (Self.Object_Directory, PRA.Artifacts_In_Object_Dir);
      if Self.Kind = K_Standard then
         Result_Append (Self.Executable_Directory, PRA.Artifacts_In_Exec_Dir);
      end if;

      return Result;
   end Artifacts;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Self   : Object;
      Name   : Attribute_Id;
      Pack   : Optional_Package_Id    := No_Package;
      Index  : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos : Unit_Index             := No_Index)
      return Project.Attribute.Object
   is
      use type Project.Attribute_Index.Object;
      use type PRA.Index_Kind;
      use type PRA.Index_Value_Type;
      use type PRA.Inherit_From_Extended_Type;

      --  Compute the attribute qualified name.
      PRA_Name  : constant PRA.Qualified_Name := PRA.Create (Name, Pack);
      Alias     : constant Optional_Attribute_Id := PRA.Alias (PRA_Name).Attr;
      PRA_Def   : PRA.Def;
      Result    : Project.Attribute.Object;
      Has_Index : constant Boolean := Index /= Attribute_Index.Undefined;

      function Found (Attribute : Project.Attribute.Object) return Boolean
         with Inline => True;

      function Get_Attribute_From_View (View : Object)
         return Project.Attribute.Object;
      --  Internal function to get attribute from a view

      function Get_Package_Attribute (View : Object)
         return Project.Attribute.Object;

      function Get_Toplevel_Attribute (View : Object)
         return Project.Attribute.Object;

      procedure Check_Matching_Index
         (Pattern : Project.Attribute.Object;
          Result  : in out Project.Attribute.Object);

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
         if Match (Index.Text,
                   Compile
                     (Pattern.Index.Text,
                      Glob           => True,
                      Case_Sensitive => Pattern.Index.Is_Case_Sensitive))
         then
            if not Found (Result)
              or else Source_Reference.Object (Result) <
                        Source_Reference.Object (Pattern)
            then
               Result := Pattern;
            end if;
         end if;
      exception
         when Error_In_Regexp =>
            null;
      end Check_Matching_Index;

      -----------
      -- Found --
      -----------

      function Found (Attribute : Project.Attribute.Object) return Boolean
      is
      begin
         --  Found if attribute is defined and not a default value, or is
         --  read_only (builtin value).

         return Attribute.Is_Defined and then
           (not Attribute.Is_Default or else PRA_Def.Builtin);
      end Found;

      -----------------------------
      -- Get_Attribute_From_View --
      -----------------------------

      function Get_Attribute_From_View (View : Object)
         return Project.Attribute.Object
      is
         Result : Project.Attribute.Object;
      begin
         if Pack = No_Package then
            Result := Get_Toplevel_Attribute (View => View);
         else
            --  If the attribute is from a project the GPR semantic is to look
            --  for the attribute in the current view. If the associated
            --  package is not present look for a package in the extended view.

            if View.Has_Packages (Pack, With_Defaults => False) then
               Result := Get_Package_Attribute (View => View);

            elsif View.Is_Extending
              and then PRA_Def.Inherit_From_Extended /= PRA.Not_Inherited
            then
               Result := Get_Attribute_From_View (View => View.Extended_Root);
            end if;
         end if;

         return Result;
      end Get_Attribute_From_View;

      -----------------------
      -- Get_Default_Index --
      -----------------------

      function Get_Default_Index return Value_Type
      is
      begin
         if not Has_Index then
            return Registry.Attribute.Any_Index;
         else
            return Index.Value (Preserve_Case => Index.Is_Case_Sensitive);
         end if;
      end Get_Default_Index;

      ---------------------------
      -- Get_Package_Attribute --
      ---------------------------

      function Get_Package_Attribute (View : Object)
         return Project.Attribute.Object
      is
         Pkg_Obj : GPR2.Project.Pack.Object renames View.Pack (Pack);
         Result  : Project.Attribute.Object;
      begin
         --  Implementation note: Check_Atttribute on package does not follow
         --  extended because if a package is defined in an extending project
         --  it override the complete package from the extended project.

         Result := Pkg_Obj.Attrs.Element (Name, Index, At_Pos);

         if not Found (Result) and then Alias /= No_Attribute then
            Result := Pkg_Obj.Attrs.Element (Alias, Index, At_Pos);
         end if;

         if Found (Result) then
            return Result;
         end if;

         --  If the attribute has not been found, first handle the case in
         --  which an index is used and pattern matching might apply.

         if Index /= Attribute_Index.Undefined then
            if PRA_Def.Index_Type in
              PRA.FileGlob_Index | PRA.FileGlob_Or_Language_Index
            then
               --  The index might match a globbing pattern. In that case
               --  iterate other the statements to find one that match the
               --  index.

               for Attr of Pkg_Obj.Attrs.Filter (Name) loop
                  Check_Matching_Index
                     (Pattern => Attr,
                      Result  => Result);
               end loop;

               if not Found (Result) and then Alias /= No_Attribute then
                  for Attr of Pkg_Obj.Attrs.Filter (Alias) loop
                     Check_Matching_Index
                       (Pattern => Attr,
                        Result  => Result);
                  end loop;
               end if;
            end if;

            if not Found (Result) and then PRA_Def.Others_Allowed then
               Result := Pkg_Obj.Attrs.Element
                 (Name, Project.Attribute_Index.I_Others);

               if not Found (Result) and then Alias /= No_Attribute then
                  Result :=
                    Pkg_Obj.Attrs.Element
                      (Alias, Project.Attribute_Index.I_Others);
               end if;
            end if;
         end if;

         return Result;
      end Get_Package_Attribute;

      ----------------------------
      -- Get_Toplevel_Attribute --
      ----------------------------

      function Get_Toplevel_Attribute (View : Object)
         return Project.Attribute.Object
      is
         Result  : Project.Attribute.Object;
      begin
         --  First try to find an exact match

         Result := Get_RO (View).Attrs.Element (Name, Index, At_Pos);

         if not Found (Result) and then Alias /= No_Attribute then
            Result := Get_RO (View).Attrs.Element (Alias, Index, At_Pos);
         end if;

         if Found (Result) then
            return Result;
         end if;

         --  If the attribute has not been found, first handle the case in
         --  which an index is used and pattern matching might apply.

         if Index /= Attribute_Index.Undefined then
            if PRA_Def.Index_Type = PRA.FileGlob_Index or else
               PRA_Def.Index_Type = PRA.FileGlob_Or_Language_Index
            then
               --  The index might match a globbing pattern. In that case
               --  iterate other the statements to find one that match the
               --  index.

               for Cursor in Definition.Get_RO (Self).Attrs.Iterate
                  (Name => Name)
               loop
                  Check_Matching_Index
                     (Pattern => Project.Attribute.Set.Element (Cursor),
                      Result  => Result);
               end loop;

               if not Found (Result) and then Alias /= No_Attribute then
                  for Cursor in Definition.Get_RO (Self).Attrs.Iterate
                    (Name => Alias)
                  loop
                     Check_Matching_Index
                       (Pattern => Project.Attribute.Set.Element (Cursor),
                        Result  => Result);
                  end loop;
               end if;
            end if;

            if not Found (Result) and then PRA_Def.Others_Allowed then
               Result := Get_RO (View).Attrs.Element
                 (Name, Project.Attribute_Index.I_Others);

               if not Found (Result) and then Alias /= No_Attribute then
                  Result := Get_RO (View).Attrs.Element
                    (Name, Project.Attribute_Index.I_Others);
               end if;
            end if;
         end if;

         --  Attribute value cannot be found, so start looking into extended
         --  views.

         if not Found (Result)
           and then View.Is_Extending
           and then PRA_Def.Inherit_From_Extended /= PRA.Not_Inherited
         then
            return Get_Toplevel_Attribute (View.Extended_Root);

         else
            if PRA_Def.Inherit_From_Extended = PRA.Concatenated
              and then View.Is_Extending
            then
               --  We need to fetch to concatenate the attribute value with
               --  the value from the extended project.

               declare
                  Result2 : Project.Attribute.Object;
               begin
                  Result2 := Get_Toplevel_Attribute (View.Extended_Root);
                  if Found (Result2) then
                     for V of Result2.Values loop
                        Result.Append (V);
                     end loop;
                  end if;
               end;
            end if;

            return Result;
         end if;
      end Get_Toplevel_Attribute;

      Cache_Cursor : constant Project.Attribute_Cache.Cursor :=
         Definition.Get_RO (Self).Cache.Check_Cache
            (Name   => Name,
             Pkg    => Pack,
             Index  => Index,
             At_Pos => At_Pos);
   begin
      if Project.Attribute_Cache.Has_Element (Cache_Cursor) then
         return Project.Attribute_Cache.Element (Cache_Cursor);
      end if;

      Definition.Get_RO (Self).Cache.Schedule_Update_Cache;

      --  First check if the attribute is defined in the registry.

      if not PRA.Exists (PRA_Name) then
         raise Attribute_Error
            with PRA.Image (PRA_Name) & " attribute does not exist";
      end if;

      --  Fetch the attribute definition

      PRA_Def := PRA.Get (PRA_Name);

      --  Check if index is used correctly

      if PRA_Def.Index = PRA.No and then Has_Index then
         raise Attribute_Error
            with PRA.Image (PRA_Name) & " attribute does not accept index";
      end if;

      --  Try to fetch the attribute from the view

      if not Result.Is_Defined then
         Result := Get_Attribute_From_View (View => Self);
      end if;

      --  Handle configuration project

      if GPR2.Project.Tree.Has_Configuration (Self.Tree.all) then

         if not Found (Result) then
            --  If at this stage Result is not defined try to fetch the value
            --  from the configuration project.

            Result := Get_Attribute_From_View
              (View => Self.Tree.all.Configuration.Corresponding_View);

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

               if Result2.Is_Defined then
                  for V of Result.Values loop
                     Result2.Append (V);
                  end loop;

                  Result := Result2;
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
            use type PRA.Default_Value_Kind;
            use type PRA.Value_Kind;
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
                                        (Name   => Default.Attr,
                                         Pack   => Pack,
                                         Index  =>
                                           (if Name = PRA.Separate_Suffix
                                            then Attribute_Index.Create
                                              (Ada_Language)
                                            else Index),
                                         At_Pos => At_Pos);
                  begin
                     if Ref.Is_Defined then
                        Result := Ref.Rename (Attr_Name);

                        --  See note above about Separate_Suffix...
                        if Name = PRA.Separate_Suffix then
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
                           As_List => PRA_Def.Value = PRA.List);
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
                  As_List => PRA_Def.Value = PRA.List);
            end case;
         end;
      end if;

      if Alias /= No_Attribute
        and then Result.Is_Defined
        and then Result.Name.Id = Alias
      then
         --  We found the alternative name for the attribute. Let's rename
         --  it to the requested name.
         Result := GPR2.Project.Attribute.Get_Alias (Result, Name);
      end if;

      --  Finally return the result
      Definition.Get_RO (Self).Cache.Update_Cache
         (Name   => Name,
          Pkg    => Pack,
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
      Name  : Attribute_Id;
      Index : Attribute_Index.Object := Attribute_Index.Undefined;
      Pack  : Optional_Package_Id    := No_Package)
      return Source_Reference.Object'Class
   is
      Attr : Project.Attribute.Object;
   begin
      Attr := Self.Attribute (Name, Pack, Index => Index);

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
      Name          : Optional_Attribute_Id  := No_Attribute;
      Index         : Attribute_Index.Object := Attribute_Index.Undefined;
      Pack          : Optional_Package_Id    := No_Package;
      With_Defaults : Boolean                := True;
      With_Config   : Boolean                := True;
      Alias_Check   : Boolean                := False)
      return Project.Attribute.Set.Object
   is
      Result  : Project.Attribute.Set.Object;
      Result2 : Project.Attribute.Set.Object;
      Alias   : constant PRA.Qualified_Name :=
                  (if Name = No_Attribute
                   then PRA.No_Name
                   else PRA.Alias (PRA.Create (Name, Pack)));

      procedure Action (Attr : Attribute_Id;
                        Def  : Registry.Attribute.Def);

      procedure Maybe_Insert
        (Attr        : Project.Attribute.Object;
         Concatenate : Boolean := False);

      ------------
      -- Action --
      ------------

      procedure Action (Attr : Attribute_Id;
                        Def  : Registry.Attribute.Def)
      is
         use Registry.Attribute;

         function Get_PA
           (Value : Value_Type;
            Index : Value_Type := "") return Project.Attribute.Object;

         ------------
         -- Get_PA --
         ------------

         function Get_PA
           (Value : Value_Type;
            Index : Value_Type := "") return Project.Attribute.Object is
         begin
            if Def.Index = PRA.No then
               return Project.Attribute.Create
                 (Name     => Attr,
                  Index    => PAI.Undefined,
                  Source   => Self.Path_Name,
                  Default  => Value,
                  As_List  => Def.Value = PRA.List);
            else
               return Project.Attribute.Create
                 (Name                 => Attr,
                  Index                => Index,
                  Index_Case_Sensitive => Def.Index_Case_Sensitive,
                  Source               => Self.Path_Name,
                  Default              => Value,
                  As_List              => Def.Value = PRA.List);
            end if;
         end Get_PA;

      begin
         --  Apply name filter
         if Name /= No_Attribute and then Attr /= Name then
            return;
         end if;

         --  Check if the attribute actually has a default in the project
         --  context
         if not Def.Has_Default_In (Self.Kind) then
            return;
         end if;

         case Def.Default.Kind is
            when D_Attribute_Reference =>
               for CA in Self.Attributes
                 (Def.Default.Attr,
                  (if Attr = PRA.Separate_Suffix
                   then Attribute_Index.Create (Ada_Language)
                   else Index),
                  Pack).Iterate (With_Defaults => True)
               loop
                  declare
                     Ref_Attr : constant Project.Attribute.Object :=
                                  Project.Attribute.Set.Element (CA);
                     Loc      : constant Source_Reference.Object :=
                                  Source_Reference.Object
                                    (Self.Attribute_Location
                                       (Attr, Pack => Pack));

                     --  Rename the original attribute. Note: this ensures the
                     --  Is_Default flag is set.

                     Def_Attr : Project.Attribute.Object :=
                                  Ref_Attr.Rename
                                    (Source_Reference.Attribute.Object
                                       (Source_Reference.Attribute.Create
                                          (Loc, Attr)));

                  begin
                     if Attr = PRA.Separate_Suffix then
                        Def_Attr.Set_Index (Attribute_Index.Undefined);
                     end if;

                     Maybe_Insert (Def_Attr);
                  end;
               end loop;

            when D_Callback =>
               declare
                  Def_Attr : constant Project.Attribute.Object :=
                               Get_PA
                                 (Def.Default.Callback (Self),
                                  Index.Value);
               begin
                  Maybe_Insert (Def_Attr);
               end;

            when D_Value =>
               for Cursor in Def.Default.Values.Iterate loop
                  if Def.Index = PRA.No
                    or else PRA.Value_Map.Key (Cursor) = PRA.Any_Index
                    or else not Index.Is_Defined
                    or else PRA.Value_Map.Key (Cursor) = Index.Value
                  then
                     declare
                        Def_Attr : constant Project.Attribute.Object :=
                                     Get_PA
                                       (PRA.Value_Map.Element (Cursor),
                                        (if Index.Is_Defined
                                         then Index.Value
                                         else Value_Map.Key (Cursor)));
                     begin
                        Maybe_Insert (Def_Attr);
                     end;
                  end if;
               end loop;
         end case;
      end Action;

      ------------------
      -- Maybe_Insert --
      ------------------

      procedure Maybe_Insert
        (Attr        : Project.Attribute.Object;
         Concatenate : Boolean := False)
      is
         use type Attribute_Index.Object;
         Attr_Index : Project.Attribute_Index.Object;
      begin
         if (Name = No_Attribute
             or else Name = Attr.Name.Id)
           and then (Index = Attribute_Index.Undefined
                     or else Index = Attr.Index)
         then
            if Attr.Has_Index then
               Attr_Index := Attr.Index;
            end if;

            if not Result.Contains
              (Name  => Attr.Name.Id,
               Index => Attr_Index)
              and then
                (Alias.Attr = No_Attribute
                 or else not Result.Contains
                   (Name  => Alias.Attr,
                    Index => Attr_Index))
            then

               Result.Insert (Attr);

            elsif Concatenate then
               declare
                  Own_Attr : constant Project.Attribute.Object :=
                               Result.Element (Attr.Name.Id, Attr_Index);
                  Inh_Attr : Project.Attribute.Object;

               begin
                  if Own_Attr.Is_Defined then
                     Inh_Attr := Attr;

                     for V of Own_Attr.Values loop
                        Inh_Attr.Append (V);
                     end loop;

                     Result.Include (Inh_Attr);
                  else
                     Result.Insert (Attr);
                  end if;
               end;
            end if;
         end if;
      end Maybe_Insert;

   begin
      if Pack = No_Package then
         Result := Definition.Get_RO (Self).Attrs.Filter (Name, Index);

         --  Check inherited attributes
         if Self.Is_Extending then
            for Attr of Self.Extended_Root.Attributes
                                             (Name, Index, No_Package, False)
            loop
               declare
                  Q_Name : constant PRA.Qualified_Name :=
                             PRA.Create (Attr.Name.Id);
                  Def    : constant PRA.Def := PRA.Get (Q_Name);
               begin
                  case Def.Inherit_From_Extended is
                     when PRA.Inherited =>
                        Maybe_Insert (Attr);
                     when PRA.Concatenated =>
                        Maybe_Insert (Attr, True);
                     when PRA.Not_Inherited =>
                        null;
                  end case;
               end;
            end loop;
         end if;

      elsif Self.Has_Packages (Pack) then
         Result := Self.Pack (Pack).Attrs.Filter (Name, Index);

         if Alias.Attr /= No_Attribute then
            for Attr of Self.Pack (Pack).Attrs.Filter (Alias.Attr, Index) loop
               declare
                  Renamed : constant Project.Attribute.Object :=
                              Attr.Get_Alias (Name);
                  C       : constant Project.Attribute.Set.Cursor :=
                              Result.Find (Renamed);
               begin
                  if not Project.Attribute.Set.Has_Element (C) then
                     Result.Insert (Renamed);

                  elsif Project.Attribute.Set.Element (C).Is_Default
                    and then not Renamed.Is_Default
                  then
                     --  Replace default value with an actual one
                     Result.Include (Renamed);

                  elsif not Renamed.Is_Default then
                     --  Both are present... look for the last defined one
                     if Project.Attribute.Set.Element (C).Line < Renamed.Line
                     then
                        Result.Include (Renamed);
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end if;

      --  ??? TODO: add attributes from config.

      --  Fill up the missing default values
      if With_Defaults then
         Registry.Attribute.For_Each_Default
           (Rules  => Registry.Attribute.Get_Default_Rules (Pack),
            Action => Action'Access);
      end if;

      --  Check if we need to adjust the results
      if not With_Defaults then
         for Attr of Result loop
            if not Attr.Is_Default then
               Result2.Insert (Attr);
            end if;
         end loop;

         return Result2;

      else
         return Result;
      end if;
   end Attributes;

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
                                   (PRA.Body_Suffix,
                                    PRP.Naming,
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
      Index  : constant Attribute_Index.Object :=
                 Attribute_Index.Create (Value_Type (Name (Language)));
   begin
      return Filename_Optional
        (Self.Attribute (PRA.Prefix, PRP.Binder, Index).Value.Text);
   end Binder_Prefix;

   ---------------------
   -- Check_Attribute --
   ---------------------

   function Check_Attribute
     (Self           : Object;
      Name           : Attribute_Id;
      Index          : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos         : Unit_Index             := No_Index;
      Result         : out Project.Attribute.Object) return Boolean is
   begin
      Result := Self.Attribute (Name, No_Package, Index, At_Pos);
      return Result.Is_Defined;
   exception
      when Attribute_Error =>
         Result := Project.Attribute.Undefined;
         return False;
   end Check_Attribute;

   function Check_Attribute
     (Self           : Object;
      Pack           : Package_Id;
      Name           : Attribute_Id;
      Index          : Attribute_Index.Object := Attribute_Index.Undefined;
      At_Pos         : Unit_Index             := No_Index;
      Result         : out Project.Attribute.Object) return Boolean is
   begin
      Result := Self.Attribute (Name, Pack, Index, At_Pos);
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

   ------------------
   -- Check_Source --
   ------------------

   function Check_Source
     (Self     : Object;
      Filename : GPR2.Simple_Name;
      Result   : in out Project.Source.Object) return Boolean is
   begin
      return Definition.Check_Source (Self, Filename, Result);
   end Check_Source;

   --------------------------
   -- Clean_Attribute_List --
   --------------------------

   function Clean_Attribute_List
     (Self     : Object;
      Name     : Attribute_Id;
      Language : Language_Id) return Containers.Value_Set
   is
      Index  : constant Attribute_Index.Object :=
                 (if Language = No_Language
                  then Attribute_Index.Undefined
                  else Attribute_Index.Create
                    (Value_Type (GPR2.Name (Language))));
      Attr   : constant Project.Attribute.Object :=
                 Self.Attribute (Name, PRP.Clean, Index);
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
     (Self    : Object;
      Source  : Simple_Name;
      At_Pos  : Unit_Index) return GPR2.Path_Name.Object
   is
      BN       : constant  Value_Not_Empty :=
                   Remove_Body_Suffix (Self, Source);
      BN_Index : constant Attribute_Index.Object :=
                   Attribute_Index.Create (BN);
      Index    : constant Attribute_Index.Object :=
                   Attribute_Index.Create (Value_Not_Empty (Source));
      Attr     : GPR2.Project.Attribute.Object;

      function Executable
        (Base_Name : Value_Not_Empty) return GPR2.Path_Name.Object
      is (GPR2.Path_Name.Create_File
          (Filename_Type (Base_Name) & Self.Executable_Suffix,
           Filename_Optional (Self.Executable_Directory.Dir_Name)));
      --  Full executable path for base name.

   begin
      if (Self.Check_Attribute
                 (PRP.Builder, PRA.Executable, Index, At_Pos, Attr)
           or else
             (Source /= Simple_Name (BN)
              and then Self.Check_Attribute
                (PRP.Builder, PRA.Executable, BN_Index, At_Pos, Attr)))
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
      return Filename_Optional (Self.Attribute
        (PRA.Executable_Suffix, Pack => PRP.Builder).Value.Text);
   end Executable_Suffix;

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

   -------------
   -- Foreach --
   -------------

   procedure Foreach
     (Self              : Object;
      Directory_Pattern : GPR2.Filename_Optional;
      Source            : GPR2.Source_Reference.Value.Object;
      File_CB           : not null access procedure
        (File : GPR2.Path_Name.Object);
      Directory_CB      : access procedure
        (Directory       : GPR2.Path_Name.Object;
         Is_Root_Dir     : Boolean;
         Do_Dir_Visit    : in out Boolean;
         Do_Subdir_Visit : in out Boolean) := null)
   is
      use GNAT.OS_Lib;

      View_Dir  : constant GPR2.Path_Name.Object :=
                    GPR2.Path_Name.Create_Directory
                      (Filename_Optional (Self.Path_Name.Dir_Name));
      Dir       : constant String :=
                    (if Directory_Pattern'Length = 0
                     then "."
                     else
                       (if Directory_Pattern = "**"
                        then "./**"
                        else String (Directory_Pattern)));
      --  normalize dir part avoiding "" & "**"
      Recursive : constant Boolean :=
                    GNATCOLL.Utils.Ends_With (Dir, "**");
      Last      : constant Positive :=
                    Dir'Last - (if Recursive then 2 else 0);
      Root_Dir  : constant String :=
                    (if Is_Absolute_Path (Dir)
                     then Dir (Dir'First .. Last)
                     else View_Dir.Compose
                       (Filename_Optional (Dir (Dir'First .. Last))).Value);

      procedure Handle_Directory
        (Dir         : Filename_Type;
         Recursive   : Boolean;
         Is_Root_Dir : Boolean := False);
      --  Handle the specified directory, that is read all files in Dir and
      --  eventually call recursivelly Handle_Directory if a recursive read
      --  is specified.

      ----------------------
      -- Handle_Directory --
      ----------------------

      procedure Handle_Directory
        (Dir         : Filename_Type;
         Recursive   : Boolean;
         Is_Root_Dir : Boolean := False)
      is
         use all type Directories.File_Kind;

         Dir_Search      : Directories.Search_Type;
         Dir_Entry       : Directories.Directory_Entry_Type;
         Do_Dir_Visit    : Boolean := True;
         Do_Subdir_Visit : Boolean := Recursive;
      begin
         if Directory_CB /= null then
            Directory_CB
              (GPR2.Path_Name.Create_Directory
                 (Dir, GPR2.Path_Name.No_Resolution),
               Is_Root_Dir,
               Do_Dir_Visit,
               Do_Subdir_Visit);
         end if;

         if Do_Dir_Visit or else Do_Subdir_Visit then
            Directories.Start_Search
              (Dir_Search, String (Dir), "",
               Filter => (Directory     => Do_Subdir_Visit,
                          Ordinary_File => Do_Dir_Visit,
                          Special_File  => False));

            while Directories.More_Entries (Dir_Search) loop
               Directories.Get_Next_Entry (Dir_Search, Dir_Entry);

               case Directories.Kind (Dir_Entry) is
                  when Ordinary_File =>
                     File_CB
                       (GPR2.Path_Name.Create_File
                          (Filename_Optional
                               (Directories.Full_Name (Dir_Entry)),
                           GPR2.Path_Name.No_Resolution));

                  when Directory =>
                     if Directories.Simple_Name (Dir_Entry) not in "." | ".."
                     then
                        Handle_Directory
                          (Filename_Type (Directories.Full_Name (Dir_Entry)),
                           Do_Subdir_Visit);
                     end if;

                  when Special_File =>
                     raise Program_Error;
               end case;
            end loop;

            Directories.End_Search (Dir_Search);
         end if;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Self.Tree.Append_Message
              (GPR2.Message.Create
                 (GPR2.Message.Error,
                  """" & String (Dir) & """ is not a valid directory",
                  Source));
      end Handle_Directory;

   begin
      Handle_Directory (Filename_Type (Root_Dir),
                        Recursive   => Recursive,
                        Is_Root_Dir => True);
   end Foreach;

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
        Name   : Attribute_Id;
        Pack   : Optional_Package_Id    := No_Package;
        Index  : Attribute_Index.Object := Attribute_Index.Undefined;
        At_Pos : Unit_Index             := No_Index) return Boolean
   is
   begin
      return Self.Attribute (Name, Pack, Index, At_Pos).Is_Defined;
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

   function Has_Mains (Self : Object) return Boolean
   is
      Attr : constant Project.Attribute.Object :=
               Self.Attribute (PRA.Main);
   begin
      if not Attr.Is_Defined then
         return False;
      else
         return Attr.Count_Values > 0;
      end if;
   end Has_Mains;

   ------------------
   -- Has_Packages --
   ------------------

   function Has_Packages
     (Self           : Object;
      Name           : Optional_Package_Id := No_Package;
      Check_Extended : Boolean := True;
      With_Defaults  : Boolean := True) return Boolean
   is
      View        : Object := Self;
      Def         : GPR2.Project.Registry.Attribute.Default_Rules;
      Has_Default : Boolean := False;
      Packages    : GPR2.Containers.Package_Id_List;

      procedure For_Rule (Attribute : Attribute_Id; Definition : PRA.Def);
      --  Check if the definition applies to Name in Self's context

      procedure For_Rule (Attribute : Attribute_Id; Definition : PRA.Def)
      is
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

      --  Check if the package has default values
      if With_Defaults then
         if Name = No_Package then
            Packages := PRA.Get_Packages_With_Default;
         else
            Packages.Include (Name);
         end if;

         for Pkg_Name of Packages loop
            Def := PRA.Get_Default_Rules (Name);

            --  Check if we can create a default value for package Name
            PRA.For_Each_Default (Def, For_Rule'Access);

            if Has_Default then
               return True;
            end if;
         end loop;
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
   end Has_Packages;

   ----------------
   -- Has_Source --
   ----------------

   function Has_Source
     (Self : Object; Filename : GPR2.Simple_Name) return Boolean is
   begin
      return Definition.Has_Source (Self, Filename);
   end Has_Source;

   -----------------
   -- Has_Sources --
   -----------------

   function Has_Sources (Self : Object) return Boolean is
      S : constant Project.Source.Set.Object := Self.Sources with Unreferenced;
      --  Let's compute the set of sources to be able to get the right answer
      --  below. Remember the sources are cached and computed only when
      --  requested.
   begin
      return not Definition.Get_RO (Self).Sources.Is_Empty;
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
      if not Self.Has_Packages (Pack, With_Defaults => False) then
         return False;
      end if;

      if Name = No_Name then
         return not Self.Pack (Pack).Vars.Is_Empty;
      else
         return Self.Pack (Pack).Vars.Contains (Name);
      end if;
   end Has_Variables;

   --------------------
   -- Hide_Unit_Body --
   --------------------

   procedure Hide_Unit_Body (Self : Object; Unit : Name_Type) is
      Ref : constant Definition.Ref := Definition.Get (Self);
      CU  : constant Unit_Info.Set.Cursor := Ref.Units.Find (Unit);
   begin
      if Unit_Info.Set.Set.Has_Element (CU) then
         Ref.Units (CU).Remove_Body;
      end if;
   end Hide_Unit_Body;

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
      use Definition_References;
   begin
      return Definition.Get_RO (Self).Extending /= Null_Weak_Ref;
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
      This : constant Definition.Const_Ref := Definition.Get_RO (Self);
   begin
      if not This.Extended_Root.Is_Defined then
         return False;
      end if;

      if not Parent.Is_Defined then
         return True;
      end if;

      for Ext of Self.Extended loop
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
               Self.Attribute
                 (Project.Registry.Attribute.Externally_Built);
   begin
      return Attr.Is_Defined and then Attr.Value_Equal ("true");
   end Is_Externally_Built;

   -------------
   -- Is_Main --
   -------------

   function Is_Main
     (Self : Object; Source : Project.Source.Object) return Boolean
   is
      Path  : constant GPR2.Path_Name.Object := Source.Path_Name;
      Mains : constant Project.Attribute.Object :=
                Self.Attribute (Registry.Attribute.Main);
   begin
      return Mains.Is_Defined
        and then
          (Mains.Has_Value (Value_Type (Path.Base_Name))
           or else Mains.Has_Value (Value_Type (Path.Simple_Name)));
   end Is_Main;

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
     (Self : Object) return GPR2.Path_Name.Object
   is
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

      Append
        (File_Name, Self.Attribute
           (PRA.Shared_Library_Prefix).Value.Text);

      --  Library name

      Append (File_Name, Self.Attribute (PRA.Library_Name).Value.Text);

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

   -----------
   -- Mains --
   -----------

   function Mains (Self : Object) return GPR2.Path_Name.Set.Object is
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

   function Namespace_Root (Self : Object) return Object is
   begin
      return Definition.Strong (Get_RO (Self).Root_View);
   end Namespace_Root;

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
     (Self           : Object;
      Name           : Package_Id) return Project.Pack.Object
   is
      View : Object := Self;
   begin
      loop
         if Definition.Get_RO (View).Has_Packages (Name) then
            return Definition.Get_RO (View).Packs.Element (Name);
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
     (Self : Object;
      With_Defaults : Boolean := True) return GPR2.Containers.Package_Id_List
   is
      Result : Containers.Package_Id_List;
   begin
      if Self.Is_Extending then
         Result := Self.Extended_Root.Packages (With_Defaults => False);
      end if;

      for Pack of Definition.Get_RO (Self).Packs loop
         Result.Include (Pack.Id);
      end loop;

      --  Check packages with default values
      if With_Defaults then
         for Pack of PRA.Get_Packages_With_Default loop
            --  Self.Has_Packages will check if the default values defined in
            --  the package apply to Self.Kind.

            if not Result.Contains (Pack)
              and then Self.Has_Packages (Pack, False, True)
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

   ------------------
   -- Reindex_Unit --
   ------------------

   procedure Reindex_Unit (Self : Object; From, To : Name_Type) is
      Ref : constant Definition.Ref := Definition.Get (Self);
      C   : constant Unit_Info.Set.Cursor := Ref.Units.Find (From);
   begin
      if Unit_Info.Set.Set.Has_Element (C) then
         Ref.Units.Include (To, Unit_Info.Set.Set.Element (C));
         Ref.Units.Delete (From);
      end if;

      Ref.Tree.Reindex_Unit (From, To);
   end Reindex_Unit;

   ------------------------
   -- Remove_Body_Suffix --
   ------------------------

   function Remove_Body_Suffix
     (Self : Object; Name : Simple_Name) return Value_Not_Empty
   is
      Last   : Positive := Name'First;
      Src    : GPR2.Project.Source.Object;
      Lang   : constant Language_Id :=
                 (if Self.Check_Source (Name, Src)
                  then Src.Language
                  else No_Language);
      Suffix : constant String :=
                 (if Lang /= No_Language
                  and then Self.Has_Body_Suffix (Lang)
                  then Self.Body_Suffix (Lang).Value.Text
                  else "");
   begin
      if Suffix'Length > 0
        and then Name'Length > Suffix'Length
        and then GPR2.Path_Name.To_OS_Case (Suffix) =
        GPR2.Path_Name.To_OS_Case
          (Ada.Strings.Fixed.Tail (String (Name), Suffix'Length))
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

   ------------
   -- Source --
   ------------

   function Source
     (Self : Object; File : GPR2.Path_Name.Object) return Project.Source.Object
   is
      CS : constant Definition.Simple_Name_Source.Cursor :=
             Definition.Get_RO (Self).Sources_Map.Find (File.Simple_Name);
   begin
      if Definition.Simple_Name_Source.Has_Element (CS) then
         return Definition.Simple_Name_Source.Element (CS);
      else
         return  Project.Source.Undefined;
      end if;
   end Source;

   ------------
   -- Source --
   ------------

   function Source
     (Self     : Object;
      Filename : GPR2.Simple_Name) return Project.Source.Object
   is
      Result : Project.Source.Object;
   begin
      if Definition.Check_Source (Self, Filename, Result) then
         return Result;
      end if;

      raise System.Assertions.Assert_Failure with
        "Source " & String (Filename) & " not found";
   end Source;

   ------------------------
   -- Source_Directories --
   ------------------------

   function Source_Directories
     (Self : Object) return Project.Attribute.Object is
   begin
      return Self.Attribute (PRA.Source_Dirs);
   end Source_Directories;

   -----------------
   -- Source_Path --
   -----------------

   function Source_Path
     (Self : Object; Filename : GPR2.Simple_Name) return GPR2.Path_Name.Object
   is
      CS : constant Definition.Simple_Name_Source.Cursor :=
             Definition.Get_RO (Self).Sources_Map.Find (Filename);
   begin
      if Definition.Simple_Name_Source.Has_Element (CS) then
         return Definition.Simple_Name_Source.Element (CS).Path_Name;
      else
         return GPR2.Path_Name.Undefined;
      end if;
   end Source_Path;

   function Source_Path
     (Self            : Object;
      Name            : GPR2.Simple_Name;
      Allow_Spec_File : Boolean;
      Allow_Unit_Name : Boolean) return GPR2.Path_Name.Object
   is
      CS : Definition.Simple_Name_Source.Cursor :=
             Definition.Get_RO (Self).Sources_Map.Find (Name);
   begin
      if Definition.Simple_Name_Source.Has_Element (CS) then
         return Definition.Simple_Name_Source.Element (CS).Path_Name;
      else
         if Allow_Unit_Name then
            declare
               Unit : constant Unit_Info.Object :=
                        Self.Unit (Name => Optional_Name_Type (Name));
            begin
               if Unit.Is_Defined then
                  if Unit.Has_Body then
                     return Unit.Main_Body.Source;
                  elsif Allow_Spec_File and then Unit.Has_Spec then
                     return Unit.Spec.Source;
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
                  CS := Definition.Get_RO (Self).Sources_Map.Find
                    (Name & Simple_Name (BS));
                  if Definition.Simple_Name_Source.Has_Element (CS) then
                     return Definition.Simple_Name_Source.Element
                       (CS).Path_Name;
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
                        CS := Definition.Get_RO (Self).Sources_Map.Find
                          (Name & Simple_Name (SS));
                        if Definition.Simple_Name_Source.Has_Element (CS) then
                           return Definition.Simple_Name_Source.Element
                             (CS).Path_Name;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end if;

      return GPR2.Path_Name.Undefined;
   end Source_Path;

   -------------------------
   -- Source_Subdirectory --
   -------------------------

   function Source_Subdirectory (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Object_Directory.Compose
        (Self.Tree.Src_Subdirs, Directory => True);
   end Source_Subdirectory;

   -------------
   -- Sources --
   -------------

   function Sources
     (Self   : Object;
      Filter : Source_Kind := K_All) return Project.Source.Set.Object
   is
      use type Ada.Streams.Stream_Element_Array;
      Data : constant Project.Definition.Ref := Project.Definition.Get (Self);
   begin
      if not Definition.Are_Sources_Loaded (Data.Tree.all) then
         Data.Tree.Update_Sources (With_Runtime => Self.Is_Runtime);

      elsif Data.Sources_Signature = GPR2.Context.Default_Signature then
         Data.Update_Sources
           (Self, Stop_On_Error => True, Backends => Source_Info.All_Backends);
      end if;

      --  Compute and return the sources depending on the filtering

      if Filter = K_All then
         return Data.Sources;

      else
         return S_Set : Project.Source.Set.Object do
            for S of Data.Sources loop
               declare
                  Is_Interface : constant Boolean :=
                                   S.Has_Units
                                   and then S.Has_Single_Unit
                                   and then Data.Units.Contains (S.Unit_Name)
                                   and then S.Is_Interface;
                  --  All sources related to an interface unit are also
                  --  taken as interface (not only the spec)???

               begin
                  if (Filter = K_Interface_Only and then Is_Interface)
                    or else
                      (Filter = K_Not_Interface and then not Is_Interface)
                  then
                     S_Set.Insert (S);
                  end if;
               end;
            end loop;
         end return;
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

   function Unit (Self : Object; Name : Name_Type) return Unit_Info.Object is
      CU : constant Unit_Info.Set.Cursor :=
             Definition.Get_RO (Self).Units.Find (Name);
   begin
      if Unit_Info.Set.Set.Has_Element (CU) then
         return Unit_Info.Set.Set.Element (CU);
      else
         return Unit_Info.Undefined;
      end if;
   end Unit;

   -----------
   -- Units --
   -----------

   function Units (Self : Object) return Unit_Info.Set.Object is
   begin
      return Definition.Get_RO (Self).Units;
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
   begin
      --  Lookup in the ancestors first

      while Dad.Is_Defined loop
         if Dad.Name = Name then
            return Dad;
         end if;

         Dad := Definition.Get_RO (Dad).Extended_Root;
      end loop;

      --  Lookup in the imported next

      declare
         package DPV renames Definition.Project_View_Store;
         Position : constant DPV.Cursor := Data.Imports.Find (Name);
      begin
         if DPV.Has_Element (Position) then
            return DPV.Element (Position);
         end if;
      end;

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
   Definition.Get_RO                 := Get_RO'Access;
   Definition.Get_RW                 := Get_RW'Access;
   Definition.Get                    := Get_Ref'Access;
   Definition.Set                    := Set_Def'Access;
   Definition.Refcount               := Refcount'Access;
   Definition.Weak                   := Weak'Access;
   Definition.Strong                 := Strong'Access;
end GPR2.Project.View;
