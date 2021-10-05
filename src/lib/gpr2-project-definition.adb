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

with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with GNAT.MD5;
with GNAT.OS_Lib;
with GNAT.Regexp;

with GPR2.Containers;
with GPR2.Unit.List;
with GPR2.Message;
with GPR2.Project.Attribute;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree;
with GPR2.Source;
with GPR2.Source_Info.Parser.Registry;
with GPR2.Source_Reference.Identifier.Set;
with GPR2.Source_Reference.Pack;
with GPR2.Source_Reference.Value;

with GNATCOLL.Utils;

package body GPR2.Project.Definition is

   use GNAT;

   package ACH renames Ada.Characters.Handling;
   package ASF renames Ada.Strings.Fixed;
   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;
   package SR  renames GPR2.Source_Reference;
   package SRI renames SR.Identifier;
   package SRP renames SR.Pack;

   Builtin_Naming_Package : Project.Pack.Object;
   --  The default naming package to use if no Naming package specified in the
   --  project and no configuration file loaded. We at least want to handle in
   --  this case the standard Ada and C namings.

   function Languages (Def : Data) return Containers.Source_Value_List is
     (Def.Attrs.Languages.Values);

   procedure Update_Sources_List
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean);

   procedure Update_Sources_Parse
     (Def : in out Data; Backends : Source_Info.Backend_Set);

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
           new Ada.Containers.Indefinite_Ordered_Maps
             (Value_Type, Language_Id);

         use type Suffix_Lang_Maps.Cursor;

         Suffix_Lang_Map : Suffix_Lang_Maps.Map;
         --  key=suffix value; value=first language registering suffix use
         --  map used to detect/report multiple use of a suffix.

         Naming_Package : constant GPR2.Project.Pack.Object :=
                             View.Naming_Package;
         --  view's naming package.

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
           (Attribute_Name : Attribute_Id;
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
            if Naming_Package.Check_Attribute (PRA.Casing, Result => Casing)
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
                                Naming_Package.Attribute (PRA.Dot_Replacement);
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

            if Value = No_Value then
               Log_Error
                 (Message.Error,
                  "Dot_Replacement cannot be empty",
                  Dot_Replacement);

            elsif ACH.Is_Alphanumeric (Value (Value'First))
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
           (Attribute_Name : Attribute_Id;
            Language       : Language_Id;
            Attribute      : Project.Attribute.Object)
         is
            Value    : constant Value_Type := Attribute.Value.Text;
            Dot_Repl : constant Value_Type :=
                         Naming_Package.Attribute
                            (PRA.Dot_Replacement).Value.Text;
         begin
            if Value'Length = 0 then
               if Attribute_Name = PRA.Separate_Suffix then
                  Log_Error
                    (Message.Error,
                     "Separate_Suffix cannot be empty",
                     Attribute);
               end if;
            elsif ASF.Index (Value, ".") = 0 then
               Log_Error
                 (Message.Error,
                  """" & Value & """ is illegal for "
                  & Image (Attribute_Name) & ": must have a dot",
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
                           & Image (Attribute_Name)
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
            begin
               if Associated_Lang /= Suffix_Lang_Maps.No_Element then
                  if Attribute_Name = PRA.Separate_Suffix
                    and then Naming_Package.Has_Body_Suffix (Ada_Language)
                    and then Naming_Package.Body_Suffix
                      (Ada_Language).Value.Text = Value
                  then
                     return;
                  end if;

                  Log_Error
                    (Message.Error,
                     Image (Attribute_Name) & "(""" & String (Value)
                     & """) for language " & Image (Language)
                     & " is also defined for language "
                     & Image (Suffix_Lang_Maps.Element (Associated_Lang)),
                     Attribute);
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
         if View.Has_Packages (PRP.Naming) then
            Check_Casing;
            Check_Dot_Replacement;

            if View.Kind /= K_Aggregate and then View.Has_Languages then
               for L of View.Languages loop
                  declare
                     Language : constant Language_Id := +Name_Type (L.Text);
                  begin
                     if Naming_Package.Has_Spec_Suffix (Language) then
                        Check_Illegal_Suffix
                          (PRA.Spec_Suffix,
                           Language,
                           Naming_Package.Spec_Suffix (Language));
                     end if;

                     if Naming_Package.Has_Body_Suffix (Language) then
                        Check_Illegal_Suffix
                          (PRA.Body_Suffix,
                           Language,
                           Naming_Package.Body_Suffix (Language));
                     end if;
                  end;
               end loop;
            end if;

            if Naming_Package.Has_Separate_Suffix then
               Check_Illegal_Suffix
                 (PRA.Separate_Suffix,
                  Ada_Language,
                  Naming_Package.Separate_Suffix);
            end if;
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
      Def.Cache := (others => <>);
   end Clear_Cache;

   -----------------------
   -- Is_Sources_Loaded --
   -----------------------

   function Is_Sources_Loaded (View : Project.View.Object) return Boolean is
   begin
      return not Get_RO (View).Sources_Map.Is_Empty;
   end Is_Sources_Loaded;

   --------------------
   -- Naming_Package --
   --------------------

   function Naming_Package (Def : Data) return Project.Pack.Object is
      CP : constant Pack.Set.Set.Cursor := Def.Packs.Find (PRP.Naming);
   begin
      if Pack.Set.Set.Has_Element (CP) then
         return Pack.Set.Set.Element (CP);

      elsif Def.Extended_Root.Is_Defined then
         return Naming_Package (Get_RO (Def.Extended_Root).all);

      elsif Def.Tree.Has_Configuration
        and then Def.Tree.Configuration.Corresponding_View.Has_Packages
                   (PRP.Naming)
      then
         return Def.Tree.Configuration.Corresponding_View.Naming_Package;

      else
         return Result : Project.Pack.Object := Builtin_Naming_Package do
            Definition.Set_Pack_Default_Attributes (Result, Def);
         end return;
      end if;
   end Naming_Package;

   ----------------------------
   -- Set_Default_Attributes --
   ----------------------------

   procedure Set_Default_Attributes (Def : in out Data) is

      procedure Inherite_Attribute (Name : Attribute_Id);
      --  Take attribute from extended project and put it into current one
      --  if it exists in extended and is not defined in the current one.

      procedure Union_Attribute (Name : Attribute_Id);
      --  Union attribute values to the current project from the extended one

      ------------------------
      -- Inherite_Attribute --
      ------------------------

      procedure Inherite_Attribute (Name : Attribute_Id) is
         Attr : Attribute.Object;
      begin
         if not Def.Attrs.Contains (Name)
           and then Def.Extended_Root.Check_Attribute (Name, Result => Attr)
         then
            Def.Attrs.Insert (Attr);
         end if;
      end Inherite_Attribute;

      ---------------------
      -- Union_Attribute --
      ---------------------

      procedure Union_Attribute (Name : Attribute_Id) is
         Parent : Attribute.Object;
         CT     : constant Attribute.Set.Cursor := Def.Attrs.Find (Name);
      begin
         if Def.Extended_Root.Check_Attribute (Name, Result => Parent) then
            if Attribute.Set.Has_Element (CT) then
               for V of Parent.Values loop
                  if not Def.Attrs (CT).Has_Value (V.Text) then
                     Def.Attrs (CT).Append (V);
                  end if;
               end loop;

            else
               Def.Attrs.Insert (Parent);
            end if;
         end if;
      end Union_Attribute;

   begin
      if Def.Extended_Root.Is_Defined then
         Union_Attribute (PRA.Languages);

         case Def.Kind is
            when K_Library | K_Aggregate_Library =>
               Inherite_Attribute (PRA.Library_Name);
            when K_Standard =>
               Inherite_Attribute (PRA.Main);
            when others =>
               null;
         end case;
      end if;

      Set_Defaults (Def.Attrs, Def, No_Package);

      for Pack of Def.Packs loop
         Definition.Set_Pack_Default_Attributes (Pack, Def);
      end loop;
   end Set_Default_Attributes;

   -----------------------
   -- Source_Map_Insert --
   -----------------------

   procedure Sources_Map_Insert
     (Def : in out Data;
      Src : Project.Source.Object)
   is
      Position : Simple_Name_Source.Cursor;
      Inserted : Boolean;
   begin
      Def.Sources_Map.Insert
        (Src.Path_Name.Simple_Name, Src, Position, Inserted);
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
      Source_Info.Parser.Registry.Clear_Cache;
      --  If the view is extended, we will use the ALI from the extending
      --  project. We still need to call Update_Sources_Parse to disambiguate
      --  SPEC/SPEC_ONLY and BODY/BODY_ONLY units.
      Update_Sources_Parse
        (Def,
         (if View.Is_Extended
          then Source_Info.No_Backends
          else Backends));
   end Update_Sources;

   -------------------------
   -- Update_Sources_List --
   -------------------------

   procedure Update_Sources_List
     (Def           : in out Data;
      View          : Project.View.Object;
      Stop_On_Error : Boolean)
   is
      use type MD5.Binary_Message_Digest;
      use type Project.Attribute.Object;

      use GPR2.Containers;
      use GPR2.Path_Name;

      use type SR.Object;

      Root : constant GPR2.Path_Name.Object := Def.Path;

      package Lang_Boolean_Map is new Ada.Containers.Hashed_Maps
        (Language_Id, Boolean, Hash, "=");

      package Unit_Name_To_Sloc is new
        Ada.Containers.Indefinite_Ordered_Maps (Name_Type, SR.Object);
      --  Used for the Interface_Units container which will initially store all
      --  the units from the Library_Interface attribute, as a mapping from
      --  unit names to slocs.

      package Source_Path_To_Sloc is new
        Ada.Containers.Indefinite_Ordered_Maps (Filename_Type, SR.Object);
      --  Same as above but for the Interfaces attribute, so here we are using
      --  Filename_Type instead of Name_Type since we're dealing with
      --  filenames.

      package Attribute_List is new
        Ada.Containers.Doubly_Linked_Lists (Project.Attribute.Object);
      --  Element type for Source_Path_To_Attribute_List below

      package Source_Path_To_Attribute_List is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Key_Type     => Filename_Type,
           Element_Type => Attribute_List.List,
           "="          => Attribute_List."=");
      --  Used for the Ada_Naming_Exceptions container which maps a filename to
      --  the list of naming attributes (Body/Spec) that reference it.

      package Naming_Exceptions_Usage renames Value_Source_Reference_Package;

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

      procedure Handle_File (File : GPR2.Path_Name.Object);
      --  Processes the given file: see if it should be added to the view's
      --  sources, and compute information such as language/unit(s)/...

      function Signature return MD5.Binary_Message_Digest;
      --  Compute the signature corresponding to the source context. If the
      --  signature is not the same recorded for the view, the source set
      --  need to be recomputed.

      procedure Read_Source_List
        (Attr_Name : Attribute_Id;
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

      procedure Fill_Ada_Naming_Exceptions (Attr : Attribute_Id)
        with Pre => Attr in  PRA.Spec | PRA.Body_N;
      --  Fill the Ada_Naming_Exceptions object with the given attribute set

      procedure Fill_Other_Naming_Exceptions
        (Set : Project.Attribute.Set.Object)
        with Pre =>
          (for all A of Set =>
             A.Name.Id = PRA.Specification_Exceptions
             or else A.Name.Id = PRA.Implementation_Exceptions);

      function Is_Compilable (Language : Language_Id) return Boolean;
      --  Check whether the language is compilable on the current View. This
      --  includes information provided by the Tree (Driver attribute). Note
      --  that this routine caches the result into a map.

      Naming : constant Project.Pack.Object := Naming_Package (Def);
      --  Package Naming for the view

      Dot_Repl : constant String :=
                   Naming.Attribute (PRA.Dot_Replacement).Value.Text;
      --  Get Dot_Replacement value

      Is_Standard_GNAT_Naming : constant  Boolean :=
                                  (Naming.Spec_Suffix
                                       (Ada_Language).Value.Text = ".ads")
                                     and then
                                  (Naming.Body_Suffix
                                       (Ada_Language).Value.Text = ".adb")
                                     and then
                                  (Dot_Repl = "-");
      --  True if the current naming scheme is GNAT's default naming scheme.
      --  This is to take into account shortened names like "Ada." (a-),
      --  "System." (s-) and so on.

      Source_Dir_Ref    : SR.Object;

      Included_Sources  : Source_Set.Set;
      Excluded_Sources  : Source_Set.Set;
      Has_Source_List   : Boolean := False;
      --  Has either Source_Files or Source_List_File attributes

      Interface_Units       : Unit_Name_To_Sloc.Map;
      Position_In_Units     : Unit_Name_To_Sloc.Cursor;
      Inserted              : Boolean;
      Interface_Units_Found : Name_Set;
      Interface_Found       : Boolean := False;
      Interface_Sources     : Source_Path_To_Sloc.Map;
      Position_In_Sources   : Source_Path_To_Sloc.Cursor;
      Language_Compilable   : Lang_Boolean_Map.Map;
      Src_Dir_Set           : Source.Set.Object;
      --  Sources from one directory defined in one item of the Source_Dirs
      --  attribute. Need to avoid source duplications in Source_Dirs items
      --  containing '*' character.
      Has_Src_In_Lang       : Language_Set;
      --  Insert record there if the language has a source

      Tree                  : constant not null access Project.Tree.Object :=
                                Def.Tree;
      Message_Count         : constant Containers.Count_Type :=
                                Tree.Log_Messages.Count;

      Ada_Naming_Exceptions : Source_Path_To_Attribute_List.Map;
      Ada_Except_Usage      : Naming_Exceptions_Usage.Map;
      Other_Except_Usage    : Filename_Source_Reference;

      Visited_Dirs          : GPR2.Containers.Filename_Set;
      --  List of already visited directories to avoid looking twice at the
      --  same one.

      procedure Mark_Language (Lang : Language_Id);
      --  Mark that language exists in sources

      function Ada_Use_Index (Attr : Attribute.Object) return Value_Type is
        (Attr.Index.Text & Image (Attr.Name.Id) (1));
      --  Index created from Body or Spec attribute index i.e. Ada unit name
      --  and first character of the attribute name i.e. B or S. It is used to
      --  distinct body naming exception from spec naming exception.

      --------------------------------
      -- Fill_Ada_Naming_Exceptions --
      --------------------------------

      procedure Fill_Ada_Naming_Exceptions (Attr : Attribute_Id) is
      begin
         for CA in Naming.Attributes.Iterate (Attr, With_Defaults => True) loop
            declare
               A               : constant Attribute.Object :=
                                   Attribute.Set.Element (CA);
               Source          : constant Filename_Type :=
                                   Filename_Type (A.Value.Text);
               Attributes      : Attribute_List.List :=
                                   Attribute_List.Empty_List;
               Insert_Position : Source_Path_To_Attribute_List.Cursor;
               Is_Inserted     : Boolean;
            begin
               Attributes.Append (A);

               Ada_Naming_Exceptions.Insert
                 (Key      => Source,
                  New_Item => Attributes,
                  Position => Insert_Position,
                  Inserted => Is_Inserted);

               if not Is_Inserted then
                  Ada_Naming_Exceptions (Insert_Position).Append (A);
               end if;

               Ada_Except_Usage.Insert (Ada_Use_Index (A), A.Value);
            end;
         end loop;
      end Fill_Ada_Naming_Exceptions;

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

      procedure Handle_File (File : GPR2.Path_Name.Object) is
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
               if Naming.Check_Attribute
                    (PRA.Specification_Exceptions,
                     Attribute_Index.Create (Value_Type (Name (Language))),
                     Result => Attr)
                 and then Attr.Has_Value (Value_Type (Basename))
               then
                  Match := True;
                  Kind  := Unit.S_Spec;

               elsif Naming.Check_Attribute
                    (PRA.Implementation_Exceptions,
                     Attribute_Index.Create (Value_Type (Name (Language))),
                     Result => Attr)
                 and then Attr.Has_Value (Value_Type (Basename))
               then
                  Match := True;
                  Kind  := Unit.S_Body;
               end if;

               if Match then
                  Other_Except_Usage.Delete (Basename);
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
                             (Naming.Attribute (PRA.Casing).Value.Text);
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
                    and then Basename (J .. J + Dot_Repl'Length - 1)
                    = Dot_Repl
                  then
                     J := J + Dot_Repl'Length;
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
            Has_Spec_Suffix  : constant Boolean :=
                                 Naming.Has_Spec_Suffix (Language);
            Spec_Suffix      : constant String :=
                                 (if Has_Spec_Suffix
                                  then Naming.Spec_Suffix (Language).Value.Text
                                  else "");
            Has_Body_Suffix  : constant Boolean :=
                                 Naming.Has_Body_Suffix (Language);
            Body_Suffix      : constant String :=
                                 (if Has_Body_Suffix
                                  then Naming.Body_Suffix (Language).Value.Text
                                  else "");
            Has_Sep_Suffix   : constant Boolean := Language = Ada_Language
                                 and then Naming.Has_Separate_Suffix;
            Sep_Suffix       : constant String :=
                                 (if Has_Sep_Suffix
                                  then Naming.Separate_Suffix.Value.Text
                                  else "");
            use GNATCOLL.Utils;

         begin
            Matches_Spec := Has_Spec_Suffix
              and then Ends_With (Basename, Spec_Suffix);

            Matches_Body := Has_Body_Suffix
              and then Ends_With (Basename, Body_Suffix);

            Matches_Separate := Has_Sep_Suffix
              and then Ends_With (Basename, Sep_Suffix);

            --  See GA05-012: if there's ambiguity with suffixes (e.g. one of
            --  the suffixes if a suffix of another) we use with the most
            --  explicit one (e.g. the longest one) that matches.

            if Matches_Spec and then Matches_Body then
               if Spec_Suffix'Length >= Body_Suffix'Length then
                  pragma Assert (Ends_With (Spec_Suffix, Body_Suffix));
                  Matches_Body := False;
               else
                  pragma Assert (Ends_With (Body_Suffix, Spec_Suffix));
                  Matches_Spec := False;
               end if;
            end if;

            if Matches_Spec and then Matches_Separate then
               if Spec_Suffix'Length >= Sep_Suffix'Length then
                  pragma Assert (Ends_With (Spec_Suffix, Sep_Suffix));
                  Matches_Separate := False;
               else
                  pragma Assert (Ends_With (Sep_Suffix, Spec_Suffix));
                  Matches_Spec := False;
               end if;
            end if;

            if Matches_Body and then Matches_Separate then
               if Body_Suffix'Length >= Sep_Suffix'Length then
                  pragma Assert (Ends_With (Body_Suffix, Sep_Suffix));
                  Matches_Separate := False;
               else
                  pragma Assert (Ends_With (Sep_Suffix, Body_Suffix));
                  Matches_Body := False;
               end if;
            end if;

            --  Additional check: dot replacement and charset
            if Language = Ada_Language then
               if Matches_Spec then
                  Matches_Spec := Test_Charset (Spec_Suffix);
               elsif Matches_Body then
                  Matches_Body := Test_Charset (Body_Suffix);
               elsif Matches_Separate then
                  Matches_Separate := Test_Charset (Sep_Suffix);
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
                                Naming.Spec_Suffix (Ada_Language).Value.Text,
                              when Unit.Body_Kind =>
                                Naming.Body_Suffix (Ada_Language).Value.Text,
                              when S_Separate     =>
                                Naming.Separate_Suffix.Value.Text);
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
                  S3 : constant Character := Element (Result, 3);

               begin
                  if S1 in 'a' | 'g' | 'i' | 's' then
                     --  Children or separates of packages A, G, I or S. These
                     --  names are x__ ... or x~... (where x is a, g, i, or s).
                     --  Both versions (x__... and x~...) are allowed in all
                     --  platforms, because it is not possible to know the
                     --  platform before processing the project files.

                     if S2 = '_' and then S3 = '_' then
                        Replace_Slice (Result, 2, 3, ".");

                     elsif S2 = '~' then
                        Replace_Element (Result, 2, '.');
                     end if;

                     --  We do nothing with S2 = '.' case here because it can
                     --  be regular package with one letter named parent. We
                     --  will detect runtime source later at unit name
                     --  clarification stage in Ada or ALI parser.
                  end if;
               end;
            end if;

            --  Some additional checks on the unit name

            if not Is_Valid_Unit_Name (Name_Type (To_String (Result))) then
               goto Invalid;
            end if;

            Success := True;

            return Name_Type (To_Mixed (To_String (Result)));

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
                 (Message.Create (Message.Error, Text, Source_Dir_Ref));
            end On_Error;

         begin
            return Unit.Valid_Unit_Name (Unit_Name, On_Error'Access);
         end Is_Valid_Unit_Name;

         Languages : constant Project.Attribute.Object := Def.Attrs.Languages;

         Basename  : constant Filename_Type := File.Simple_Name;

         Match                  : Boolean := False;

         Source_Is_In_Interface : Boolean := False;
         Naming_Exception       : Project.Source.Naming_Exception_Kind := No;
         Units                  : Unit.List.Object;  --  For Ada
         Kind                   : Unit.Library_Unit_Type;
         Source                 : GPR2.Source.Object;

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

                              Kind := (if Exc.Name.Id = PRA.Spec
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
                        Unit_Idx  : constant Attribute_Index.Object :=
                                      Attribute_Index.Create
                                        (Value_Type (Unit_Name));

                        function Has_Conflict_NE
                          (Attr_Name : Attribute_Id) return Boolean;
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
                          (Attr_Name : Attribute_Id) return Boolean
                        is
                           Attr : Project.Attribute.Object;
                        begin
                           if Naming.Check_Attribute
                             (Attr_Name, Unit_Idx, Result => Attr)
                           then
                              if not Naming_Exception_Equal
                                (Attr, Value_Type (Basename), 1)
                              then
                                 return True;
                              end if;
                           end if;

                           return False;
                        end Has_Conflict_NE;

                     begin
                        if Match then
                           --  Check if we have conflicting naming exceptions:
                           --  same (unit,kind) but different source.
                           --  In this case we skip this source.

                           if (Kind = Unit.S_Spec
                               and then Has_Conflict_NE (PRA.Spec))
                             or else
                               (Kind = Unit.S_Body
                                and then Has_Conflict_NE (PRA.Body_N))
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

                  Source_Is_In_Interface :=
                    Interface_Sources.Contains (Basename);
                  --  Different Source constructors for Ada and other
                  --  languages. Also some additional checks for Ada.

                  if Language = Ada_Language then
                     for CU of Units loop
                        if Interface_Units.Contains (CU.Name) then
                           Interface_Units_Found.Include (CU.Name);
                           Source_Is_In_Interface := True;
                        end if;
                     end loop;

                     if Is_Indexed then
                        Source := GPR2.Source.Object
                          (GPR2.Source.Create_Ada
                             (Filename      => File,
                              Units         => Units));
                     else
                        Source := GPR2.Source.Object
                          (GPR2.Source.Create_Ada
                             (Filename      => File,
                              Unit          => Units (No_Index),
                              Is_RTS_Source => View.Is_Runtime));
                     end if;

                  else
                     Source := GPR2.Source.Object
                       (GPR2.Source.Create (File, Language, Kind));
                  end if;

                  --  Final processing

                  if Source_Is_In_Interface then
                     Interface_Sources.Exclude (Basename);
                  end if;

                  declare
                     Is_Interface : constant Boolean :=
                                      Source_Is_In_Interface
                                          or else
                                      (not Interface_Found
                                       and then View.Kind in K_Library
                                       and then
                                         (not Source.Has_Units or else
                                          not Source.Units.Is_Indexed_List)
                                       and then Source.Kind in
                                                  Unit.Spec_Kind);
                     Project_Source : constant GPR2.Project.Source.Object :=
                                        Project.Source.Create
                                          (Source           => Source,
                                           View             => View,
                                           Is_Interface     => Is_Interface,
                                           Naming_Exception =>
                                             Naming_Exception,
                                           Is_Compilable    => Is_Compilable
                                                                 (Language));

                     --  Check source duplication and insert if possible or
                     --  replace if necessary.

                     CS             : constant Project.Source.Set.Cursor :=
                                        Src_Dir_Set.Find (Project_Source);
                  begin
                     if Project.Source.Set.Has_Element (CS) then
                        if not Src_Dir_Set (CS).Has_Naming_Exception
                          and then Project_Source.Has_Naming_Exception
                        then
                           --  We are here only when
                           --  Src_Dir_Set (CS).Has_Naming_Exception is False
                           --  and Project_Source.Has_Naming_Exception is True.
                           --  Module with naming exception has priority after
                           --  default naming. Replace the old source with the
                           --  new one.

                           Src_Dir_Set.Replace (Project_Source);

                        elsif Src_Dir_Set (CS).Has_Naming_Exception
                          = Project_Source.Has_Naming_Exception
                          and then not Def.Sources.Contains (Project_Source)
                        then
                           --  We are here when duplicated sources have naming
                           --  exception or does not have it both.
                           --  and Project_Source not already in Def.Sources

                           Tree.Append_Message
                             (Message.Create
                                (Message.Error,
                                 '"' & String (File.Simple_Name) & '"'
                                 & " is found in several source directories",
                                 Source_Dir_Ref));
                           return;

                        else
                           --  Remains condition when old source has naming
                           --  exception but new one does not have it. We don't
                           --  need to do anything because of more priority
                           --  source already in its place.

                           return;
                        end if;

                     else
                        Src_Dir_Set.Insert (Project_Source);
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
           (View : in out Project.View.Object; Source : Project.Source.Object);

         ----------------
         -- Add_Source --
         ----------------

         procedure Add_Source (Src : Project.Source.Object) is
            --
            --  TODO: avoid the code duplication from Handle_File
            --

            File                   : constant Path_Name.Object :=
                                       Src.Path_Name;
            Basename               : constant Filename_Type :=
                                       File.Simple_Name;
            Language               : constant Language_Id :=
                                       Src.Language;
            Source_Is_In_Interface : Boolean :=
                                       Interface_Sources.Contains (Basename);

         begin
            Mark_Language (Language);

            --  Different Source constructors for Ada and other
            --  languages. Also some additional checks for Ada.

            if Language = Ada_Language then
               for CU of Src.Units loop
                  if Interface_Units.Contains (CU.Name) then
                     Interface_Units_Found.Include (CU.Name);
                     Source_Is_In_Interface := True;
                  end if;
               end loop;
            end if;

            --  Final processing

            if Source_Is_In_Interface then
               Interface_Sources.Exclude (Basename);
            end if;

            Def.Sources.Insert (Src);
            Def.Sources_Map_Insert (Src);

            --  For Ada, register the Unit object into the view

            if Language = Ada_Language then
               Register_Units (Src);
            end if;
         end Add_Source;

         -------------------------
         -- Exclude_Recursively --
         -------------------------

         procedure Exclude_Recursively
           (View : in out Project.View.Object; Source : Project.Source.Object)
         is
            Def : constant Ref := Get_RW (View);
         begin
            Def.Sources.Delete (Source);
            Def.Sources_Map.Delete (Source.Path_Name.Simple_Name);

            Remove_Source (Source);

            if Def.Extended_Root.Is_Defined then
               Exclude_Recursively (Def.Extended_Root, Source);
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
            Pck : Project.Pack.Object;
            Att : Project.Attribute.Object;
         begin
            if View.Has_Packages (PRP.Compiler) then
               Pck := View.Pack (PRP.Compiler);

               if Pck.Check_Attribute
                 (PRA.Driver,
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
        (Attr_Name : Attribute_Id;
         Set       : in out Source_Set.Set)
      is
         Attr_Value : constant SR.Value.Object :=
                        Def.Attrs.Element (Attr_Name).Value;
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

            procedure Add (Attribute_Name : Attribute_Id);
            --  Add attribute by into the MD5 context

            ---------
            -- Add --
            ---------

            procedure Add (A : Project.Attribute.Object) is
            begin
               MD5.Update (C, String (Name (A.Name.Id)) & "/");
               for Value of A.Values loop
                  MD5.Update (C, Value.Text);
               end loop;
            end Add;

            procedure Add (Attribute_Name : Attribute_Id) is
               Attr : constant Project.Attribute.Object :=
                        Data.Attrs.Element (Attribute_Name);
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
                  Naming : constant Project.Pack.Object :=
                             Data.Packs (PRP.Naming);
               begin
                  if Naming.Check_Attribute
                       (PRA.Dot_Replacement, Result => Attr)
                  then
                     Add (Attr);
                  end if;

                  for A of Naming.Attributes (PRA.Spec_Suffix) loop
                     Add (A);
                  end loop;

                  for A of Naming.Attributes (PRA.Body_Suffix) loop
                     Add (A);
                  end loop;

                  for A of Naming.Attributes (PRA.Separate_Suffix) loop
                     Add (A);
                  end loop;

                  for CA in Naming.Attributes.Iterate
                              (PRA.Spec, With_Defaults => True)
                  loop
                     Add (Attribute.Set.Element (CA));
                  end loop;

                  for CA in Naming.Attributes.Iterate
                              (PRA.Body_N, With_Defaults => True)
                  loop
                     Add (Attribute.Set.Element (CA));
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

      --  Setup the naming exceptions look-up table if needed

      Fill_Ada_Naming_Exceptions (PRA.Spec);
      Fill_Ada_Naming_Exceptions (PRA.Body_N);

      Fill_Other_Naming_Exceptions
        (Naming.Attributes (PRA.Specification_Exceptions));
      Fill_Other_Naming_Exceptions
        (Naming.Attributes (PRA.Implementation_Exceptions));

      --  Record units being set as interfaces, first for Library_Interface
      --  which contains unit names.

      if Def.Attrs.Has_Library_Interface then
         Interface_Found := True;

         for Unit of Def.Attrs.Library_Interface.Values loop
            Interface_Units.Insert
              (Name_Type (Unit.Text), SR.Object (Unit),
               Position_In_Units, Inserted);

            if not Inserted then
               Tree.Append_Message
                 (Message.Create
                    (Message.Warning,
                     "duplicate unit '" & Unit.Text
                     & "' in library_interface attribute",
                     Unit));
            end if;
         end loop;
      end if;

      --  And then for Interfaces which contains filenames

      if Def.Attrs.Has_Interfaces then
         Interface_Found := True;

         for Source of Def.Attrs.Interfaces.Values loop
            Interface_Sources.Insert
              (Filename_Type (Source.Text), SR.Object (Source),
               Position_In_Sources, Inserted);

            if not Inserted then
               Tree.Append_Message
                 (Message.Create
                    (Message.Warning,
                     "duplicate source '" & Source.Text
                     & "' in interfaces attribute",
                     Source));
            end if;
         end loop;
      end if;

      --  Read sources and set up the corresponding definition

      --  First reset the current set

      Def.Sources.Clear;
      Def.Sources_Map.Clear;

      --  Clear the units record, note that we also want to record the
      --  unit_name -> view lookup table in the tree.

      for U of Def.Units loop
         Def.Tree.Clear_View (Unit => U);
      end loop;

      Def.Units.Clear;

      --  If we have attribute Excluded_Source_List_File

      if Def.Attrs.Has_Excluded_Source_List_File then
         Read_Source_List (PRA.Excluded_Source_List_File, Excluded_Sources);
      end if;

      --  If we have attribute Excluded_Source_Files

      if Def.Attrs.Has_Excluded_Source_Files then
         for File of Def.Attrs.Excluded_Source_Files.Values loop
            Include_Simple_Filename (Excluded_Sources, File.Text, File);
         end loop;
      end if;

      --  Remove naming exception sources from inactive case alternatives

      for File of Def.Trees.Project.Skip_Sources loop
         Include_Simple_Filename (Excluded_Sources, File.Text, File);
      end loop;

      --  If we have attribute Source_List_File

      if Def.Attrs.Has_Source_List_File then
         Read_Source_List (PRA.Source_List_File, Included_Sources);

         Has_Source_List := True;
      end if;

      --  If we have attribute Source_Files

      if Def.Attrs.Has_Source_Files then
         for File of Def.Attrs.Source_Files.Values loop
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
               In_Interface : Boolean      := False;
               A_Set        : Project.Source.Set.Object;
            begin
               Update_Sources
                 (DA.all, Agg, Stop_On_Error => True,
                  Backends => Source_Info.No_Backends);

               for P of Agg.Sources loop
                  In_Interface :=
                    Interface_Sources.Contains
                      (P.Path_Name.Simple_Name);

                  if P.Has_Units then
                     for CU of P.Units loop
                        if Interface_Units.Contains (CU.Name) then
                           Interface_Units_Found.Include (CU.Name);
                           In_Interface := True;
                        end if;
                     end loop;
                  end if;

                  declare
                     use all type Unit.Library_Unit_Type;

                     Is_Interface : constant Boolean :=
                                      In_Interface
                                          or else
                                            (not Interface_Found
                                             and then P.Kind
                                                      in Unit.Spec_Kind);

                  begin
                     --  An aggregate library project does not allow naming
                     --  exception. So the source naming exception status is
                     --  the one from the aggregated project.

                     A_Set.Insert
                       (Project.Source.Create
                          (Source           => GPR2.Source.Object (P),
                           View             => View,
                           Is_Interface     => Is_Interface,
                           Naming_Exception => P.Naming_Exception,
                           Is_Compilable    => P.Is_Compilable,
                           Aggregated       => P.View));
                  end;
               end loop;

               Insert
                 (A_Set,
                  Aggregated_Copy,
                  (if DA.Attrs.Has_Source_Dirs
                   then DA.Attrs.Source_Dirs
                   else Source_Reference.Create
                     (DA.Trees.Project.Path_Name.Value, 0, 0)));
            end;
         end loop;

      else
         --  Handle Source_Dirs

         declare
            use GNAT.Regexp;

            package Regexp_List is new Ada.Containers.Indefinite_Vectors
              (Positive, GNAT.Regexp.Regexp);

            Ignored_Sub_Dirs_Values  : GPR2.Project.Attribute.Object;
            --  Ignore_Source_Sub_Dirs attribute values

            Ignored_Sub_Dirs_Regexps : Regexp_List.Vector;
            --  Ignore_Source_Sub_Dirs attribute regexps

         begin
            --  Fill Ignored_Sub_Dirs_Regexps vector. "" pattern is not added

            if View.Check_Attribute
              (Name   => PRA.Ignore_Source_Sub_Dirs,
               Result => Ignored_Sub_Dirs_Values)
            then
               for V of Ignored_Sub_Dirs_Values.Values loop
                  if V.Text /= "" then
                     Ignored_Sub_Dirs_Regexps.Append
                       (GPR2.Compile_Regexp (Filename_Optional (V.Text)));
                  end if;
               end loop;
            end if;

            for Dir of View.Source_Directories.Values loop
               --  Keep reference for error messages

               Source_Dir_Ref := SR.Object (Dir);

               declare

                  procedure Is_Directory_Handled
                    (Directory       : GPR2.Path_Name.Object;
                     Is_Root_Dir     : Boolean;
                     Do_Dir_Visit    : in out Boolean;
                     Do_Subdir_Visit : in out Boolean);

                  --------------------------
                  -- Is_Directory_Handled --
                  --------------------------

                  procedure Is_Directory_Handled
                    (Directory       : GPR2.Path_Name.Object;
                     Is_Root_Dir     : Boolean;
                     Do_Dir_Visit    : in out Boolean;
                     Do_Subdir_Visit : in out Boolean)
                  is
                     Position : GPR2.Containers.Filename_Type_Set.Cursor;
                  begin
                     --  Handle ignored source dirs

                     if not Is_Root_Dir then
                        for Ignored_Sub_Dir of Ignored_Sub_Dirs_Regexps loop
                           if GNAT.Regexp.Match
                             (String (Directory.Simple_Name), Ignored_Sub_Dir)
                           then
                              --  Ignore this matching sub dir tree.

                              Do_Dir_Visit := False;
                              Do_Subdir_Visit := False;
                              return;
                           end if;
                        end loop;
                     end if;

                     --  If Directory already inserted in Visited_Dirs,
                     --  Do_Dir_Visit is set to False.

                     Visited_Dirs.Insert (New_Item => Directory.Name,
                                          Position => Position,
                                          Inserted => Do_Dir_Visit);
                  end Is_Directory_Handled;

               begin
                  View.Foreach
                    (Directory_Pattern  => GPR2.Filename_Optional (Dir.Text),
                     Source             => Dir,
                     File_CB            => Handle_File'Access,
                     Directory_CB       => Is_Directory_Handled'Access);
               end;

               Def.Sources.Union (Src_Dir_Set);

               for S of Src_Dir_Set loop
                  Def.Sources_Map_Insert (S);
               end loop;

               Src_Dir_Set.Clear;
            end loop;

         end;

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
                        (if Def.Attrs.Has_Source_List_File
                         then Def.Attrs.Source_List_File
                         else Def.Attrs.Source_Files)));
               end if;
            end loop;
         end if;

         if View.Has_Packages (PRP.Naming) then
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
                       ((if View.Has_Attributes (PRA.Source_Files)
                            or else View.Has_Attributes (PRA.Source_List_File)
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

      if Def.Attrs.Languages.Is_Defined
        and then Def.Kind not in K_Abstract | K_Configuration
        and then not Def.Attrs.Source_Dirs.Values.Is_Empty
        and then Excluded_Sources.Is_Empty
      then
         declare
            SF : constant Attribute.Object := Def.Attrs.Source_Files;
         begin
            if not SF.Is_Defined or else not SF.Values.Is_Empty then
               for L of Def.Languages loop
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

      --  And update the interface units bookkeeping

      for U of Interface_Units_Found loop
         Interface_Units.Exclude (U);
      end loop;

      --  Check that all unit and source interfaces have been found in the
      --  project view.

      for Cur in Interface_Units.Iterate loop
         declare
            Sloc      : constant SR.Object := Unit_Name_To_Sloc.Element (Cur);
            Unit_Name : constant Name_Type := Unit_Name_To_Sloc.Key (Cur);
         begin
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  "source for interface unit '" & String (Unit_Name)
                  & "' not found",
                  Sloc));
         end;
      end loop;

      for Cur in Interface_Sources.Iterate loop
         declare
            Sloc        : constant SR.Object :=
                            Source_Path_To_Sloc.Element (Cur);
            Source_Path : constant Filename_Type :=
                            Source_Path_To_Sloc.Key (Cur);
         begin
            Tree.Append_Message
              (Message.Create
                 (Message.Error,
                  "source for '" & String (Source_Path) & "' not found",
                  Sloc));
         end;
      end loop;

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

   --------------------------
   -- Update_Sources_Parse --
   --------------------------

   procedure Update_Sources_Parse
     (Def : in out Data; Backends : Source_Info.Backend_Set)
   is
      Def_Sources : Project.Source.Set.Object;
      Def_Src_Map : Simple_Name_Source.Map;
      Position    : Simple_Name_Source.Cursor;
      Inserted    : Boolean;
      SW          : Project.Source.Object;
   begin
      for S of Def.Sources loop
         SW := S;
         SW.Update (Backends);
         Def_Sources.Insert (SW);
         Def_Src_Map.Insert (SW.Path_Name.Simple_Name, SW, Position, Inserted);

         pragma Assert
           (Inserted or else SW.Language /= Ada_Language,
            String (SW.Path_Name.Simple_Name) & " duplicated");

         Set_Source (SW);
      end loop;

      Def.Sources     := Def_Sources;
      Def.Sources_Map := Def_Src_Map;
   end Update_Sources_Parse;

begin
   --  Setup the default/built-in naming package

   Builtin_Naming_Package :=
     Project.Pack.Create
       (SRP.Object (SRP.Create (SR.Builtin, PRP.Naming)),
        Project.Attribute.Set.Empty_Set,
        Project.Variable.Set.Set.Empty_Map);
end GPR2.Project.Definition;
