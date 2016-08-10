------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--            Copyright (C) 2016, Free Software Foundation, Inc.            --
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

with GNAT.MD5;
with GNAT.OS_Lib;

with GPR2.Containers;
with GPR2.Project.Definition;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Source;
with GPR2.Source_Reference;

package body GPR2.Project.View is

   function Naming_Package (Self : Object) return Pack.Object;
   --  Returns the Naming package for the current view. This is either
   --  the view Naming package, the project's tree Naming package from the
   --  loaded configuration project if any and finally the default Naming
   --  package.

   Builtin_Naming_Package : Pack.Object;
   --  The default naming package to use if no Naming package specified in the
   --  project and no configuration file loaded. We at least want to handle in
   --  this case the standard Ada and C namings.

   Builtin_Languages      : Project.Attribute.Object;
   --  The default languages to use if no languages attribute specified in the
   --  project. The default value is just "ada".

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := "") return Project.Attribute.Object is
   begin
      return Definition.Get (Self).Attrs.Element (Name, Index);
   end Attribute;

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := "";
      Index : Value_Type := "") return Project.Attribute.Set.Object is
   begin
      return Definition.Get (Self).Attrs.Filter (Name, Index);
   end Attributes;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is

      function Recursive_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return GPR2.Context.Object;
      --  Recursively get the context for the view. This properly handle
      --  the context given by an aggregate project through the External
      --  attribute.

      -----------------------
      -- Recursive_Context --
      -----------------------

      function Recursive_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return GPR2.Context.Object
      is
         Data : constant Definition.Data := Definition.Get (Self);

         function Get_Context return GPR2.Context.Object;

         -----------------
         -- Get_Context --
         -----------------

         function Get_Context return GPR2.Context.Object is
            use type Definition.Relation_Status;

            Context : GPR2.Context.Object := Data.Context;
         begin
            if Status = Definition.Aggregated then
               for C in Data.A_Context.Iterate loop
                  Context.Include
                    (GPR2.Context.Key_Value.Key (C),
                     GPR2.Context.Key_Value.Element (C));
               end loop;
            end if;

            return Context;
         end Get_Context;

      begin
         if Data.Context_View = Undefined then
            return (if Data.Has_Context
                    then Get_Context
                    else GPR2.Context.Empty);

         else
            return Ctx : GPR2.Context.Object :=
              Recursive_Context (Data.Context_View, Status)
            do
               --  And override by our definition if any
               if Data.Has_Context then
                  for C in Get_Context.Iterate loop
                     Ctx.Include
                       (GPR2.Context.Key_Value.Key (C),
                        GPR2.Context.Key_Value.Element (C));
                  end loop;
               end if;
            end return;
         end if;
      end Recursive_Context;

   begin
      return Recursive_Context (Self, Definition.Get (Self).Status);
   end Context;

   -------------
   -- From_Id --
   -------------

   function From_Id (Id : View.Id) return Object is
   begin
      return Object'(Id => Id);
   end From_Id;

   --------------------
   -- Has_Attributes --
   --------------------

   function Has_Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := "";
      Index : Value_Type := "") return Boolean is
   begin
      if Name = No_Name and then Index = No_Value then
         return not Definition.Get (Self).Attrs.Is_Empty;

      elsif Index = No_Value then
         return Definition.Get (Self).Attrs.Contains (Name);

      else
         return not Attributes (Self, Name, Index).Is_Empty;
      end if;
   end Has_Attributes;

   -----------------
   -- Has_Context --
   -----------------

   function Has_Context (Self : Object) return Boolean is

      function Recursive_Has_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return Boolean;
      --  Recursively check that the view has a context or not. This handles
      --  aggregated project context.

      -----------------------
      -- Recursive_Context --
      -----------------------

      function Recursive_Has_Context
        (Self   : Object;
         Status : Definition.Relation_Status) return Boolean
      is
         Data : constant Definition.Data := Definition.Get (Self);

         function Has_Context return Boolean;

         -----------------
         -- Get_Context --
         -----------------

         function Has_Context return Boolean is
            use type Definition.Relation_Status;
         begin
            return Data.Has_Context
              and then (not Data.Context.Is_Empty
                        or else (Status = Definition.Aggregated
                                 and then not Data.A_Context.Is_Empty));
         end Has_Context;

      begin
         if Data.Context_View = Undefined then
            return Data.Has_Context and then Has_Context;

         else
            return Has_Context
              or else Recursive_Has_Context (Data.Context_View, Status);
         end if;
      end Recursive_Has_Context;

   begin
      return Recursive_Has_Context (Self, Definition.Get (Self).Status);
   end Has_Context;

   -----------------
   -- Has_Imports --
   -----------------

   function Has_Imports (Self : Object) return Boolean is
      use type Ada.Containers.Count_Type;
   begin
      return not Definition.Get (Self).Trees.Imports.Is_Empty;
   end Has_Imports;

   ------------------
   -- Has_Packages --
   ------------------

   function Has_Packages
     (Self : Object;
      Name : Optional_Name_Type := "") return Boolean is
   begin
      if Name = No_Name then
         return not Definition.Get (Self).Packs.Is_Empty;
      else
         return Definition.Get (Self).Packs.Contains (Name_Type (Name));
      end if;
   end Has_Packages;

   -----------------
   -- Has_Sources --
   -----------------

   function Has_Sources (Self : Object) return Boolean is
      use type GPR2.Containers.Count_Type;
   begin
      return not Definition.Get (Self).Sources.Is_Empty;
   end Has_Sources;

   -------------------
   -- Has_Variables --
   -------------------

   function Has_Variables
     (Self : Object;
      Name : Optional_Name_Type := "") return Boolean is
   begin
      if Name = No_Name then
         return not Definition.Get (Self).Vars.Is_Empty;
      else
         return Definition.Get (Self).Vars.Contains (Name_Type (Name));
      end if;
   end Has_Variables;

   ----------
   -- Kind --
   ----------

   function Kind (Self : Object) return Project_Kind is
   begin
      return Definition.Get (Self).Kind;
   end Kind;

   ----------
   -- Name --
   ----------

   function Name (Self : Object) return Name_Type is
   begin
      return Definition.Get (Self).Trees.Project.Name;
   end Name;

   --------------------
   -- Naming_Package --
   --------------------

   function Naming_Package (Self : Object) return Pack.Object is
      Data : constant Definition.Data := Definition.Get (Self);
   begin
      if Self.Has_Packages (Registry.Pack.Naming) then
         return Self.Packages.Element (Name_Type (Registry.Pack.Naming));

      elsif Data.Tree.Has_Configuration_Project
        and then
          Data.Tree.Configuration_Project.Has_Packages
            (Project.Registry.Pack.Naming)
      then
         return Data.Tree.Configuration_Project.Packages.Element
           (Project.Registry.Pack.Naming);

      else
         return Builtin_Naming_Package;
      end if;
   end Naming_Package;

   --------------
   -- Packages --
   --------------

   function Packages (Self : Object) return Pack.Set.Object is
   begin
      return Definition.Get (Self).Packs;
   end Packages;

   ---------------
   -- Path_Name --
   ---------------

   function Path_Name (Self : Object) return Path_Name_Type is
   begin
      return Definition.Get (Self).Trees.Project.Path_Name;
   end Path_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Self : Object) return Project_Kind is
   begin
      return Definition.Get (Self).Trees.Project.Qualifier;
   end Qualifier;

   ---------------
   -- Signature --
   ---------------

   function Signature (Self : Object) return GPR2.Context.Binary_Signature is
   begin
      return Definition.Get (Self).Signature;
   end Signature;

   -------------
   -- Sources --
   -------------

   function Sources (Self : Object) return Source.Set.Object is

      use Ada;
      use GNAT;
      use type MD5.Binary_Message_Digest;

      procedure Handle_Directory (Dir : Full_Path_Name);
      --  Handle the specified directory, that is read all files in Dir and
      --  eventually call recursivelly Handle_Directory if a recursive read
      --  is specified.

      procedure Handle_File (Filename : Full_Path_Name);
      --  Handle Filename which can eventually be part of the current view
      --  depending on the language handled by the current view.

      function Language_For
        (Filename : Full_Path_Name;
         Kind     : out GPR2.Source.Kind_Type)
         return Value_Type;
      --  The language for Filename based on the Naming package. It also
      --  returns in Kind if Filename is a spec, a body or a separate.

      function Unit_For
        (Filename : Full_Path_Name;
         Language : Name_Type;
         Kind     : GPR2.Source.Kind_Type) return Name_Type;
      --  Given Filename, returns the unit name. This is meaningful for unit
      --  based language like Ada. For other languages the unit name is the
      --  same as the Filename.

      function Signature return MD5.Binary_Message_Digest;
      --  Compute the signature corresponding to the source context. If the
      --  signature is not the same recorded for the view, the source set
      --  need to be recomputed.

      Naming : constant Pack.Object := Naming_Package (Self);
      --  Package Naming for the view

      Data : Definition.Data := Definition.Get (Self);
      --  View definition data, will be updated and recorded back into the
      --  definition set.

      ----------------------
      -- Handle_Directory --
      ----------------------

      procedure Handle_Directory (Dir : Full_Path_Name) is
         use all type Directories.File_Kind;

         Is_Recursive : constant Boolean :=
                          Dir'Length > 2
                          and then Dir (Dir'Last) = '*'
                          and then Dir (Dir'Last - 1) = '*';
         --  Recursivityy is controlled by a double * at the end of the
         --  directory.

         Dir_Search   : Directories.Search_Type;
         Dir_Entry    : Directories.Directory_Entry_Type;
         Dir_Name     : constant Full_Path_Name :=
                          (if Is_Recursive
                           then Dir (Dir'First .. Dir'Last - 1)
                           else Dir);
      begin
         Directories.Start_Search (Dir_Search, Dir_Name, "*");

         while Directories.More_Entries (Dir_Search) loop
            Directories.Get_Next_Entry (Dir_Search, Dir_Entry);

            if Directories.Kind (Dir_Entry) = Ordinary_File then
               Handle_File (Directories.Full_Name (Dir_Entry));

            elsif Directories.Kind (Dir_Entry) = Directory
              and then Is_Recursive
            then
               Handle_Sub_Directory : declare
                  New_Dir : constant String :=
                              Directories.Simple_Name (Dir_Entry);
               begin
                  if New_Dir not in "." | ".." then
                     Handle_Directory (Directories.Full_Name (Dir_Entry));
                  end if;
               end Handle_Sub_Directory;
            end if;
         end loop;

         Directories.End_Search (Dir_Search);
      end Handle_Directory;

      -----------------
      -- Handle_File --
      -----------------

      procedure Handle_File (Filename : Full_Path_Name) is
         Kind     : GPR2.Source.Kind_Type;
         Language : constant Value_Type :=
                      Language_For (Filename, Kind);
      begin
         --  Check the language, if no language found this is not a source for
         --  this project.

         if Language /= No_Value then
            declare
               Lang : constant Name_Type := Name_Type (Language);
               Src  : constant GPR2.Source.Object :=
                        GPR2.Source.Create
                          (Filename  => Create_File (Name_Type (Filename)),
                           Kind      => Kind,
                           Language  => Lang,
                           Unit_Name =>
                             (if Lang = "ada"
                              then Value_Type (Unit_For (Filename, Lang, Kind))
                              else No_Value));
            begin
               Data.Sources.Insert (GPR2.Project.Source.Create (Src, Self));
            end;
         end if;
      end Handle_File;

      ------------------
      -- Language_For --
      ------------------

      function Language_For
        (Filename : Full_Path_Name;
         Kind     : out GPR2.Source.Kind_Type)
         return Value_Type
      is

         function Ends_With (Str, Ending : String) return Boolean with Inline;
         --  Returns True if Str ends with the string Ending

         ---------------
         -- Ends_With --
         ---------------

         function Ends_With (Str, Ending : String) return Boolean is
         begin
            if Str'Length >= Ending'Length then
               return Strings.Fixed.Tail (Str, Ending'Length) = Ending;
            else
               return False;
            end if;
         end Ends_With;

         Languages : constant Project.Attribute.Object :=
                       (if Self.Has_Attributes (Registry.Attribute.Languages)
                        then Self.Attribute (Registry.Attribute.Languages)
                        else Builtin_Languages);

         use type Project.Attribute.Object;

      begin
         --  For every languages defined for the view

         for Lang of Languages.Values loop
            Check_Spec : declare
               Spec_Suffix : constant Project.Attribute.Object :=
                               Naming.Spec_Suffix (Name_Type (Lang));
            begin
               if Spec_Suffix /= Project.Attribute.Undefined
                 and then Ends_With (Filename, Spec_Suffix.Value)
               then
                  Kind := GPR2.Source.S_Spec;
                  return Lang;
               end if;
            end Check_Spec;

            Check_Body : declare
               Body_Suffix : constant Project.Attribute.Object :=
                               Naming.Body_Suffix (Name_Type (Lang));
            begin
               if Body_Suffix /= Project.Attribute.Undefined
                 and then Ends_With (Filename, Body_Suffix.Value)
               then
                  Kind := GPR2.Source.S_Body;
                  return Lang;
               end if;
            end Check_Body;

            Check_Separate : declare
               Sep_Suffix : constant Project.Attribute.Object :=
                              Naming.Separate_Suffix (Name_Type (Lang));
            begin
               if Sep_Suffix /= Project.Attribute.Undefined
                 and then Ends_With (Filename, Sep_Suffix.Value)
               then
                  Kind := GPR2.Source.S_Separate;
                  return Lang;
               end if;
            end Check_Separate;
         end loop;

         return No_Value;
      end Language_For;

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

            ---------
            -- Add --
            ---------

            procedure Add (A : Project.Attribute.Object) is
            begin
               MD5.Update (C, String (A.Name) & "/");
               for Value of A.Values loop
                  MD5.Update (C, Value);
               end loop;
            end Add;

         begin
            --  The signature to detect the source change is based on the
            --  attributes which are used to compute the actual source set.

            if Data.Attrs.Has_Source_Dirs then
               Add (Data.Attrs.Source_Dirs);
            end if;

            if Data.Attrs.Has_Source_File then
               Add (Data.Attrs.Source_File);
            end if;
         end Handle;

      begin
         Handle (Data);

         --  If an aggregate library project take into account the
         --  aggregated projects.

         if Data.Kind = K_Aggregate_Library then
            for A of Data.Aggregated loop
               declare
                  A_Data : constant Definition.Data := Definition.Get (A);
               begin
                  Handle (A_Data);
               end;
            end loop;
         end if;

         return MD5.Digest (C);
      end Signature;

      --------------
      -- Unit_For --
      --------------

      function Unit_For
        (Filename : Full_Path_Name;
         Language : Name_Type;
         Kind     : GPR2.Source.Kind_Type) return Name_Type
      is
         use Ada.Strings;

         function Is_Standard_GNAT_Naming return Boolean;
         --  True if the current naming scheme is GNAT's default naming scheme.
         --  This is to take into account shortened names like "Ada." (a-),
         --  "System." (s-) and so on.

         -----------------------------
         -- Is_Standard_GNAT_Naming --
         -----------------------------

         function Is_Standard_GNAT_Naming return Boolean is
            Spec_Suffix : constant Project.Attribute.Object :=
                            Naming.Spec_Suffix ("ada");
            Body_Suffix : constant Project.Attribute.Object :=
                            Naming.Body_Suffix ("ada");
         begin
            return
              (Spec_Suffix = Project.Attribute.Undefined
               or else Spec_Suffix.Value = ".ads")
                 or else
              (Body_Suffix = Project.Attribute.Undefined
               or else Body_Suffix.Value = ".adb")
                 or else
              (not Naming.Has_Attributes
                 (Registry.Attribute.Dot_Replacement, "ada")
               or else Naming.Attribute
                 (Registry.Attribute.Dot_Replacement, "ada").Value = "-");
         end Is_Standard_GNAT_Naming;

         Result : Unbounded_String :=
                    To_Unbounded_String (Directories.Simple_Name (Filename));

      begin
         --  First remove the suffix for the given language

         declare
            Suffix : constant Value_Type :=
                       (case Kind is
                           when GPR2.Source.S_Spec =>
                              Naming.Spec_Suffix (Language).Value,
                           when GPR2.Source.S_Body =>
                              Naming.Body_Suffix (Language).Value,
                           when GPR2.Source.S_Separate =>
                              Naming.Separate_Suffix (Language).Value);
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

         declare
            Dot_Repl : constant String :=
                         (if Naming.Has_Attributes
                            (Registry.Attribute.Dot_Replacement,
                             Value_Type (Language))
                          then Naming.Attribute
                            (Registry.Attribute.Dot_Replacement,
                             Value_Type (Language)).Value
                          else ".");

         begin
            if Dot_Repl /= "." then
               if Index (Result, ".") /= 0 then
                  --  Message.Create
                  --   (Message.Error, "invalid name, contains dot");
                  return Name_Type (To_String (Result));

               else
                  declare
                     I : Natural;
                  begin
                     loop
                        I := Index (Result, Dot_Repl);
                        exit when I = 0;

                        Replace_Slice
                          (Result, I, I + Dot_Repl'Length - 1, ".");
                     end loop;
                  end;
               end if;
            end if;

            Translate (Result, Maps.Constants.Lower_Case_Map);
         end;

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
                  --  platform before processing of the project files.

                  if S2 = '_' and then S3 = '_' then
                     --  Replace first _ by a dot
                     Replace_Element (Result, 2, '.');

                     --  and remove the second _
                     Delete (Result, 3, 3);

                  elsif S2 = '~' then
                     Replace_Element (Result, 2, '.');

                  elsif S2 = '.' then

                     --  If it is potentially a run time source

                     null;
                  end if;
               end if;
            end;
         end if;

         --  Result contains the name of the unit in lower-cases. Check
         --  that this is a valid unit name.

         --  ?? TODO: start with letter, cannot have 2 consecutive underscores,
         --  cannot have aa dot after an underscore, only contains alphanumeric
         --  characters...

         --  If there is a naming exception for the same unit, the file is not
         --  a source for the unit.

         --  ?? TODO: this is not currently supported

         return Name_Type (To_String (Result));
      end Unit_For;

      Current_Signature : constant MD5.Binary_Message_Digest :=
                            Signature;
   begin
      --  Check if up-to-date using signature for source_dirs, source_files...
      --  An abstract or aggregate project has no sources.

      if Data.Sources_Signature /= Current_Signature
        and then Data.Kind not in K_Abstract | K_Aggregate
      then
         --  Read sources and set-up the corresponding definition

         --  First reset the current set

         Data.Sources.Clear;

         if Data.Kind = K_Aggregate_Library then
            --  Sources for an aggregate library is the cumulative set of
            --  sources of the aggregated projects.

            for Agg of Data.Aggregated loop
               Data.Sources.Union (Agg.Sources);
            end loop;

         else
            Populate_Sources : declare
               Root : constant Full_Path_Name :=
                        Directories.Containing_Directory
                          (Value (Data.Trees.Project.Path_Name));
            begin
               --  Handle Source_Dirs

               if Data.Attrs.Has_Source_Dirs then
                  for Dir of Data.Attrs.Source_Dirs.Values loop
                     if OS_Lib.Is_Absolute_Path (Dir) then
                        Handle_Directory (Dir);
                     else
                        Handle_Directory (Directories.Compose (Root, Dir));
                     end if;
                  end loop;
               end if;
            end Populate_Sources;
         end if;

         --  Record back new definition for the view with updated sources

         Data.Sources_Signature := Current_Signature;
         Definition.Set (Self, Data);
      end if;

      --  Then returns the sources

      return Data.Sources;
   end Sources;

   ---------------
   -- Variables --
   ---------------

   function Variables
     (Self : Object;
      Name : Optional_Name_Type := "") return Variable.Set.Object is
   begin
      if Name = No_Name then
         return Definition.Get (Self).Vars;

      else
         return Result : Variable.Set.Object do
            Result.Insert
              (Name_Type (Name),
               Definition.Get (Self).Vars (Name_Type (Name)));
         end return;
      end if;
   end Variables;

begin
   --  Setup the default/build-in naming package

   declare
      Undef_Sloc : Source_Reference.Object renames Source_Reference.Undefined;
      Ada_Spec   : constant Project.Attribute.Object :=
                     Project.Attribute.Create
                       (Registry.Attribute.Spec_Suffix,
                        "ada", ".ads", Undef_Sloc);
      Ada_Body   : constant Project.Attribute.Object :=
                     Project.Attribute.Create
                       (Registry.Attribute.Body_Suffix,
                        "ada", ".adb", Undef_Sloc);
      C_Spec     : constant Project.Attribute.Object :=
                     Project.Attribute.Create
                       (Registry.Attribute.Spec_Suffix,
                        "c", ".h", Undef_Sloc);
      C_Body     : constant Project.Attribute.Object :=
                     Project.Attribute.Create
                       (Registry.Attribute.Body_Suffix,
                        "c", ".c", Undef_Sloc);
      Attrs      : Project.Attribute.Set.Object;
      Langs      : Containers.Value_List;
   begin
      --  Default naming package

      Attrs.Insert (Ada_Spec);
      Attrs.Insert (Ada_Body);
      Attrs.Insert (C_Spec);
      Attrs.Insert (C_Body);
      Builtin_Naming_Package :=
        Pack.Create (Registry.Pack.Naming, Attrs, Undef_Sloc);

      --  Default languages attribute

      Langs.Append ("ada");
      Builtin_Languages :=
        Project.Attribute.Create
          (Registry.Attribute.Languages, Langs, Undef_Sloc);
   end;
end GPR2.Project.View;
