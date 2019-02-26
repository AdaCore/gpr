------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with GPR2.Message;
with GPR2.Project.Definition;
with GPR2.Project.Pack;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source.Set;
with GPR2.Project.Tree;
with GPR2.Project.View.Set;
with GPR2.Source;
with GPR2.Source_Reference;
with GPR2.Unit;

package body GPR2.Project.View is

   use Ada;
   use Ada.Strings.Unbounded;

   use GNAT;

   Executable_Suffix : constant access constant String :=
                         OS_Lib.Get_Executable_Suffix;

   function Get_Ref (View : Object) return Definition.Ref is
      (View.Get.Element);

   function Get_RO (View : Object) return Definition.Const_Ref is
      (View.Get.Element);

   function Get_RW (View : in out Object) return Definition.Ref is
      (View.Get.Element);

   function Refcount (Self : Object) return Natural is
     (Definition_References.Get_Refcount (Self));
   --  Get view refcount

   procedure Set_Def (Ref : out View.Object; Def : Definition_Base'Class);
   --  Convert definition to view

   procedure Update_Sources (Self : Object)
     with Pre => Self.Is_Defined;
   --  Ensure that the view sources are up-to-date. This is needed before
   --  computing the dependencies of a source in the project tree. This routine
   --  is called where needed and is there for internal use only.

   function Apply_Root_And_Subdirs
     (Self : Object; Dir : Value_Type) return GPR2.Path_Name.Object;
   --  Apply project path and subdir option for library, object and executable
   --  directories.

   function Weak (View : Object) return Weak_Reference is
     (Definition_References.Weak (View));

   function Strong (Weak : Weak_Reference) return Object;

   ---------------
   -- Aggregate --
   ---------------

   function Aggregate (Self : Object) return GPR2.Project.View.Object is
   begin
      return Definition.Get_RO (Self).Aggregate;
   end Aggregate;

   ----------------
   -- Aggregated --
   ----------------

   function Aggregated (Self : Object) return GPR2.Project.View.Set.Object is
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
     (Self : Object; Dir : Value_Type) return GPR2.Path_Name.Object
   is
      Result   : GPR2.Path_Name.Object;
      Subdirs  : constant Optional_Name_Type := Self.Tree.Subdirs;
      Dir_Name : constant Name_Type :=
                   (if Dir = "" then "." else Name_Type (Dir));
   begin
      if OS_Lib.Is_Absolute_Path (Dir) then
         Result := GPR2.Path_Name.Create_Directory (Dir_Name);

      elsif Self.Tree.Build_Path.Is_Defined then
         Result := GPR2.Path_Name.Create_Directory
           (Self.Path_Name.Relative_Path
              (Self.Tree.Root_Project.Path_Name).Name,
            Name_Type (Self.Tree.Build_Path.Value));

         Result := GPR2.Path_Name.Create_Directory
           (Dir_Name, Name_Type (Result.Value));

      else
         Result := GPR2.Path_Name.Create_Directory
           (Dir_Name, Name_Type (Self.Path_Name.Dir_Name));
      end if;

      if Subdirs = No_Name then
         return Result;
      end if;

      return GPR2.Path_Name.Create_Directory
               (Subdirs, Name_Type (Result.Value));
   end Apply_Root_And_Subdirs;

   ---------------
   -- Attribute --
   ---------------

   function Attribute
     (Self  : Object;
      Name  : Name_Type;
      Index : Value_Type := No_Value) return Project.Attribute.Object is
   begin
      return Definition.Get_RO (Self).Attrs.Element (Name, Index);
   end Attribute;

   ----------------
   -- Attributes --
   ----------------

   function Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type := No_Value) return Project.Attribute.Set.Object is
   begin
      return Definition.Get_RO (Self).Attrs.Filter (Name, Index);
   end Attributes;

   -------------------
   -- Binder_Prefix --
   -------------------

   function Binder_Prefix
     (Self : Object; Language : Name_Type) return Optional_Name_Type
   is
      package P renames GPR2.Project.Registry.Pack;
      package A renames GPR2.Project.Registry.Attribute;
      Binder : Project.Pack.Object;
   begin
      if Self.Has_Packages (P.Binder) then
         Binder := Self.Pack (P.Binder);

         if Binder.Has_Attributes (A.Prefix, Value_Type (Language)) then
            return Name_Type
              (Binder.Attribute (A.Prefix, Value_Type (Language)).Value.Text);
         end if;
      end if;

      if Self.Tree.Has_Configuration
        and then Self /= Self.Tree.Configuration.Corresponding_View
      then
         return Self.Tree.Configuration.Corresponding_View.Binder_Prefix
           (Language);
      end if;

      return "";
   end Binder_Prefix;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is

      function Recursive_Context (Self : Object) return GPR2.Context.Object;
      --  Recursively get the context for the view. This properly handle
      --  the context given by an aggregate project through the External
      --  attribute.

      -----------------------
      -- Recursive_Context --
      -----------------------

      function Recursive_Context (Self : Object) return GPR2.Context.Object is

         Data : constant Definition.Const_Ref := Definition.Get_RO (Self);

         function Root_Context return GPR2.Context.Object;
         --  Returns the constext of the root project

         ------------------
         -- Root_Context --
         ------------------

         function Root_Context return GPR2.Context.Object is
            R_Data : constant Definition.Const_Ref :=
                       Definition.Get_RO (Data.Tree.Root_Project);
         begin
            return R_Data.Context;
         end Root_Context;

      begin
         if not Data.Context_View.Is_Defined then
            --  Let's return the Root_Project context and possibly the
            --  aggregate context if any.

            return Ctx : GPR2.Context.Object := Root_Context do
               if Data.Trees.Project.Qualifier in Aggregate_Kind then
                  for C in Data.A_Context.Iterate loop
                     Ctx.Include
                       (GPR2.Context.Key_Value.Key (C),
                        GPR2.Context.Key_Value.Element (C));
                  end loop;
               end if;
            end return;

         else
            return Recursive_Context (Data.Context_View);
         end if;
      end Recursive_Context;

   begin
      return Recursive_Context (Self);
   end Context;

   --------------------------
   -- Executable_Directory --
   --------------------------

   function Executable_Directory
     (Self : Object) return GPR2.Path_Name.Object
   is
      package A renames GPR2.Project.Registry.Attribute;
   begin
      return Self.Apply_Root_And_Subdirs
        (if Self.Has_Attributes (A.Exec_Dir)
         then Self.Attribute (A.Exec_Dir).Value.Text
         elsif  Self.Has_Attributes (A.Object_Dir)
         then Self.Attribute (A.Object_Dir).Value.Text
         else ".");
   end Executable_Directory;

   --------------
   -- Extended --
   --------------

   function Extended (Self : Object) return Object is
   begin
      return Definition.Get_RO (Self).Extended;
   end Extended;

   --------------------
   -- Has_Attributes --
   --------------------

   function Has_Attributes
     (Self  : Object;
      Name  : Optional_Name_Type := No_Name;
      Index : Value_Type := No_Value) return Boolean is
   begin
      if Name = No_Name and then Index = No_Value then
         return not Definition.Get_RO (Self).Attrs.Is_Empty;

      elsif Index = No_Value then
         return Definition.Get_RO (Self).Attrs.Contains (Name);

      else
         return not Attributes (Self, Name, Index).Is_Empty;
      end if;
   end Has_Attributes;

   -----------------
   -- Has_Context --
   -----------------

   function Has_Context (Self : Object) return Boolean is

      function Recursive_Has_Context (Self : Object) return Boolean;
      --  Recursively get the context for the view. This properly handle
      --  the context given by an aggregate project through the External
      --  attribute.

      -----------------------
      -- Recursive_Context --
      -----------------------

      function Recursive_Has_Context (Self : Object) return Boolean is

         Data : constant Definition.Const_Ref := Definition.Get_RO (Self);

         function Root_Has_Context return Boolean;
         --  Returns wether the root project has a context

         ------------------
         -- Root_Context --
         ------------------

         function Root_Has_Context return Boolean is
            R_Data : constant Definition.Const_Ref :=
                       Definition.Get_RO (Data.Tree.Root_Project);
         begin
            return not R_Data.Context.Is_Empty;
         end Root_Has_Context;

      begin
         if not Data.Context_View.Is_Defined then
            --  Let's return the Root_Project context and possibly the
            --  aggregate context if any.

            return Result : Boolean := Root_Has_Context do
               if Data.Trees.Project.Qualifier in Aggregate_Kind then
                  Result := Result or else not Data.A_Context.Is_Empty;
               end if;
            end return;

         else
            return Recursive_Has_Context (Data.Context_View);
         end if;
      end Recursive_Has_Context;

   begin
      return Recursive_Has_Context (Self);
   end Has_Context;

   ------------------
   -- Has_Extended --
   ------------------

   function Has_Extended (Self : Object) return Boolean is
   begin
      return Definition.Get_RO (Self).Extended.Is_Defined;
   end Has_Extended;

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
      return not Definition.Get_RO (Self).Languages.Is_Empty;
   end Has_Languages;

   ---------------
   -- Has_Mains --
   ---------------

   function Has_Mains (Self : Object) return Boolean is
   begin
      return Self.Has_Attributes (Project.Registry.Attribute.Main)
        and then
          Self.Attribute (Project.Registry.Attribute.Main).Values.Length > 0;
   end Has_Mains;

   ------------------
   -- Has_Packages --
   ------------------

   function Has_Packages
     (Self : Object;
      Name : Optional_Name_Type := No_Name) return Boolean is
   begin
      return Definition.Get_RO (Self).Has_Packages (Name);
   end Has_Packages;

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

   -------------
   -- Imports --
   -------------

   function Imports
     (Self      : Object;
      Recursive : Boolean := False) return GPR2.Project.View.Set.Object
   is
      Result : GPR2.Project.View.Set.Object;

      procedure Add (Self : Object);
      --  Add Self imported projects

      ---------
      -- Add --
      ---------

      procedure Add (Self : Object) is
      begin
         for Import of Definition.Get_RO (Self).Imports loop
            if not Result.Contains (Import) then
               Result.Insert (Import);

               if Recursive then
                  Add (Import);
               end if;
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

   -------------------
   -- Is_Aggregated --
   -------------------

   function Is_Aggregated (Self : Object) return Boolean is
   begin
      return Definition.Get_RO (Self).Aggregate.Is_Defined;
   end Is_Aggregated;

   ------------------------------
   -- Is_Aggregated_In_Library --
   ------------------------------

   function Is_Aggregated_In_Library (Self : Object) return Boolean is
      Ref : constant Definition.Const_Ref := Definition.Get_RO (Self);
   begin
      return Ref.Aggregate.Is_Defined
        and then Ref.Aggregate.Kind = K_Aggregate_Library;
   end Is_Aggregated_In_Library;

   ---------------------
   -- Is_Extended_All --
   ---------------------

   function Is_Extended_All (Self : Object) return Boolean is
   begin
      return Definition.Get_RO (Self).Trees.Project.Is_Extended_All;
   end Is_Extended_All;

   -------------------------
   -- Is_Externally_Built --
   -------------------------

   function Is_Externally_Built (Self : Object) return Boolean is
   begin
      return Self.Has_Attributes (Project.Registry.Attribute.Externally_Built)
        and then
          Self.Attribute
            (Project.Registry.Attribute.Externally_Built).Value_Equal ("true");
   end Is_Externally_Built;

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
   begin
      return Definition.Get_RO (Self).Languages;
   end Languages;

   -----------------------
   -- Library_Directory --
   -----------------------

   function Library_Directory (Self : Object) return GPR2.Path_Name.Object is
   begin
      return Self.Apply_Root_And_Subdirs
        (Self.Attribute (Project.Registry.Attribute.Library_Dir).Value.Text);
   end Library_Directory;

   ----------------------
   -- Library_Filename --
   ----------------------

   function Library_Filename (Self : Object) return GPR2.Path_Name.Object is

      package A renames GPR2.Project.Registry.Attribute;

      function Config_Has_Attribute (Name : Name_Type) return Boolean is
        (Self.Tree.Has_Configuration
         and then
         Self.Tree.Configuration.Corresponding_View.Has_Attributes (Name));

      function Config return GPR2.Project.View.Object is
        (Self.Tree.Configuration.Corresponding_View);

      File_Name : Unbounded_String;

   begin
      --  Library prefix

      if not Self.Is_Static_Library
        and then Config_Has_Attribute (A.Shared_Lib_Prefix)
      then
         Append
           (File_Name,
            Config.Attribute (A.Shared_Lib_Prefix).Value.Text);
      else
         Append (File_Name, "lib");
      end if;

      --  Library name

      Append (File_Name, Self.Attribute (A.Library_Name).Value.Text);

      --  Library suffix

      if Self.Is_Static_Library then
         Append (File_Name, String (Self.Tree.Archive_Suffix));

      elsif Config_Has_Attribute (A.Shared_Lib_Suffix) then
         Append
           (File_Name,
            Config.Attribute (A.Shared_Lib_Suffix).Value.Text);

      else
         Append (File_Name, ".so");
      end if;

      return GPR2.Path_Name.Create_File
        (Name_Type (To_String (File_Name)),
         Directory =>  Optional_Name_Type (Self.Library_Directory.Dir_Name));
   end Library_Filename;

   ------------------
   -- Library_Kind --
   ------------------

   function Library_Kind (Self : Object) return Name_Type is
      package A renames GPR2.Project.Registry.Attribute;
   begin
      if Self.Has_Attributes (A.Library_Kind) then
         return Name_Type (Self.Attribute (A.Library_Kind).Value.Text);
      else
         return "static";
      end if;
   end Library_Kind;

   --------------------------------
   -- Library_Major_Version_Name --
   --------------------------------

   function Library_Major_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object
   is

      function Major_Version_Name (Lib_Version : Name_Type) return Name_Type;
      --  Returns the major version name

      ------------------------
      -- Major_Version_Name --
      ------------------------

      function Major_Version_Name (Lib_Version : Name_Type) return Name_Type is
      begin
         for J in reverse Lib_Version'Range loop
            if Lib_Version (J) = '.' then
               return Lib_Version (Lib_Version'First .. J - 1);
            end if;
         end loop;

         --  inpossible if project view was validated just after parse

         raise Program_Error;
      end Major_Version_Name;

      package A renames GPR2.Project.Registry.Attribute;

   begin
      if Self.Has_Attributes (A.Library_Version)
        and then not Self.Is_Static_Library
      then
         return GPR2.Path_Name.Create_File
           (Major_Version_Name
              (Name_Type (Self.Attribute (A.Library_Version).Value.Text)),
            Directory => Optional_Name_Type (Self.Library_Filename.Dir_Name));

      else
         return GPR2.Path_Name.Undefined;
      end if;
   end Library_Major_Version_Filename;

   ------------------------
   -- Library_Standalone --
   ------------------------

   function Library_Standalone
     (Self : Object) return Standalone_Library_Kind
   is
      Has_Interface : constant Boolean :=
                        Self.Has_Attributes
                          (Project.Registry.Attribute.Interfaces)
                            or else
                        Self.Has_Attributes
                          (Project.Registry.Attribute.Library_Interface);
   begin
      if Has_Interface then
         if Self.Has_Attributes
           (Project.Registry.Attribute.Library_Standalone)
         then
            return Standalone_Library_Kind'Value
              (Self.Attribute
                 (Project.Registry.Attribute.Library_Standalone).Value.Text);

         else
            --  By default, if there is no attribute Library_Standalone we are
            --  building a standard standalone library.
            return Standard;
         end if;

      else
         --  No interface, that is not a standalone library
         return No;
      end if;
   end Library_Standalone;

   ------------------------------
   -- Library_Version_Filename --
   ------------------------------

   function Library_Version_Filename
     (Self : Object) return GPR2.Path_Name.Object is
   begin
      return GPR2.Path_Name.Create_File
        (Name_Type
           (Self.Attribute
              (Project.Registry.Attribute.Library_Version).Value.Text),
         Directory => Optional_Name_Type (Self.Library_Directory.Dir_Name));
   end Library_Version_Filename;

   -----------
   -- Mains --
   -----------

   function Mains (Self : Object) return GPR2.Path_Name.Set.Object is
      use GPR2.Project.Pack;

      package A renames GPR2.Project.Registry.Attribute;
      package P renames GPR2.Project.Registry.Pack;

      Tree    : constant not null access Project.Tree.Object := Self.Tree;

      Builder : constant GPR2.Project.Pack.Object :=
                  (if Self.Has_Packages (P.Builder)
                   then Self.Packages.Element (P.Builder)
                   else Project.Pack.Undefined);

      Executable_Suffix : constant String :=
                            (if Tree.Has_Configuration
                                 and then
                               Tree.Configuration.Corresponding_View.
                                 Has_Attributes (A.Executable_Suffix)
                             then
                                Tree.Configuration.Corresponding_View.
                                  Attribute (A.Executable_Suffix).Value.Text

                             elsif Builder.Is_Defined
                                 and then
                               Builder.Has_Attributes (A.Executable_Suffix)
                             then
                               Builder.Attribute
                                 (A.Executable_Suffix).Value.Text

                             else
                                View.Executable_Suffix.all);

      function Create (Source : Value_Not_Empty) return GPR2.Path_Name.Object;
      --  Returns the full pathname of the main executable for the givem main

      ------------
      -- Create --
      ------------

      function Create
        (Source : Value_Not_Empty) return GPR2.Path_Name.Object
      is
         Attr : GPR2.Project.Attribute.Object;
      begin
         if Builder.Is_Defined then
            if Builder.Has_Attributes (A.Executable, Source) then
               Attr := Builder.Attribute (A.Executable, Source);

            else
               --  Not found but an extension is present, check without

               declare
                  BN : constant Value_Type := Directories.Base_Name (Source);
               begin
                  if Source /= BN
                    and then Builder.Has_Attributes (A.Executable, BN)
                  then
                     Attr := Builder.Attribute (A.Executable, BN);
                  end if;
               end;
            end if;
         end if;

         return GPR2.Path_Name.Create_File
           (Name_Type
              ((if Attr.Is_Defined
                then Attr.Value.Text
                else Ada.Directories.Base_Name (String (Source)))
               & Executable_Suffix),
            Optional_Name_Type (Self.Executable_Directory.Dir_Name));
      end Create;

   begin
      return Set : GPR2.Path_Name.Set.Object do
         for Main of Self.Attribute (A.Main).Values loop
            Set.Append (Mains.Create (Main.Text));
         end loop;
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
   -- Naming_Package --
   --------------------

   function Naming_Package (Self : Object) return Project.Pack.Object is
   begin
      return Definition.Get_RO (Self).Naming_Package;
   end Naming_Package;

   ----------------------
   -- Object_Directory --
   ----------------------

   function Object_Directory (Self : Object) return GPR2.Path_Name.Object is

      package A renames GPR2.Project.Registry.Attribute;

   begin
      return Self.Apply_Root_And_Subdirs
        (if Self.Has_Attributes (A.Object_Dir)
         then Self.Attribute (A.Object_Dir).Value.Text
         else ".");
   end Object_Directory;

   ----------
   -- Pack --
   ----------

   function Pack
     (Self : Object;
      Name : Name_Type) return Project.Pack.Object is
   begin
      return Definition.Get_RO (Self).Packs (Name);
   end Pack;

   --------------
   -- Packages --
   --------------

   function Packages (Self : Object) return Project.Pack.Set.Object is
   begin
      return Definition.Get_RO (Self).Packs;
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

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Object) is
      Data : constant Definition.Const_Ref := Definition.Get_RO (Self);
   begin
      for C in Data.Sources.Iterate loop
         declare
            S : Project.Source.Object := Project.Source.Set.Element (C);
         begin
            S.Release;
         end;
      end loop;

      Self := Undefined;
   end Release;

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
     (Self : Object;
      File : GPR2.Path_Name.Object) return Project.Source.Object is
   begin
      Self.Update_Sources;

      for S of Definition.Get_RO (Self).Sources loop
         if S.Source.Path_Name.Value = File.Value then
            return S;
         end if;
      end loop;

      return Project.Source.Undefined;
   end Source;

   ------------------------
   -- Source_Directories --
   ------------------------

   function Source_Directories
     (Self : Object) return Project.Attribute.Object is
   begin
      return Definition.Get_RO (Self).Source_Directories;
   end Source_Directories;

   -------------
   -- Sources --
   -------------

   function Sources
     (Self   : Object;
      Filter : Source_Kind := K_All) return Project.Source.Set.Object is
   begin
      Self.Update_Sources;

      if Filter = K_All then
         return Definition.Get_RO (Self).Sources;

      else
         return S_Set : Project.Source.Set.Object do
            declare
               Data : constant Project.Definition.Const_Ref :=
                        Definition.Get_RO (Self);
            begin
               for S of Data.Sources loop
                  declare
                     Unit_Is_Interface : constant Boolean :=
                                           Data.Units
                                             (S.Source.Unit_Name).Is_Interface;
                  begin
                     if (Filter = K_Interface_Only and then Unit_Is_Interface)
                       or else
                        (Filter = K_Not_Interface
                         and then not Unit_Is_Interface)
                     then
                        S_Set.Insert (S);
                     end if;
                  end;
               end loop;
            end;
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

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources (Self : Object) is
   begin
      Get_Ref (Self).Update_Sources (Self);
   end Update_Sources;

   --------------
   -- Variable --
   --------------

   function Variable
     (Self : Object; Name : Name_Type) return Project.Variable.Object is
   begin
      return Definition.Get_RO (Self).Vars (Name);
   end Variable;

   ---------------
   -- Variables --
   ---------------

   function Variables (Self : Object) return Project.Variable.Set.Object is
   begin
      return Definition.Get_RO (Self).Vars;
   end Variables;

   --------------
   -- View_For --
   --------------

   function View_For (Self : Object; Name : Name_Type) return View.Object is
      Data : constant Definition.Const_Ref := Definition.Get_RO (Self);
   begin
      --  Returns the project view corresponding to Name and found in the
      --  context of View (e.g. imported or extended).

      if Data.Extended.Is_Defined
        and then Definition.Get_RO (Data.Extended).Trees.Project.Name = Name
      then
         return Data.Extended;

      elsif Data.Imports.Contains (Name) then
         return Data.Imports.Element (Name);
      end if;

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
