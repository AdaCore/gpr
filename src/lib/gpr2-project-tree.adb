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

with Ada.Containers.Indefinite_Holders;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with GPR2.Parser.Project.Create;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Definition;
with GPR2.Project.Import.Set;
with GPR2.Project.Pack;
with GPR2.Project.Registry.Pack;
with GPR2.Source_Reference.Attribute;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids.Set;
with GPR2.View_Ids.Vector;
with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.String_Split;

with GNATCOLL.OS.Constants;

package body GPR2.Project.Tree is

   use GNAT;
   use type GPR2.Path_Name.Object;
   use type GNATCOLL.OS.OS_Type;

   package PC renames Project.Configuration;
   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;
   package IDS renames GPR2.View_Ids;

   Is_Windows_Host : constant Boolean :=
                       GNATCOLL.OS.Constants.OS = GNATCOLL.OS.Windows
                         with Warnings => Off;

   Wildcards       : constant Ada.Strings.Maps.Character_Set :=
                       Ada.Strings.Maps.To_Set ("?*");
   --  Wild chars for filename pattern

   procedure Error
      (Self : in out Object;
       Msg  : String;
       Sloc : Source_Reference.Object'Class);
   --  Append an error to Self.Messages

   procedure Warning
      (Self : in out Object;
       Msg  : String;
       Sloc : Source_Reference.Object'Class);
   --  Append a warning to Self.Messages

   function Register_View
     (Def : in out Definition.Data) return Project.View.Object
     with Post => Register_View'Result.Is_Defined;
   --  Register view definition in the Tree and return the View object

   procedure Set_Context
     (Self    : in out Object;
      Changed : access procedure (Project : View.Object) := null);
   --  Update project tree with updated context

   function Check_Source
     (View   : Project.View.Object;
      Name   : Simple_Name;
      Result : in out Source.Object) return Boolean;
   --  Get the source by simple filename from the same subtree with the View.
   --  Return True on success and set Result.
   --  Return False if source not found and remain Result untouched.

   function Check_Source
     (View   : Project.View.Object;
      Unit   : GPR2.Unit.Object;
      Result : in out Source.Object) return Boolean;
   --  Get source by unit name and kind from the same subtree with the View

   procedure Enable_Ali_Parser (Tree : in out Object; Enable : Boolean);

   function Ali_Parser_Is_Enabled (Tree : Object) return Boolean
   is (Tree.Ali_Parser_Is_On);

   function Has_Source
     (View : Project.View.Object; Name : Simple_Name) return Boolean
   is
     (View.Tree.Rooted_Sources.Contains (Key (View, Name)));
   --  Return True if source with such filename found in project namespace
   --  subtree.

   function Get_Context
     (View : Project.View.Object) return GPR2.Context.Object
   is
     (View.Tree.Context (View.Context));
   --  Returns context of the project view

   function Are_Sources_Loaded (Tree : Object) return Boolean is
     (Tree.Sources_Loaded);

   procedure Set_Source (Source : Project.Source.Object);
   --  Insert source into internal Tree container indexed by Root of subtree
   --  project name and simple source filename.

   procedure Remove_Source (Source : Project.Source.Object);
   --  Remove Source from internal Tree container indexed by Root of subtree
   --  project name and simple source filename.

   type Iterator is new Project_Iterator.Forward_Iterator with record
      Kind   : Iterator_Control;
      Filter : Filter_Control;
      Status : Status_Control;
      Root   : not null access constant Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Recursive_Load
     (Self          : in out Object;
      Filename      : Path_Name.Object;
      Context       : Context_Kind;
      Starting_From : View.Object := View.Undefined) return View.Object
     with Pre =>
       (if Starting_From.Is_Defined
        then Starting_From.Qualifier in Aggregate_Kind);
   --  Load a project Filename recursively and returns the corresponding root
   --  view.
   --  Context indicates if project is loaded using the root context or the
   --  root aggregate context.
   --  Starting_From if set is the aggregate or aggregate library starting
   --  point for the parsing.

   function Create_Runtime_View (Self : Object) return View.Object
     with Pre => Self.Is_Defined
                 and then Self.Has_Configuration;
   --  Create the runtime view given the configuration project

   function Get
     (Tree    : Project.Tree.Object;
      Name    : Name_Type;
      Context : Context_Kind) return Project.View.Object;
   --  Returns the project view corresponding to Name.
   --  If Aggregated is True then view should be taken from aggregated subtree.
   --  Returns Undefined if project view is not found.

   function Get_View
      (Tree : Project.Tree.Object;
       Id   : IDS.View_Id)
       return Project.View.Object;
   --  Given a View_Id Id returns the associated view if it exists. Returns
   --  Project.View.Undefined otherwise.

   procedure Update_Context
     (Context   : in out GPR2.Context.Object;
      Externals : Containers.Name_List);
   --  For all externals in Externals, if external is not already present in
   --  the context, fetch its value from the environment and insert it into the
   --  context.

   procedure Update_Project_Search_Path_From_Config
     (Self : in out Object;
      Conf : Project.Configuration.Object)
     with Pre => Conf.Is_Defined;
   --  Update project search path with directories relevant to

   procedure Get_File
     (Self            : Object;
      Base_Name       : Simple_Name;
      Ambiguous       : out Boolean;
      Full_Path       : out Path_Name.Object;
      View            : Project.View.Object := Project.View.Undefined;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True;
      Predefined_Only : Boolean := False);
   --  Return in Full_Path absolute path of source/object/project file found
   --  in Self or in View when defined.
   --
   --  If no file found, Undefined is returned in Full_Path and Ambiguous is
   --  set to False.
   --
   --  If file is part of the sources for several projects, Ambiguous is set
   --  to True and all of them have same absolute path Full_Path is set to
   --  the common source file, otherwise Undefined is returned in Full_Path.
   --
   --  see also function Get_File

   ---------------------
   -- Add_Tool_Prefix --
   ---------------------

   function Add_Tool_Prefix
     (Self      : Object;
      Tool_Name : Name_Type) return Name_Type
   is

      Not_Defined : constant Optional_Name_Type := " ";
      --  value returned when GNAT_Compilers_Prefix cannot find Compiler
      --  Driver in View

      function GNAT_Compilers_Prefix
        (View : Project.View.Object) return Optional_Name_Type
        with Pre => View.Is_Defined;
      --  Returns prefix found in Compiler package's Driver attribute for gcc
      --  or g++ tools. Returns Not_Defined if no attribute was defined.

      ---------------------------
      -- GNAT_Compilers_Prefix --
      ---------------------------

      function GNAT_Compilers_Prefix
        (View : Project.View.Object) return Optional_Name_Type
      is
         package PRP renames Project.Registry.Pack;
         package PRA renames Project.Registry.Attribute;
      begin
         if View.Is_Defined and then View.Has_Packages (PRP.Compiler) then
            for Language of View.Languages loop
               declare
                  Index  : constant Attribute_Index.Object :=
                             Attribute_Index.Create (Language.Text);
                  Driver : constant String :=
                             (if View.Pack (PRP.Compiler).Has_Attributes
                                (PRA.Driver, Index)
                              then
                                 String (Path_Name.Create_File
                                           (Filename_Type (View.Pack
                                             (PRP.Compiler).Attribute
                                               (PRA.Driver,
                                                Index).Value.Text)).Base_Name)
                              else "");
               begin
                  if Driver'Length > 2 then
                     declare
                        subtype Last_3_Chars_Range is Positive range
                          Driver'Last - 2 .. Driver'Last;
                        Last_3_Chars : constant String :=
                                         (if Is_Windows_Host
                                          then Ada.Characters.Handling.To_Lower
                                                 (Driver (Last_3_Chars_Range))
                                          else Driver (Last_3_Chars_Range));
                     begin
                        if Last_3_Chars = "gcc"
                          or else Last_3_Chars = "g++"
                        then
                           if Driver'Length = 3 then
                              return No_Name;
                           else
                              return Optional_Name_Type
                                (Driver (Driver'First .. Driver'Last - 3));
                           end if;
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end if;
         return Not_Defined;
      end GNAT_Compilers_Prefix;

      Prefix_From_Root : constant Optional_Name_Type :=
                           GNAT_Compilers_Prefix (Self.Root);
      --  Try to find prefix from root project compiler package

   begin
      if Prefix_From_Root = Not_Defined then
         if Self.Has_Configuration then
            declare
               Prefix_From_Config : constant Optional_Name_Type
                 := GNAT_Compilers_Prefix
                   (Self.Configuration.Corresponding_View);
               --  Try to find prefix from config project compiler package
            begin
               if Prefix_From_Config = Not_Defined then
                  --  Use Target as prefix if prefix not defined in project &
                  --  configuration file.

                  return Name_Type
                    (String (Self.Target) & "-" & String (Tool_Name));
               else
                  return Name_Type
                    (String (Prefix_From_Config) & String (Tool_Name));
               end if;
            end;

         else
            --  Use Target as prefix if not defined in project and no
            --  configution file was loaded.

            return Name_Type (String (Self.Target) & "-" & String (Tool_Name));
         end if;

      else
         return Name_Type (String (Prefix_From_Root) & String (Tool_Name));
      end if;
   end Add_Tool_Prefix;

   --------------------
   -- Append_Message --
   --------------------

   procedure Append_Message
     (Self    : in out Object;
      Message : GPR2.Message.Object) is
   begin
      Self.Messages.Append (Message);
   end Append_Message;

   ------------------
   -- Check_Source --
   ------------------

   function Check_Source
     (View   : Project.View.Object;
      Name   : Simple_Name;
      Result : in out Source.Object) return Boolean
   is
      Position : constant Source_Maps.Cursor :=
                   View.Tree.Rooted_Sources.Find (Key (View, Name));
   begin
      if Source_Maps.Has_Element (Position) then
         Result := Source_Maps.Element (Position);
         return True;

      else
         --  Search for runtime sources separately because runtime view does
         --  not belong any naming roots.

         return not View.Is_Runtime
           and then View.Tree.Runtime.Is_Defined
           and then View.Tree.Runtime.Check_Source (Name, Result);
      end if;
   end Check_Source;

   ------------------
   -- Check_Source --
   ------------------

   function Check_Source
     (View   : Project.View.Object;
      Unit   : GPR2.Unit.Object;
      Result : in out Source.Object) return Boolean
   is
      Position : constant Source_Maps.Cursor :=
                   View.Tree.Rooted_Sources.Find (Key (View, Unit));
   begin
      if Source_Maps.Has_Element (Position) then
         Result := Source_Maps.Element (Position);
         return True;
      else
         return False;
      end if;
   end Check_Source;

   ----------------
   -- Clear_View --
   ----------------

   procedure Clear_View
     (Self : in out Object;
      Unit : Unit_Info.Object) is
   begin
      --  Clear the corresponding sources

      if Unit.Spec.Is_Defined then
         Self.Sources.Exclude
           (Filename_Type (Unit.Spec.Value));
      end if;

      if Unit.Main_Body.Is_Defined then
         Self.Sources.Exclude
           (Filename_Type (Unit.Main_Body.Value));
      end if;

      for S of Unit.Separates loop
         Self.Sources.Exclude (Filename_Type (S.Value));
      end loop;
   end Clear_View;

   -------------------
   -- Configuration --
   -------------------

   function Configuration (Self : Object) return PC.Object is
   begin
      return Self.Conf;
   end Configuration;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
   is
      Ref : Project_View_Store.Constant_Reference_Type renames
              Project_View_Store.Constant_Reference
                (Position.Views, Position.Current);
   begin
      --  Constant reference is given by the constant reference of the
      --  element contained in the Views set at the current location.
      return Constant_Reference_Type'
        (View => Ref.Element.all'Unrestricted_Access,
         Ref  => Ref);
   end Constant_Reference;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is
   begin
      return Self.Root.Context;
   end Context;

   -------------------------
   -- Create_Runtime_View --
   -------------------------

   function Create_Runtime_View (Self : Object) return View.Object is
      CV   : constant View.Object := Self.Conf.Corresponding_View;
      DS   : Character renames OS_Lib.Directory_Separator;
      Data : Project.Definition.Data;
      RTD  : Attribute.Object;
      RTF  : Path_Name.Object;

      procedure Add_Attribute (Name : Attribute_Id; Value : Value_Type);
      --  Add builtin attribute into Data.Attrs

      -------------------
      -- Add_Attribute --
      -------------------

      procedure Add_Attribute (Name : Attribute_Id; Value : Value_Type) is
      begin
         Data.Attrs.Insert
           (Project.Attribute.Create
              (Name  => Source_Reference.Attribute.Object
                          (Source_Reference.Attribute.Create
                             (Source_Reference.Builtin, Name)),
               Value => Source_Reference.Value.Object
                          (Source_Reference.Value.Create
                             (Source_Reference.Object
                                (Source_Reference.Create (RTF.Value, 0, 0)),
                              Value))));
      end Add_Attribute;

   begin
      --  Check runtime path

      if CV.Check_Attribute
        (PRA.Runtime_Dir,
         Attribute_Index.Create ("ada"), Result => RTD)
        and then RTD.Value.Text /= ""
      then
         --  Runtime_Dir (Ada) exists, this is used to compute the Source_Dirs
         --  and Object_Dir for the Runtime project view.

         RTF := Path_Name.Create_File
           ("runtime.gpr", Directory => Filename_Optional (RTD.Value.Text));

         declare
            Dirs : Containers.Source_Value_List;

            procedure Add_If_Exists (Dir_Name : String);
            --  Add directory name into Dirs if it exists

            -------------------
            -- Add_If_Exists --
            -------------------

            procedure Add_If_Exists (Dir_Name : String) is
            begin
               if Directories.Exists (Dir_Name) then
                  Dirs.Append
                    (Source_Reference.Value.Object
                       (Source_Reference.Value.Create
                          (Source_Reference.Object
                             (Source_Reference.Create (RTF.Value, 0, 0)),
                              Dir_Name)));
               end if;
            end Add_If_Exists;

            function With_RTD_Prefix (Name : String) return String is
              (Directories.Compose (RTD.Value.Text, Name));
            --  Prepend the Name with

            Ada_Source_Path : constant String :=
                                With_RTD_Prefix ("ada_source_path");

            use Ada.Text_IO;

            File : File_Type;

         begin
            if Directories.Exists (Ada_Source_Path) then
               Open (File, Text_IO.In_File, Ada_Source_Path);

               while not End_Of_File (File) loop
                  declare
                     Line : constant String := Get_Line (File);
                  begin
                     if Line /= "" then
                        Add_If_Exists
                          (if OS_Lib.Is_Absolute_Path (Line) then Line
                           else With_RTD_Prefix (Line));
                     end if;
                  end;
               end loop;

               Close (File);

            else
               Add_If_Exists (With_RTD_Prefix ("adainclude"));
            end if;

            Data.Attrs.Insert
              (Project.Attribute.Create
                 (Name => Source_Reference.Attribute.Object
                            (Source_Reference.Attribute.Create
                               (Source_Reference.Builtin, PRA.Source_Dirs)),
                  Values => Dirs));
         end;

         Add_Attribute (PRA.Object_Dir,  RTD.Value.Text & DS & "adalib");

         --  The only language supported is Ada

         Add_Attribute (PRA.Languages, "ada");

         Data.Tree      := Self.Self;
         Data.Kind      := K_Standard;
         Data.Path      := Path_Name.Create_Directory
                             (Filename_Type (RTD.Value.Text));
         Data.Is_Root   := True;
         Data.Unique_Id := GPR2.View_Ids.Runtime_View_Id;

         Data.Trees.Project := Parser.Project.Create
           (Name      => Name (PRA.Runtime),
            File      => RTF,
            Qualifier => K_Standard);

         return Result : View.Object := Register_View (Data) do
            --  If we simply return Register_View (Data) the reference counter
            --  will be one more than should be, see T709-001.
            --  It is not actual for now because line below is added after the
            --  T709-001 bug detected.

            Definition.Get_RW (Result).Root_View := Definition.Weak (Result);
         end return;

      else
         return Project.View.Undefined;
      end if;
   end Create_Runtime_View;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return View.Object is
   begin
      return Position.Views (Position.Current);
   end Element;

   -----------------------
   -- Enable_Ali_Parser --
   -----------------------

   procedure Enable_Ali_Parser (Tree : in out Object; Enable : Boolean) is
   begin
      Tree.Ali_Parser_Is_On := Enable;
   end Enable_Ali_Parser;

   -----------
   -- Error --
   -----------

   procedure Error
     (Self : in out Object;
      Msg  : String;
      Sloc : Source_Reference.Object'Class) is
   begin
      Self.Messages.Append (Message.Create (Message.Error, Msg, Sloc));
   end Error;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is

      Seen : GPR2.Project.View.Set.Object;
      --  Keep track of already seen projects. Better than using the P vector
      --  which is not efficient when checking if an element exists.

      P_Set    : GPR2.Project.View.Set.Object;
      Projects : Project_View_Store.Vector;
      --  Set of projects for the iterator which is returned in the Cursor and
      --  fill by the recursive procedure For_Project and For_Imports. P_Set is
      --  used to have a fast check on views already in Projects.

      procedure Append (View : Project.View.Object)
        with Post => P_Set.Contains (View);
      --  Append into P if not already seen and View matches the filter

      procedure For_Project (View : Project.View.Object);
      --  Handle project node

      procedure For_Imports (View : Project.View.Object);
      --  Handle import nodes

      procedure For_Aggregated (View : Project.View.Object);
      --  Handle aggregated nodes

      ------------
      -- Append --
      ------------

      procedure Append (View : Project.View.Object) is
      begin
         if not P_Set.Contains (View) then
            if Equal
              (Iter.Status (S_Externally_Built),
               View.Is_Externally_Built) in True | Indeterminate
            then
               declare
                  Qualifier : constant Project_Kind := View.Kind;
               begin
                  --  Check if it corresponds to the current filter
                  if (Qualifier = K_Library and then Iter.Filter (F_Library))
                    or else
                     (Qualifier = K_Standard and then Iter.Filter (F_Standard))
                    or else
                     (Qualifier = K_Abstract and then Iter.Filter (F_Abstract))
                    or else
                     (Qualifier = K_Aggregate
                      and then Iter.Filter (F_Aggregate))
                    or else
                     (Qualifier = K_Aggregate_Library
                      and then Iter.Filter (F_Aggregate_Library))
                  then
                     Projects.Append (View);
                  end if;
               end;
            end if;

            P_Set.Insert (View);
         end if;
      end Append;

      --------------------
      -- For_Aggregated --
      --------------------

      procedure For_Aggregated (View : Project.View.Object) is
      begin
         if View.Kind in Aggregate_Kind then
            for A of Definition.Get_RO (View).Aggregated loop
               if Iter.Kind (I_Recursive) then
                  For_Project (A);
               else
                  Append (A);
               end if;
            end loop;
         end if;
      end For_Aggregated;

      -----------------
      -- For_Imports --
      -----------------

      procedure For_Imports (View : Project.View.Object) is
      begin
         for I of Definition.Get_RO (View).Imports loop
            if Iter.Kind (I_Recursive) then
               For_Project (I);
            else
               Append (I);
            end if;
         end loop;

         for I of Definition.Get_RO (View).Limited_Imports loop
            if Iter.Kind (I_Recursive) then
               For_Project (I);
            else
               Append (I);
            end if;
         end loop;
      end For_Imports;

      -----------------
      -- For_Project --
      -----------------

      procedure For_Project (View : Project.View.Object) is
         Position : Project.View.Set.Set.Cursor;
         Inserted : Boolean;
      begin
         Seen.Insert (View, Position, Inserted);

         if Inserted then
            --  Handle imports

            if Iter.Kind (I_Imported) or else Iter.Kind (I_Recursive) then
               For_Imports (View);
            end if;

            --  Handle extended if any

            if Iter.Kind (I_Extended) then
               declare
                  Data : constant Definition.Const_Ref :=
                           Definition.Get_RO (View);
               begin
                  if Data.Extended_Root.Is_Defined then
                     if Iter.Kind (I_Recursive) then
                        For_Project (Data.Extended_Root);
                     else
                        Append (Data.Extended_Root);
                     end if;
                  end if;
               end;
            end if;

            --  The project itself

            Append (View);

            --  Now if View is an aggregate or aggregate library project we
            --  need to run through all aggregated projects.

            if Iter.Kind (I_Aggregated) then
               For_Aggregated (View);
            end if;
         end if;
      end For_Project;

   begin
      For_Project (Iter.Root.Root);

      if Projects.Length = 0 then
         return No_Element;
      else
         return Cursor'(Projects, 1, Iter.Root.Root);
      end if;
   end First;

   ---------
   -- Get --
   ---------

   function Get
     (Tree    : Project.Tree.Object;
      Name    : Name_Type;
      Context : Context_Kind) return Project.View.Object
   is
      Position : constant View_Maps.Cursor :=
                   Tree.Views.Find (To_Lower (Name));
   begin
      if View_Maps.Has_Element (Position) then
         for V of View_Maps.Element (Position) loop
            declare
               Defs : constant Definition.Const_Ref := Definition.Get_RO (V);
            begin
               pragma Assert (Defs.Tree.all = Tree);

               if Defs.Context = Context then
                  return V;
               end if;
            end;
         end loop;
      end if;

      return Project.View.Undefined;
   end Get;

   --------------
   -- Get_File --
   --------------

   procedure Get_File
     (Self            : Object;
      Base_Name       : Simple_Name;
      Ambiguous       : out Boolean;
      Full_Path       : out Path_Name.Object;
      View            : Project.View.Object := Project.View.Undefined;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True;
      Predefined_Only : Boolean := False)
   is

      Found_Count : Natural := 0;
      --  Found files so far

      procedure Add_File
        (Name : Path_Name.Object; Check_Exist : Boolean := True);
      --  Add Name to matching files, when Check_Exist is True file existence
      --  is checked before Path is added to matching files.

      procedure Handle_Object_File;
      --  Set Full_Path with matching object file

      procedure Handle_Project_File;
      --  Set Full_Path with the first matching project

      procedure Handle_Source_File;
      --  Set Full_Path with matching source file

      --------------
      -- Add_File --
      --------------

      procedure Add_File
        (Name : Path_Name.Object; Check_Exist : Boolean := True) is
      begin
         if not Check_Exist or else Name.Exists then
            Found_Count := Found_Count + 1;

            if Found_Count = 1 then
               Full_Path := Name;

            else
               Ambiguous := True;
               if Name /= Full_Path then
                  Full_Path := Path_Name.Undefined;
               end if;
            end if;
         end if;
      end Add_File;

      ------------------------
      -- Handle_Object_File --
      ------------------------

      procedure Handle_Object_File is

         procedure Handle_Object_File_In_View (View : Project.View.Object);
         --  Set Full_Path with matching View's object file

         --------------------------------
         -- Handle_Object_File_In_View --
         --------------------------------

         procedure Handle_Object_File_In_View (View : Project.View.Object) is
         begin
            if View.Is_Library then
               Add_File (View.Object_Directory.Compose (Base_Name));

               if Found_Count = 0 then
                  Add_File (View.Library_Directory.Compose (Base_Name));
               end if;

               if Found_Count = 0 then
                  Add_File (View.Library_Ali_Directory.Compose (Base_Name));
               end if;

            elsif View.Kind = K_Standard then
               Add_File (View.Object_Directory.Compose (Base_Name));
            end if;
         end Handle_Object_File_In_View;

      begin
         if not Predefined_Only then
            if View.Is_Defined then
               Handle_Object_File_In_View (View);

            else
               for V in Self.Iterate
                 (Status => (Project.S_Externally_Built => Indeterminate))
               loop
                  Handle_Object_File_In_View (Project.Tree.Element (V));
               end loop;
            end if;
         end if;

         if Found_Count = 0 then
            if Self.Has_Runtime_Project then
               Handle_Object_File_In_View (Self.Runtime_Project);
            end if;
         end if;
      end Handle_Object_File;

      -------------------------
      -- Handle_Project_File --
      -------------------------

      procedure Handle_Project_File is
      begin
         for V in Self.Iterate
           (Status => (Project.S_Externally_Built => Indeterminate))
         loop
            declare
               View : constant Project.View.Object := Project.Tree.Element (V);
            begin
               if View.Path_Name.Simple_Name = Base_Name then
                  Full_Path := View.Path_Name;
               end if;
            end;
         end loop;
      end Handle_Project_File;

      ------------------------
      -- Handle_Source_File --
      ------------------------

      procedure Handle_Source_File is

         procedure Handle_Source_File_In_View (View : Project.View.Object);
         --  Set Full_Path with matching View's source file

         --------------------------------
         -- Handle_Source_File_In_View --
         --------------------------------

         procedure Handle_Source_File_In_View (View : Project.View.Object) is
            Full_Path : constant Path_Name.Object :=
                          View.Source_Path (Base_Name);
         begin
            if Full_Path.Is_Defined then
               Add_File (Full_Path, False);
            end if;
         end Handle_Source_File_In_View;

      begin
         if not Predefined_Only then
            if View.Is_Defined then
               Handle_Source_File_In_View (View);

            else
               for V in Self.Iterate
                 (Status => (Project.S_Externally_Built => Indeterminate))
               loop
                  Handle_Source_File_In_View (Project.Tree.Element (V));
               end loop;
            end if;
         end if;

         if Found_Count = 0 then
            if Self.Has_Runtime_Project then
               Handle_Source_File_In_View (Self.Runtime_Project);
            end if;
         end if;
      end Handle_Source_File;

   begin
      --  Initialize return values

      Ambiguous := False;
      Full_Path := Path_Name.Undefined;

      --  Handle project file

      if Project.Ensure_Extension (Base_Name) = Base_Name then
         Handle_Project_File;
         return;
      end if;

      --  Handle source file

      if Use_Source_Path then
         Handle_Source_File;
      end if;

      --  Handle object file

      if Found_Count = 0 and then Use_Object_Path then
         Handle_Object_File;
      end if;
   end Get_File;

   function Get_File
     (Self             : Object;
      Base_Name        : Simple_Name;
      View             : Project.View.Object := Project.View.Undefined;
      Use_Source_Path  : Boolean := True;
      Use_Object_Path  : Boolean := True;
      Predefined_Only  : Boolean := False;
      Return_Ambiguous : Boolean := True) return Path_Name.Object
   is
      File_Name : Path_Name.Object;
      Ambiguous : Boolean;
   begin
      Self.Get_File (Base_Name,
                     Ambiguous,
                     File_Name,
                     View,
                     Use_Source_Path,
                     Use_Object_Path,
                     Predefined_Only);

      if Ambiguous and then not Return_Ambiguous then
         return Path_Name.Undefined;
      else
         return File_Name;
      end if;
   end Get_File;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Self   : Object;
      Source : Path_Name.Object) return Project.View.Object
   is
      Pos : constant Filename_View.Cursor :=
              Self.Sources.Find
                (if Source.Has_Dir_Name
                 then Filename_Type (Source.Value)
                 else Source.Simple_Name);
   begin
      if Filename_View.Has_Element (Pos) then
         return Filename_View.Element (Pos);
      end if;

      return Project.View.Undefined;
   end Get_View;

   function Get_View
     (Self : Object;
      Unit : Name_Type) return Project.View.Object
   is
      Pos : constant Name_View.Cursor := Self.Units.Find (Unit);
   begin
      if Name_View.Has_Element (Pos) then
         return Name_View.Element (Pos);
      end if;

      return Project.View.Undefined;
   end Get_View;

   function Get_View
     (Tree : Project.Tree.Object;
      Id   : IDS.View_Id) return Project.View.Object
   is
      CV : constant Id_Maps.Cursor := Tree.View_Ids.Find (Id);
   begin
      if Id_Maps.Has_Element (CV) then
         return Id_Maps.Element (CV);
      else
         return Project.View.Undefined;
      end if;
   end Get_View;

   -----------------------
   -- Has_Configuration --
   -----------------------

   function Has_Configuration (Self : Object) return Boolean is
   begin
      return Self.Conf.Is_Defined;
   end Has_Configuration;

   -----------------
   -- Has_Context --
   -----------------

   function Has_Context (Self : Object) return Boolean is
   begin
      return not Self.Root.Context.Is_Empty;
   end Has_Context;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   ------------------
   -- Has_Messages --
   ------------------

   function Has_Messages (Self : Object) return Boolean is
   begin
      return not Self.Messages.Is_Empty;
   end Has_Messages;

   -------------------------
   -- Has_Runtime_Project --
   -------------------------

   function Has_Runtime_Project (Self : Object) return Boolean is
   begin
      return Self.Runtime.Is_Defined;
   end Has_Runtime_Project;

   ------------------
   -- Has_View_For --
   ------------------

   function Has_View_For
     (Self    : Object;
      Name    : Name_Type;
      Context : Context_Kind) return Boolean
   is
      View : constant Project.View.Object := Self.Get (Name, Context);
   begin
      if not View.Is_Defined then
         declare
            CV : constant Project.View.Object :=
                   (if Self.Has_Configuration
                    then Self.Conf.Corresponding_View
                    else Project.View.Undefined);
         begin
            --  If not found let's check if it is the configuration or runtime
            --  project. Note that this means that any Runtime or Config user's
            --  project name will have precedence.

            if CV.Is_Defined and then CV.Name = Name then
               return True;

            elsif Self.Has_Runtime_Project
              and then Self.Runtime.Name = Name
            then
               return True;
            end if;
         end;

         return False;

      else
         return True;
      end if;
   end Has_View_For;

   -----------------
   -- Instance_Of --
   -----------------

   function Instance_Of
     (Self        : Object;
      Instance_Id : GPR2.View_Ids.View_Id) return View.Object is
   begin
      return Self.View_Ids.Element (Instance_Id);
   end Instance_Of;

   ------------------------
   -- Invalidate_Sources --
   ------------------------

   procedure Invalidate_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined) is
   begin
      if not View.Is_Defined then
         for V of Self.Views_Set loop
            Definition.Get (V).Sources_Signature :=
              GPR2.Context.Default_Signature;
         end loop;

         Self.Self.Sources_Loaded := False;

      else
         Definition.Get (View).Sources_Signature :=
           GPR2.Context.Default_Signature;

         if View.Is_Aggregated_In_Library then
            for Agg of View.Aggregate_Libraries loop
               Self.Invalidate_Sources (Agg);
            end loop;
         end if;
      end if;
   end Invalidate_Sources;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Position : Cursor) return Boolean is
   begin
      return Position.Views (Position.Current) = Position.Root;
   end Is_Root;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self   : Object;
      Kind   : Iterator_Control := Default_Iterator;
      Filter : Filter_Control   := Default_Filter;
      Status : Status_Control   := Default_Status)
      return Project_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Kind, Filter, Status, Self.Self);
   end Iterate;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self             : in out Object;
      Filename         : Path_Name.Object;
      Context          : GPR2.Context.Object;
      Config           : PC.Object                 := PC.Undefined;
      Project_Dir      : Path_Name.Object          := Path_Name.Undefined;
      Build_Path       : Path_Name.Object          := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type        := No_Name;
      Src_Subdirs      : Optional_Name_Type        := No_Name;
      Check_Shared_Lib : Boolean                   := True;
      Absent_Dir_Error : Boolean                   := False;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Pre_Conf_Mode    : Boolean                   := False)
   is

      Project_Path  : Path_Name.Object;
      Root_Context  : GPR2.Context.Object := Context;
      Def           : Definition.Ref;

   begin
      Self.Self := Self'Unchecked_Access;

      --  If re-loading, invalidate the views cache
      for V of Self.Views_Set loop
         Definition.Get (V).Clear_Cache;
      end loop;

      --  First record and parse the configuration object, this is needed as
      --  used to check the target in Set_Project_Search_Paths above.

      if Config.Is_Defined then
         --  Set Tree for this config project
         Self.Conf := Config;

         for M of Config.Log_Messages loop
            Self.Messages.Append (M);
         end loop;

         if Self.Messages.Has_Error then
            raise Project_Error with "configuration project has errors";
         end if;

         if Config.Has_Externals then
            Update_Context (Root_Context, Config.Externals);
         end if;

         Definition.Bind_Configuration_To_Tree (Self.Conf, Self.Self);

         declare
            C_View : Project.View.Object := Self.Conf.Corresponding_View;
            P_Data : constant Definition.Ref := Definition.Get_RW (C_View);
         begin
            --  Set and record the tree now, needed for the parsing

            P_Data.Tree := Self.Self;
            P_Data.Is_Root := True;

            --  Parse the configuration project, no need for full/complex
            --  parsing as a configuration project is a simple project no
            --  with clauses.

            Parser.Project.Process
              (P_Data.Trees.Project,
               Self,
               Root_Context,
               C_View,
               P_Data.Attrs,
               P_Data.Vars,
               P_Data.Packs,
               P_Data.Types);

            if Self.Conf.Is_Defined then
               Update_Project_Search_Path_From_Config (Self, Self.Conf);
            end if;

            pragma Assert
              (P_Data.Kind = K_Configuration,
               "expected K_Configuration, found : " & P_Data.Kind'Img);
         end;
      end if;

      Self.Project_Dir      := Project_Dir;
      Self.Build_Path       := Build_Path;
      Self.Subdirs          := To_Unbounded_String (String (Subdirs));
      Self.Src_Subdirs      := To_Unbounded_String (String (Src_Subdirs));
      Self.Check_Shared_Lib := Check_Shared_Lib;
      Self.Implicit_With    := Implicit_With;
      Self.Absent_Dir_Error := Absent_Dir_Error;

      Self.Pre_Conf_Mode := Pre_Conf_Mode;

      if Filename.Is_Implicit_Project then
         Project_Path := Project_Dir;

      elsif Filename.Has_Dir_Name then
         Project_Path := Filename;

      else
         --  If project directory still not defined, search it in full set
         --  of search paths.

         Project_Path := Create (Filename.Name, Self.Search_Paths);

         if not Build_Path.Is_Defined then
            Self.Build_Path := Path_Name.Create_Directory
              (Filename_Type (Project_Path.Dir_Name));
         end if;
      end if;

      --  Add full project path in the message log
      Self.Messages.Append
        (Message.Create
           (Message.Information,
            "Parsing """ & Project_Path.Value & """",
            Source_Reference.Create (Project_Path.Value, 0, 0)));

      --  Add all search paths into the message log

      Self.Messages.Append
        (Message.Create
           (Message.Information,
            "search path:",
            Source_Reference.Create (Project_Path.Value, 0, 0)));

      for P of Self.Search_Paths loop
         Self.Messages.Append
           (Message.Create
              (Message.Information,
               P.Value,
               Source_Reference.Create (Project_Path.Value, 0, 0)));
      end loop;

      Self.Root := Recursive_Load
        (Self,
         Filename => Project_Path,
         Context  => Root);

      --  Do nothing more if there are errors during the parsing

      if not Self.Messages.Has_Error then
         --  Add to root view's externals, configuration project externals

         Def := Definition.Get (Self.Root);

         if Config.Is_Defined and then Config.Has_Externals then

            for E of Config.Externals loop
               if not Def.Externals.Contains (E) then
                  Def.Externals.Append (E);
               end if;
            end loop;
         end if;

         for V_Data of Self.Views_Set loop
            --  Compute the external dependencies for the views. This
            --  is the set of external used in the project and in all
            --  imported/extended project.

            for E of Definition.Get_RO (V_Data).Externals loop
               if not Def.Externals.Contains (E) then
                  --  Note that if we have an aggregate project, then
                  --  we are not dependent on the external if it is
                  --  statically redefined in the aggregate project. But
                  --  at this point we have not yet parsed the project.
                  --
                  --  The externals will be removed in Set_Context when
                  --  the parsing is done.

                  Def.Externals.Append (E);
               end if;
            end loop;
         end loop;

         Set_Context (Self, Context);

         Definition.Check_Same_Name_Extended (Self.Root);

         if not Self.Pre_Conf_Mode then
            --  We only need those checks if we are not in pre-confinguration
            --  stage, otherwise we might have errors if a project references
            --  corresponding attributes from a not yet found project and their
            --  values default to empty ones.
            Definition.Check_Aggregate_Library_Dirs (Self.Root);
            Definition.Check_Package_Naming (Self.Root);
         end if;
      end if;

      if not Self.Pre_Conf_Mode and then Self.Messages.Has_Error then
         raise Project_Error with Project_Path.Value & " syntax error";
      end if;
   end Load;

   -------------------
   -- Load_Autoconf --
   -------------------

   procedure Load_Autoconf
     (Self              : in out Object;
      Filename          : Path_Name.Object;
      Context           : GPR2.Context.Object;
      Project_Dir       : Path_Name.Object          := Path_Name.Undefined;
      Build_Path        : Path_Name.Object          := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type        := No_Name;
      Src_Subdirs       : Optional_Name_Type        := No_Name;
      Check_Shared_Lib  : Boolean                   := True;
      Absent_Dir_Error  : Boolean                   := False;
      Implicit_With     : GPR2.Path_Name.Set.Object :=
                            GPR2.Path_Name.Set.Empty_Set;
      Target            : Optional_Name_Type        := No_Name;
      Language_Runtimes : Containers.Lang_Value_Map :=
                            Containers.Lang_Value_Maps.Empty_Map;
      Base              : GPR2.KB.Object            := GPR2.KB.Undefined)
   is
      Languages   : Containers.Language_Set;
      Conf        : Project.Configuration.Object;
      GNAT_Prefix : constant String := Get_Tools_Directory;
      Default_Cfg : Path_Name.Object;
      Lang_Sloc   : Attribute.Object;
      --  Keep languages attribute for Sloc parameter in error message

      Old_Paths    : constant Path_Name.Set.Object := Self.Search_Paths;
      --  Search paths may be affected by -aP options passed by gprtools,
      --  so we need to keep the original search paths for the reconfiguration
      --  stage.
      Old_Messages : constant Log.Object := Self.Messages;
      --  Likewise, Self may already have some messages and we don't want
      --  to loose them when we unload the tree for conf/reconf.

      function Actual_Target return Name_Type;
      --  Returns the target, depending on the parsing stage

      procedure Add_Languages (View : Project.View.Object);
      --  Adds project languages into the Languages container to configure.
      --  Warns about project has no languages.

      function Conf_Descriptions return Project.Configuration.Description_Set;
      --  Returns set of descriptions for configuration creation

      function Default_Config_File return Filename_Type;
      --  Returns default config filename

      function Runtime
        (Language : Language_Id) return Optional_Name_Type;
      --  Returns the runtime to use during configuration for the specified
      --  language.

      function Toolchain_Name
        (Language : Language_Id) return Optional_Name_Type;
      --  Returns toolchain name specified by Toolchain_Name attribute

      function Toolchain_Version
        (Language : Language_Id) return Optional_Name_Type;
      --  Returns toolchain version specified by Required_Toolchain_Version
      --  attribute.

      function Toolchain_Path
        (Language : Language_Id) return Filename_Optional;
      --  Returns toolchain search path specified by Toolchain_Path attribute

      type Reconfiguration_Status is (Unchanged, Extended, Incompatible);

      Reconf_Status : Reconfiguration_Status;

      procedure Compare_Configurations
        (Before : Project.Configuration.Description_Set;
         After  : Project.Configuration.Description_Set;
         Result : out Reconfiguration_Status);
      --  Compares two sets of description sets to check for possible changes
      --  that happened after autoconfiguration. For example, new languages
      --  may be added after finding missing imports. Also, previously
      --  unresolved constructs may cause changes in already established
      --  descriptions (toolchain-related attribute under a case statement
      --  depending on a variable from a missing import).
      --  Returns Unchanged when Before and After are identical.
      --  Returns Extended when all descriptions for languages present in
      --  Before are identical to those in After, but there are new extra
      --  languages in after.
      --  Otherwise returns Incompatible and adds corresponding error messages.

      -------------------
      -- Actual_Target --
      -------------------

      function Actual_Target return Name_Type is
         Tmp_Attr : GPR2.Project.Attribute.Object;
      begin
         if Target /= No_Name and then Target /= "all" then
            --  If Target is specified as parameter, this always takes
            --  precedence
            return Target;
         end if;

         if Self.Root.Is_Defined
           and then Self.Root.Check_Attribute
                      (PRA.Target, Check_Extended => True, Result => Tmp_Attr)
         then
            --  Check if the project explicitly defines the attribute or if
            --  this comes from a default value.

            if not Tmp_Attr.Is_Default and then
              not Tmp_Attr.Value.Is_From_Default
            then
               return Name_Type (Tmp_Attr.Value.Text);
            end if;
         end if;

         --  No explicit target as parameter or in project: return "all"
         return "all";
      end Actual_Target;

      -------------------
      -- Add_Languages --
      -------------------

      procedure Add_Languages (View : Project.View.Object) is
      begin
         if not View.Is_Abstract
           and then View.Languages.Length = 0
         then
            Self.Append_Message
              (Message.Create
                 (Level   => Message.Warning,
                  Message => "no language for the project "
                  & String (View.Name),
                  Sloc    => View.Attributes.Languages));
         end if;

         if View.Has_Languages then
            for L of View.Languages loop
               Languages.Include (+Name_Type (L.Text));
            end loop;

            --  Keep languages attribute for possible error message Sloc
            --  parameter.

            Lang_Sloc := View.Attributes.Languages;
         end if;
      end Add_Languages;

      ----------------------------
      -- Compare_Configurations --
      ----------------------------

      procedure Compare_Configurations
        (Before : Project.Configuration.Description_Set;
         After  : Project.Configuration.Description_Set;
         Result : out Reconfiguration_Status)
      is
         use Project.Configuration;

         Found_In_After : Boolean;

         function Error (Before, After : Description) return String;
         --  Returns string with incompatible parts of descriptions

         -----------
         -- Error --
         -----------

         function Error (Before, After : Description) return String is
            Result : Unbounded_String;

            procedure Append_Result
              (Param   : String;
               Old_Val : Optional_Name_Type;
               New_Val : Optional_Name_Type);
            --  Adds new parts of error message

            -------------------
            -- Append_Result --
            -------------------

            procedure Append_Result
              (Param   : String;
               Old_Val : Optional_Name_Type;
               New_Val : Optional_Name_Type)
            is
               Msg : constant String :=
                       Param & " """ & String (Old_Val) & """ changed to """
                       & String (New_Val) & """";
            begin
               if Result = Null_Unbounded_String then
                  Result := To_Unbounded_String (Msg);
               else
                  Append (Result, "; " & Msg);
               end if;
            end Append_Result;

         begin
            if Version (Before) /= Version (After) then
               Append_Result ("version", Version (Before), Version (After));
            end if;

            if Runtime (Before) /= Runtime (After) then
               Append_Result ("runtime", Runtime (Before), Runtime (After));
            end if;

            if Path (Before) /= Path (After) then
               Append_Result
                 ("path",
                  Optional_Name_Type (Path (Before)),
                  Optional_Name_Type (Path (After)));
            end if;

            if Name (Before) /= Name (After) then
               Append_Result ("name", Name (Before), Name (After));
            end if;

            return  To_String (Result);
         end Error;

      begin
         for Descr_B of Before loop
            Found_In_After := False;

            for Descr_A of After loop
               if Language (Descr_B) = Language (Descr_A) then
                  if Descr_B = Descr_A then
                     Found_In_After := True;
                     exit;

                  else
                     Self.Append_Message
                       (Message.Create
                          (Level   => Message.Error,
                           Message => "incompatible change for language "
                           & Image (Language (Descr_B))
                           & " during reconfiguration",
                           Sloc    => Source_Reference.Create
                             (Self.Root.Path_Name.Value, 0, 0)));

                     Self.Append_Message
                       (Message.Create
                          (Level   => Message.Error,
                           Message => Error (Descr_B, Descr_A),
                           Sloc    => Source_Reference.Create
                             (Self.Root.Path_Name.Value, 0, 0)));

                     Result := Incompatible;

                     return;
                  end if;
               end if;
            end loop;

            if not Found_In_After then
               Self.Append_Message
                 (Message.Create
                    (Level   => Message.Error,
                     Message => "language " & Image (Language (Descr_B))
                     & " missing for reconfiguration",
                     Sloc    => Source_Reference.Create
                       (Self.Root.Path_Name.Value, 0, 0)));
               Result := Incompatible;
               return;
            end if;

         end loop;

         if Before'Length = After'Length then
            Result := Unchanged;
         else
            Result := Extended;
         end if;

      end Compare_Configurations;

      -----------------------
      -- Conf_Descriptions --
      -----------------------

      function Conf_Descriptions return Project.Configuration.Description_Set
      is
         Descr_Index : Natural := 0;
         Result      : Project.Configuration.Description_Set
                         (1 .. Positive (Languages.Length));
      begin
         for L of Languages loop
            Descr_Index := Descr_Index + 1;

            Result (Descr_Index) :=
              Project.Configuration.Create
                (Language => L,
                 Version  => Toolchain_Version (L),
                 Runtime  => Runtime (L),
                 Path     => Toolchain_Path (L),
                 Name     => Toolchain_Name (L));
         end loop;

         return Result;
      end Conf_Descriptions;

      -------------------------
      -- Default_Config_File --
      -------------------------

      function Default_Config_File return Filename_Type is
         Ada_RTS_Val : constant Value_Type :=
                         Containers.Value_Or_Default
                           (Language_Runtimes, Ada_Language);
         Ada_RTS     : constant Filename_Optional :=
                         (if Ada_RTS_Val = No_Value then No_Filename
                          else Filename_Optional
                            (Ada.Directories.Simple_Name (Ada_RTS_Val)));
      begin
         if Target not in No_Name | "all" then
            return Filename_Type (Target)
              & (if Ada_RTS = No_Filename then "" else "-" & Ada_RTS)
              & Config_File_Extension;

         elsif Ada_RTS /= No_Filename then
            return Ada_RTS & Config_File_Extension;

         else
            declare
               GPR_Config : constant String := "GPR_CONFIG";
               Filename   : constant String :=
                              Environment_Variables.Value (GPR_Config, "");
            begin
               if Filename = "" then
                  return Default_Config_Name;
               else
                  return Filename_Type (Filename);
               end if;
            end;
         end if;
      end Default_Config_File;

      -------------
      -- Runtime --
      -------------

      function Runtime
        (Language : Language_Id) return Optional_Name_Type
      is
         function Attr_As_Abs_Path
           (Attr : Attribute.Object;
            View : GPR2.Project.View.Object) return Optional_Name_Type;

         ----------------------
         -- Attr_As_Abs_Path --
         ----------------------

         function Attr_As_Abs_Path
           (Attr : Attribute.Object;
            View : GPR2.Project.View.Object) return Optional_Name_Type
         is
            Value              : constant String := String (Attr.Value.Text);
            Has_Dir_Indication : Boolean := False;
         begin
            for C of Value loop
               if C = '/' or else C = '\' then
                  Has_Dir_Indication := True;
                  exit;
               end if;
            end loop;

            if Has_Dir_Indication then
               if GNAT.OS_Lib.Is_Absolute_Path (Value) then
                  return Name_Type (Value);
               else
                  return Name_Type (GNAT.OS_Lib.Normalize_Pathname
                                    (Value, View.Dir_Name.Value));
               end if;
            else
               return Optional_Name_Type (Value);
            end if;
         end Attr_As_Abs_Path;

         Tmp_Attr : Attribute.Object;
         LRT      : constant Value_Type :=
                      Containers.Value_Or_Default
                        (Language_Runtimes, Language);

      begin
         if LRT /= No_Value then
            --  Return the value given as parameter
            return Name_Type (LRT);
         end if;

         if Self.Root.Is_Defined
           and then Self.Root.Check_Attribute
                      (PRA.Runtime,
                       Attribute_Index.Create
                         (Value_Not_Empty (Name (Language))),
                       Check_Extended => True, Result => Tmp_Attr)
         then
            return Attr_As_Abs_Path (Tmp_Attr, Self.Root);
         end if;

         return No_Name;
      end Runtime;

      --------------------
      -- Toolchain_Name --
      --------------------

      function Toolchain_Name
        (Language : Language_Id) return Optional_Name_Type
      is
         Tmp_Attr : GPR2.Project.Attribute.Object;
      begin
         if Self.Root.Is_Defined
           and then Self.Root.Check_Attribute
                      (PRA.Toolchain_Name,
                       Attribute_Index.Create
                         (Value_Not_Empty (Name (Language))),
                       Check_Extended => True, Result => Tmp_Attr)
           and then Tmp_Attr.Value.Text /= ""
         then
            return Name_Type (Tmp_Attr.Value.Text);
         end if;

         return No_Name;
      end Toolchain_Name;

      --------------------
      -- Toolchain_Path --
      --------------------

      function Toolchain_Path
        (Language : Language_Id) return Filename_Optional
      is
         Tmp_Attr : GPR2.Project.Attribute.Object;
      begin
         if Self.Root.Is_Defined
           and then Self.Root.Check_Attribute
                      (PRA.Toolchain_Path,
                       Attribute_Index.Create
                         (Value_Not_Empty (Name (Language))),
                       Check_Extended => True, Result => Tmp_Attr)
           and then Tmp_Attr.Value.Text /= ""
         then
            return Filename_Type
              (GNAT.OS_Lib.Normalize_Pathname
                 (Tmp_Attr.Value.Text, Self.Root.Dir_Name.Value));
         end if;

         return No_Filename;
      end Toolchain_Path;

      -----------------------
      -- Toolchain_Version --
      -----------------------

      function Toolchain_Version
        (Language : Language_Id) return Optional_Name_Type
      is
         Tmp_Attr : GPR2.Project.Attribute.Object;
      begin
         if Self.Root.Is_Defined
           and then Self.Root.Check_Attribute
                      (PRA.Required_Toolchain_Version,
                       Attribute_Index.Create
                         (Value_Not_Empty (Name (Language))),
                       Check_Extended => True, Result => Tmp_Attr)
           and then Tmp_Attr.Value.Text /= ""
         then
            return Name_Type (Tmp_Attr.Value.Text);
         end if;

         return No_Name;
      end Toolchain_Version;

      package Description_Set_Holders is new Ada.Containers.Indefinite_Holders
        (Project.Configuration.Description_Set,
         Project.Configuration."=");
      Pre_Conf_Description   : Description_Set_Holders.Holder;
      Post_Conf_Description  : Description_Set_Holders.Holder;
      Has_Errors             : Boolean;
      use Description_Set_Holders;

   begin

      if GNAT_Prefix = "" then
         --  No GNAT, use default config only in current directory

         Default_Cfg := Path_Name.Create_File (Default_Config_File);

      else
         --  GNAT found, look for the default config first in the current
         --  directory and then in the GNAT/share/gpr

         Default_Cfg :=
           Create
             (Default_Config_File,
              Path_Name.Set.To_Set
                (Path_Name.Create_Directory
                   ("share", Filename_Type (GNAT_Prefix)).Compose
                 ("gpr", Directory => True)));
      end if;

      if Default_Cfg.Exists then
         Conf := Project.Configuration.Load
           (Default_Cfg, (if Target = No_Name then "all" else Target));
      end if;

      if Base.Is_Defined then
         Self.Base := Base;
      end if;

      if not Conf.Is_Defined then
         --  Default configuration file does not exists. Generate configuration
         --  automatically.

         --  This involves some delicate bootstrap:
         --  1- we load the project without configuration
         --  2- using the loaded project, we determine
         --     * the Target: if explicitely given to us, this one is used,
         --       else if the project defines it, this one is used, else the
         --       host's value is used.
         --     * the list of languages
         --     and we load a configuration for the above.
         --  3- we then reload the project with the configuration

         Self.Load
           (Filename, Context,
            Project_Dir      => Project_Dir,
            Build_Path       => Build_Path,
            Subdirs          => Subdirs,
            Src_Subdirs      => Src_Subdirs,
            Check_Shared_Lib => Check_Shared_Lib,
            Absent_Dir_Error => False,
            Implicit_With    => Implicit_With,
            Pre_Conf_Mode    => True);
         --  Ignore possible missing dirs and imported projects since they can
         --  depend on the result of autoconfiguration.

         Has_Errors := Self.Messages.Has_Error;

         --  Ignore messages issued with this initial load: as we don't have
         --  a valid configuration here, we can't really know whether they
         --  are meaningful or not
         Self.Messages.Clear;

         if not Has_Errors then
            for C in Self.Iterate
              (Filter =>
                 (F_Aggregate | F_Aggregate_Library => False, others => True))
            loop
               Add_Languages (Element (C));
            end loop;

            if Languages.Length = 0 then
               Self.Append_Message
                 (Message.Create
                    (Level   => Message.Warning,
                     Message => "no language for the projects tree: "
                     & "configuration skipped",
                     Sloc    => (if Lang_Sloc.Is_Defined
                                 then Lang_Sloc
                                 else Source_Reference.Create
                                        (Self.Root.Path_Name.Value, 0, 0))));
               return;
            end if;

         else
            --  Generate a default config, since a critical failure occurred:
            --  this will reload the project in normal mode and print the
            --  relevant error messages.

            Languages.Include (Ada_Language);
         end if;

         Pre_Conf_Description := To_Holder (Conf_Descriptions);

         if not Self.Base.Is_Defined then
            Self.Base := GPR2.KB.Create (GPR2.KB.Default_Flags);
         end if;

         Conf := Project.Configuration.Create
           (Pre_Conf_Description.Element,
            Actual_Target,
            (if Self.Root.Is_Defined then Self.Root.Path_Name else Filename),
            Self.Base);

         --  Unload the project that was loaded without configuration.
         --  We need to backup the messages and default search path:
         --  messages issued during configuration are relevant, together with
         --  already computed search paths

         Self.Unload;
         Self.Messages := Old_Messages;
         Self.Search_Paths := Old_Paths;
      end if;

      Self.Load
        ((if Self.Root.Is_Defined then Self.Root.Path_Name else Filename),
         Context, Conf,
         Project_Dir      => Project_Dir,
         Build_Path       => Build_Path,
         Subdirs          => Subdirs,
         Src_Subdirs      => Src_Subdirs,
         Check_Shared_Lib => Check_Shared_Lib,
         Absent_Dir_Error => Absent_Dir_Error,
         Implicit_With    => Implicit_With);

      if Default_Cfg.Exists then
         --  No need for reconfiguration if explicit default configuration
         --  project has been specified.
         return;
      end if;

      --  Configuration parameters might have changed, i.e. new languages
      --  may be added from missing imported projects that have been found
      --  after search path update from configuration data. We need to check
      --  for that and perform a reconfiguration if necessary.

      Languages.Clear;

      for C in Self.Iterate
        (Filter =>
           (F_Aggregate | F_Aggregate_Library => False, others => True))
      loop
         Add_Languages (Element (C));
      end loop;

      Post_Conf_Description := To_Holder (Conf_Descriptions);

      if not Pre_Conf_Description.Is_Empty then
         Compare_Configurations
           (Pre_Conf_Description.Element,
            Post_Conf_Description.Element,
            Reconf_Status);

         if Reconf_Status = Unchanged then
            --  Nothing changed, no need for reconfiguration
            return;
         end if;

         if Reconf_Status = Incompatible then
            raise Project_Error with "reconfiguration error";
         end if;
      end if;

      --  We need to reconfigure in order to account for new languages

      Conf := Project.Configuration.Create
        (Post_Conf_Description.Element,
         Actual_Target,
         Self.Root.Path_Name,
         Self.Base);

      Self.Unload;
      Self.Messages := Old_Messages;
      Self.Search_Paths := Old_Paths;

      Self.Load
        ((if Self.Root.Is_Defined then Self.Root.Path_Name else Filename),
         Context, Conf,
         Project_Dir      => Project_Dir,
         Build_Path       => Build_Path,
         Subdirs          => Subdirs,
         Src_Subdirs      => Src_Subdirs,
         Check_Shared_Lib => Check_Shared_Lib,
         Absent_Dir_Error => Absent_Dir_Error,
         Implicit_With    => Implicit_With);
   end Load_Autoconf;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration
     (Self     : in out Object;
      Filename : Path_Name.Object) is
   begin
      pragma Assert (Self.Self = Self'Unrestricted_Access);

      Self.Conf := PC.Load (Filename);
      Definition.Bind_Configuration_To_Tree (Self.Conf, Self.Self);

      for M of Self.Conf.Log_Messages loop
         Self.Messages.Append (M);
      end loop;

      if not Self.Messages.Has_Error then
         Set_Context (Self);
      end if;
   end Load_Configuration;

   ------------------
   -- Log_Messages --
   ------------------

   function Log_Messages (Self : Object) return not null access Log.Object is
   begin
      return Self.Self.Messages'Access;
   end Log_Messages;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
      pragma Unreferenced (Iter);
      C : Cursor := Position;
   begin
      if C.Current < Natural (C.Views.Length) then
         C.Current := C.Current + 1;
         return C;
      else
         return No_Element;
      end if;
   end Next;

   -------------------
   -- Ordered_Views --
   -------------------

   function Ordered_Views (Self : Object) return View.Vector.Object is
      use GPR2.View_Ids;
      use GPR2.View_Ids.DAGs;
      Result : View.Vector.Object;

   begin
      for Id of Self.View_DAG.Topological_Sort loop
         Result.Append (Self.Instance_Of (Id));
      end loop;

      return Result;
   end Ordered_Views;

   --------------------------
   -- Project_Search_Paths --
   --------------------------

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object is
   begin
      return Self.Search_Paths;
   end Project_Search_Paths;

   -----------------
   -- Record_View --
   -----------------

   procedure Record_View
     (Self   : in out Object;
      View   : GPR2.Project.View.Object;
      Source : Path_Name.Object;
      Unit   : Name_Type) is
   begin
      if not View.Is_Extended then
         Self.Units.Include (Unit, View);
         Self.Sources.Include (Filename_Type (Source.Value), View);
         Self.Sources.Include (Source.Simple_Name, View);
      end if;
   end Record_View;

   --------------------
   -- Recursive_Load --
   --------------------

   function Recursive_Load
     (Self          : in out Object;
      Filename      : Path_Name.Object;
      Context       : Context_Kind;
      Starting_From : View.Object := View.Undefined) return View.Object
   is

      type Relation_Status is
        (Root,            --  Root project
         Extended,        --  Extended project
         Aggregated,      --  In an aggregate project
         Simple);         --  Import, Limited import or aggregate library

      Search_Path : Path_Name.Set.Object := Self.Search_Paths;
      PP          : Attribute.Object;

      function Load (Filename : Path_Name.Object) return Definition.Data;
      --  Returns the Data definition for the given project

      function Internal
        (Filename        : Path_Name.Object;
         Aggregate       : View.Object;
         Status          : Relation_Status;
         Parent          : View.Object;
         Extends_Ctx     : View.Vector.Object) return View.Object;
      --  Internal function doing the actual load of the tree.
      --  Filename:  the project to laod
      --  Aggregate: if defines, is set to the aggregate project that includes
      --             "Filename".
      --  Status:    denotes the context of the load.
      --             "root": the root project is being loaded.
      --             "extended": the current project is extending "Filename"
      --             "aggregated": the current project is aggregating
      --                "filename". It can be either an aggregate project or
      --                an aggregate library project.
      --             "simple": a regular import.
      --  Parent:    the loading project.
      --  Extends_Ctx: In case the project is loaded in a subtree of an
      --               extends all, the extending project is .

      function Is_Limited
         (View         : GPR2.Project.View.Object;
          Import_Path  : Path_Name.Object) return Boolean;
      --  Returns True if the Import_Path is a limited with in View

      procedure Propagate_Aggregate_Library
        (View        : in out GPR2.Project.View.Object;
         Agg_Library : GPR2.View_Ids.View_Id);
      --  Make sure that all views in the subtree of View reference the
      --  Aggregate Library.
      --
      --  This is needed if several aggregate libraries exist in the tree
      --  and they reference the same project. This is also needed if a
      --  subproject is withed from the regular project before being also
      --  in the closure of the aggregate library, as in these cases the
      --  view will already exist before the aggregate library is loaded.

      --------------
      -- Internal --
      --------------

      function Internal
        (Filename        : Path_Name.Object;
         Aggregate       : View.Object;
         Status          : Relation_Status;
         Parent          : View.Object;
         Extends_Ctx     : View.Vector.Object) return View.Object
      is
         Extending : constant IDS.View_Id :=
                       (if Status = Extended then Parent.Id
                        elsif not Extends_Ctx.Is_Empty
                        then Extends_Ctx.First_Element.Id
                        else View_Ids.Undefined);
         --  A project can be extended either explicitly (Status is then
         --  Extended and the parent points to the extending project), or
         --  implicitly (withed unit of the extending project replaces withed
         --  using of the extended project, creating an implicit extension).
         --  In this case Extends_Ctx is not empty.

         Id        : constant IDS.View_Id :=
                       IDS.Create
                         (Project_File => Filename,
                          Context      => Context,
                          Extending    => Extending);
         View      : Project.View.Object := Self.Get_View (Id);

      begin
         --  If the view is already defined just return it
         if not View.Is_Defined then
            declare
               Data : Definition.Data := Load (Filename);
            begin
               --  If there are parsing errors, do not go further

               if Self.Messages.Has_Error then
                  return View;
               end if;

               --  Compute directory used as project file directory
               --  This is influenced by the Project_Dir parameters used on
               --  load to simulate that a project is in another location.

               Data.Path := (if Self.Project_Dir.Is_Defined
                             then Self.Project_Dir
                             else Path_Name.Create_Directory
                                    (Filename_Type (Filename.Dir_Name)));

               --  If parent view is an extending project keep track of the
               --  relationship.

               if Status = Extended then
                  Data.Extending := Definition.Weak (Parent);
               elsif not Extends_Ctx.Is_Empty then
                  Data.Extending :=
                    Definition.Weak (Extends_Ctx.First_Element);
               end if;

               --  Update context associated with the the list of externals
               --  defined in that project file.

               Update_Context (Self.Context (Context), Data.Externals);

               --  At this stage even if not complete we can create the view
               --  and register it so that we can have references to it.

               Data.Context     := Context;
               Data.Is_Root     := Status = Root;
               Data.Is_Imported := Status = Simple;
               Data.Unique_Id   := Id;
               View := Register_View (Data);
            end;

            declare
               Data            : constant Definition.Ref :=
                                   Definition.Get_RW (View);
               New_Extends_Ctx : GPR2.Project.View.Vector.Object :=
                                   Extends_Ctx;
               --  Extends all context for the extended project if any

            begin
               --  Set root view regarding context namespace
               --  ??? (need more explanation)

               if not Parent.Is_Defined then
                  --  This is the root project or an aggregate project. This
                  --  create a new namespace (i.e root in the subtree)

                  Data.Root_View := Definition.Weak (View);

               else
                  --  ??? what happens if a project is withed inside several
                  --  aggregate projects ???

                  Data.Root_View := Definition.Get_RO (Parent).Root_View;
                  Data.Agg_Libraries :=
                    Definition.Get_RO (Parent).Agg_Libraries;

                  if Parent.Kind = K_Aggregate_Library then
                     Data.Agg_Libraries.Include (Parent.Id);
                  end if;
               end if;

               --  Update the extends all view

               if not Extends_Ctx.Is_Empty then
                  declare
                     --  We need to add the current view to the extended
                     --  property of the extends all view. To this end, we
                     --  use a local variable that will allow us to write
                     --  its definition
                     Temp_View : Project.View.Object :=
                                   Extends_Ctx.First_Element;
                  begin
                     Definition.Get_RW (Temp_View).Extended.Include (View);
                  end;
               end if;

               --  Load all imported projects

               if Status = Extended then
                  --  Temporarily add the parent to the list of
                  --  extending projects so that we can simply loop over
                  --  those to gather all the imports of extending projects

                  New_Extends_Ctx.Prepend (Parent);
               end if;

               for Project of Data.Trees.Imports loop
                  declare
                     Imported_View : GPR2.Project.View.Object;
                     Limited_With  : Boolean := False;

                  begin
                     --  Look for the list of imported projects from the
                     --  extending project to see if we need to substitute
                     --  regular imports with their extended view.

                     Extends_Loop :
                     for Ext of New_Extends_Ctx loop
                        Import_Loop :
                        for Imp of Ext.Imports loop
                           if Imp.Is_Extending
                             and then
                               Imp.Extended_Root.Path_Name = Project.Path_Name
                           then
                              Imported_View := Imp;
                              exit Extends_Loop;
                           end if;
                        end loop Import_Loop;
                     end loop Extends_Loop;

                     if not Imported_View.Is_Defined then
                        Imported_View :=
                          Internal
                            (Project.Path_Name,
                             Aggregate   =>
                               GPR2.Project.View.Undefined,
                             Status      => Simple,
                             Parent      => View,
                             Extends_Ctx => Extends_Ctx);

                        if not Imported_View.Is_Defined then
                           --  Some issue happened

                           pragma Assert (Self.Messages.Has_Error);

                           return GPR2.Project.View.Undefined;
                        end if;

                        --  limited with and with are tracked separately due
                        --  their very distinct nature.

                        if Is_Limited (View, Project.Path_Name) then
                           Limited_With := True;
                        end if;
                     end if;

                     if Limited_With then
                        Data.Limited_Imports.Insert
                          (Project.Name, Imported_View);

                     else
                        Data.Imports.Insert
                          (Project.Name, Imported_View);
                     end if;
                  end;
               end loop;

               if Status = Extended then
                  --  Remove Parent from New_Extends_Ctx: simple extension
                  --  don't propagate to the subtree.
                  New_Extends_Ctx.Delete_First;
               end if;

               --  Load the extended project if any:

               if Data.Trees.Extended.Is_Defined then
                  if Data.Trees.Project.Is_Extending_All then
                     --  Update the extends context in case we do an
                     --  extends all: this is applied to the whole sub-tree.

                     New_Extends_Ctx.Prepend (View);
                  end if;

                  declare
                     Extended_View : constant GPR2.Project.View.Object :=
                                       Internal
                                         (Data.Trees.Extended.Path_Name,
                                          Aggregate     =>
                                            GPR2.Project.View.Undefined,
                                          Status        => Extended,
                                          Parent        => View,
                                          Extends_Ctx   => New_Extends_Ctx);
                  begin
                     if Extended_View.Is_Defined then
                        Data.Extended.Include (Extended_View);
                        Data.Extended_Root := Extended_View;
                     end if;
                  end;
               end if;
            end;

         elsif Parent.Is_Defined
           and then Parent.Kind = K_Aggregate_Library
         then
            --  We need to keep track of aggregate libraries
            --  closure.

            Propagate_Aggregate_Library (View, Parent.Id);
         end if;

         --  At this stage the view is complete. Update mappings
         --  (i.e effective view for extends and extends all) and DAG to
         --  order the views.

         declare
            use IDS;
            Unique_ID   : constant View_Id := View.Id;
         begin
            pragma Assert (Is_Defined (Unique_ID));

            --  Finally update the DAG structure that will define the
            --  processing order for the views.

            declare
               Predecessors : GPR2.View_Ids.Set.Object;
            begin
               for Import of View.Imports loop
                  Predecessors.Include (Import.Id);
               end loop;

               View.Tree.View_DAG.Update_Vertex
                 (Vertex       => Unique_ID,
                  Predecessors => Predecessors);
            end;

            --  Add aggregate dependency

            if Status = Aggregated then
               --  inclusion of a project by an aggregate/aggregate library
               --  project

               if Parent.Is_Defined then
                  View.Tree.View_DAG.Update_Vertex
                    (Vertex      => Parent.Id,
                     Predecessor => Unique_ID);
               else
                  --  Inclusion from the root aggregate project

                  View.Tree.View_DAG.Update_Vertex
                    (Vertex      => Aggregate.Id,
                     Predecessor => Unique_ID);
               end if;
            end if;

            --  Add dependency on extended if not a "extends all"

            if View.Is_Extending then
               View.Tree.View_DAG.Update_Vertex
                  (Vertex      => Unique_ID,
                   Predecessor => View.Extended_Root.Id);
            end if;
         end;

         return View;
      end Internal;

      ----------------
      -- Is_Limited --
      ----------------

      function Is_Limited
         (View        : GPR2.Project.View.Object;
          Import_Path : Path_Name.Object) return Boolean is
      begin
         return Definition.Get_RO
            (View).Trees.Project.Imports.Element (Import_Path).Is_Limited;
      end Is_Limited;

      ----------
      -- Load --
      ----------

      function Load (Filename : Path_Name.Object) return Definition.Data is

         Paths   : constant Path_Name.Set.Object :=
                     GPR2.Project.Search_Paths (Filename, Search_Path);
         Project : constant Parser.Project.Object :=
                     Parser.Project.Parse
                       (Filename, Self.Implicit_With, Self.Messages);
         Data    : Definition.Data;
      begin
         Data.Trees.Project := Project;

         --  Record the project tree for this view

         Data.Tree := Self.Self;
         Data.Kind := K_Standard;

         --  Do the following only if there are no error messages

         if not Self.Messages.Has_Error then
            Data.Kind := Project.Qualifier;
            Data.Externals := Data.Trees.Project.Externals;

            --  Now load all imported projects if any

            for Import of Data.Trees.Project.Imports loop
               declare
                  Import_Filename : constant Path_Name.Object :=
                                      Create (Import.Path_Name.Name, Paths);
               begin
                  if Import_Filename.Exists then
                     Data.Trees.Imports.Insert
                       (Import_Filename,
                        Parser.Project.Parse
                          (Import_Filename,
                           Self.Implicit_With,
                           Self.Messages));

                  else
                     Self.Messages.Append
                       (GPR2.Message.Create
                          (Level   => (if Self.Pre_Conf_Mode
                                       then Message.Warning
                                       else Message.Error),
                           Message => "imported project file """
                                        & String (Import.Path_Name.Name)
                                        & """ not found",
                           Sloc    => Import));
                  end if;
               end;
            end loop;

            if Data.Trees.Project.Has_Extended then
               declare
                  Extended          : constant GPR2.Project.Import.Object :=
                                        Data.Trees.Project.Extended;
                  Extended_Name     : constant Filename_Type :=
                                        Extended.Path_Name.Name;
                  Extended_Filename : constant Path_Name.Object :=
                                        Create (Extended_Name, Paths);
               begin
                  if Extended_Filename.Exists then
                     Data.Trees.Extended := Parser.Project.Parse
                        (Extended_Filename, Self.Implicit_With, Self.Messages);
                  else
                     Self.Messages.Append
                       (GPR2.Message.Create
                          (Level   => (if Self.Pre_Conf_Mode
                                       then Message.Warning
                                       else Message.Error),
                           Message => "extended project file """
                                        & String (Extended_Name)
                                        & """ not found",
                           Sloc    => Data.Trees.Project.Extended));
                  end if;
               end;
            end if;
         end if;

         return Data;
      end Load;

      ---------------------------------
      -- Propagate_Aggregate_Library --
      ---------------------------------

      procedure Propagate_Aggregate_Library
        (View        : in out GPR2.Project.View.Object;
         Agg_Library : GPR2.View_Ids.View_Id)
      is
         Data : constant GPR2.Project.Definition.Ref :=
                  Definition.Get_RW (View);
         Position : GPR2.View_Ids.Set.Set.Cursor;
         Inserted : Boolean;
      begin
         Data.Agg_Libraries.Insert (Agg_Library, Position, Inserted);

         if not Inserted then
            return;
         end if;

         if Data.Extended_Root.Is_Defined then
            Propagate_Aggregate_Library (Data.Extended_Root, Agg_Library);
         end if;

         for Import of Data.Imports loop
            Propagate_Aggregate_Library (Import, Agg_Library);
         end loop;

         for Import of Data.Limited_Imports loop
            Propagate_Aggregate_Library (Import, Agg_Library);
         end loop;
      end Propagate_Aggregate_Library;

   begin
      if Starting_From.Is_Defined
        and then Starting_From.Check_Attribute (PRA.Project_Path, Result => PP)
      then
         declare
            Prepend : Boolean := False;
            Path    : Path_Name.Object;
         begin
            for P of PP.Values loop
               if P.Text = "-" then
                  Prepend := True;
               else
                  Path := Path_Name.Create_Directory
                    (Filename_Type (P.Text),
                     Filename_Type (Starting_From.Dir_Name.Value));

                  if Prepend then
                     Search_Path.Prepend (Path);
                  else
                     Search_Path.Append (Path);
                  end if;
               end if;
            end loop;
         end;
      end if;

      declare
         Aggregate : GPR2.Project.View.Object;
         Parent    : GPR2.Project.View.Object;
         Status    : Relation_Status;
         Result    : View.Object;

      begin
         if Context = GPR2.Context.Aggregate then
            --  In the closure of an aggregate project

            --  Take care of nested aggregates: only the root aggregate project
            --  defines the context.

            if Starting_From.Context = Root then
               --  Starting_From is the root aggregate project, and the view
               --  being loaded will thus define a new namespace for its
               --  subtree.

               Aggregate := Starting_From;
               Parent    := GPR2.Project.View.Undefined;
               Status    := Aggregated;

            else
               --  Nested aggregate, or inclusion from an aggregate context:
               --  we retrieve the Aggregate from the parent view, or
               --  inclusion from an aggregate context

               Parent    := Starting_From;

               if Parent.Kind in Aggregate_Kind then
                  Aggregate := Parent;
                  Status    := Aggregated;
               else
                  Aggregate := GPR2.Project.View.Undefined;
                  Status    := Simple;
               end if;
            end if;

         else
            --  Root context: no aggregate project

            Aggregate := GPR2.Project.View.Undefined;
            Parent    := Starting_From;

            --  Parent may be an aggregate library project, we need to set
            --  the status to Aggregate in this case
            if not Parent.Is_Defined then
               Status := Root;

            elsif Parent.Kind in Aggregate_Kind then
               Status := Aggregated;

            else
               Status := Simple;
            end if;
         end if;

         Result := Internal
           (Filename    => Filename,
            Aggregate   => Aggregate,
            Status      => Status,
            Parent      => Parent,
            Extends_Ctx => View.Vector.Empty_Vector);

         --  Update the DAG with the newly loaded tree.
         --
         --  ??? This is certainly not optimal, in particlar when there's
         --  a lot of aggregates, potentially nested, in the final tree, as
         --  Recursive_Load will be called for each aggregated project. However
         --  that's convenient to do it there as we have a single entry point
         --  to handle circularity issues. If performance becomes an issue, we
         --  would need to move this DAG update to the upper level to have a
         --  smarter handling of aggregated projects, so in Set_Context and
         --  Load (and of course we need to propagate the list to this upper
         --  level handling)

         declare
            Cycle       : GPR2.View_Ids.Vector.Object;
            Prev        : View.Object;
            Current     : View.Object;
            Circularity : Boolean;
         begin
            Self.View_DAG.Update (Circularity);

            if Circularity then
               Cycle := Self.View_DAG.Shortest_Circle;
               Self.Messages.Append
                 (Message.Create
                    (Message.Error, "circular dependency detected",
                     Source_Reference.Create
                       (Filename.Value, 0, 0)));

               Prev := View.Undefined;

               for Id of reverse Cycle loop
                  Current := Self.Instance_Of (Id);

                  if Prev.Is_Defined then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "depends on " &
                             String (Current.Path_Name.Value),
                           Source_Reference.Create
                             (Prev.Path_Name.Value, 0, 0)));
                  end if;

                  Prev := Current;
               end loop;
            end if;
         end;

         return Result;
      end;
   end Recursive_Load;

   ----------------------------------
   -- Register_Project_Search_Path --
   ----------------------------------

   procedure Register_Project_Search_Path
     (Self : in out Object;
      Dir  : Path_Name.Object) is
   begin
      Self.Search_Paths.Prepend (Dir);
   end Register_Project_Search_Path;

   -------------------
   -- Register_View --
   -------------------

   function Register_View
     (Def : in out Definition.Data) return Project.View.Object
   is
      View : Project.View.Object;

      procedure Add_View (Key : Value_Not_Empty);
      --  Add view to the Def.Tree.Views with the Key index

      --------------
      -- Add_View --
      --------------

      procedure Add_View (Key : Value_Not_Empty) is
         Position : View_Maps.Cursor;
         Inserted : Boolean;
      begin
         Def.Tree.Views.Insert
           (Key, Project.View.Set.Empty_Set, Position, Inserted);
         Def.Tree.Views (Position).Insert (View);
      end Add_View;

   begin
      --  Is the Id actually needed here?
      Def.Id := Natural (Def.Tree.Views_Set.Length) + 1;

      --  Populate the view with its definitions
      Definition.Set (View, Def);

      --  Ensure Views_Set and View_Ids know about it
      Def.Tree.Views_Set.Insert (View);

      pragma Assert (Definition.Refcount (View) = 2);

      Def.Tree.View_Ids.Include (Def.Unique_Id, View);

      pragma Assert (Definition.Refcount (View) = 3);

      Add_View (Path_Name.To_OS_Case (View.Path_Name.Value));

      pragma Assert (Definition.Refcount (View) = 4);

      Add_View (To_Lower (View.Name));

      pragma Assert (Definition.Refcount (View) = 5);

      return View;
   end Register_View;

   ------------------
   -- Reindex_Unit --
   ------------------

   procedure Reindex_Unit (Self : in out Object; From, To : Name_Type) is
      C : constant Name_View.Cursor := Self.Units.Find (From);
   begin
      if Name_View.Has_Element (C) then
         Self.Units.Include (To, Name_View.Element (C));
         Self.Units.Delete (From);
      end if;
   end Reindex_Unit;

   -------------------
   -- Remove_Source --
   -------------------

   procedure Remove_Source (Source : Project.Source.Object) is

      Self : constant access Object := Source.View.Tree;

      procedure Remove_If_Proper (Key : String);

      ----------------------
      -- Remove_If_Proper --
      ----------------------

      procedure Remove_If_Proper (Key : String) is
         Position : Source_Maps.Cursor := Self.Rooted_Sources.Find (Key);
      begin
         if Source_Maps.Has_Element (Position)
           and then Self.Rooted_Sources (Position).View = Source.View
         then
            Self.Rooted_Sources.Delete (Position);
         end if;
      end Remove_If_Proper;

   begin
      Remove_If_Proper (Key (Source));

      if Source.Has_Units then
         for U of Source.Units loop
            Remove_If_Proper (Key (Source.View, U));
         end loop;
      end if;
   end Remove_Source;

   ------------------
   -- Root_Project --
   ------------------

   function Root_Project (Self : Object) return View.Object is
   begin
      return Self.Root;
   end Root_Project;

   -------------
   -- Runtime --
   -------------

   function Runtime
     (Self : Object; Language : Language_Id) return Optional_Name_Type
   is
      TA : Attribute.Object;

   begin
      if Self.Has_Configuration
        and then Self.Conf.Runtime (Language) /= No_Name
      then
         return Self.Conf.Runtime (Language);

      elsif Self.Root.Is_Defined
        and then Self.Root.Check_Attribute
          (PRA.Runtime,
           Index => GPR2.Project.Attribute_Index.Create (Language),
           Check_Extended => True,
           Result         => TA)
      then
         return Optional_Name_Type (TA.Value.Text);

      else
         return No_Name;
      end if;
   end Runtime;

   ---------------------
   -- Runtime_Project --
   ---------------------

   function Runtime_Project (Self : Object) return View.Object is
   begin
      return Self.Runtime;
   end Runtime_Project;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null)
   is
      Root        : constant Definition.Ref := Definition.Get_RW (Self.Root);
      Src_Subdirs : constant String         := To_String (Self.Src_Subdirs);
   begin
      --  Register the root context for this project tree

      Self.Context (GPR2.Context.Root) := Context;

      --  Take missing external values from environment

      Update_Context (Self.Context (GPR2.Context.Root), Root.Externals);

      Set_Context (Self, Changed);

      for V of Self.Views_Set loop
         Definition.Set_Default_Attributes (Definition.Get (V).all);

         if Src_Subdirs /= ""
           and then V.Kind not in K_Configuration | K_Abstract
           and then V /= Self.Runtime
         then
            declare
               Def : constant Definition.Ref := Definition.Get (V);
               CS  : constant Attribute.Set.Cursor :=
                       Def.Attrs.Find (PRA.Source_Dirs);
               SD  : constant Attribute.Object    := Def.Attrs (CS);
               SV  : Containers.Source_Value_List := SD.Values;
            begin
               SV.Prepend
                 (Source_Reference.Value.Object
                    (Source_Reference.Value.Create
                       (Source_Reference.Object (SD),
                        V.Source_Subdirectory.Value)));
               Def.Attrs (CS) := Attribute.Create (SD.Name, SV);
            end;
         end if;
      end loop;
   end Set_Context;

   procedure Set_Context
     (Self    : in out Object;
      Changed : access procedure (Project : View.Object) := null)
   is

      procedure Set_View (View : Project.View.Object);
      --  Set the context for the given view

      procedure Validity_Check (View : Project.View.Object);
      --  Do validity check on the given view

      function Has_Error return Boolean is
        (Self.Messages.Has_Error);

      --------------
      -- Set_View --
      --------------

      procedure Set_View (View : Project.View.Object) is
         use type GPR2.Context.Binary_Signature;

         P_Data        : constant Definition.Ref := Definition.Get (View);
         Old_Signature : constant GPR2.Context.Binary_Signature :=
                           P_Data.Signature;
         New_Signature : GPR2.Context.Binary_Signature;
         Paths         : Path_Name.Set.Object;
         Tmp_Attr      : Project.Attribute.Object;

         function Get_Matching_Files
           (Projects : Source_Reference.Value.Object)
            return Path_Name.Set.Object;
         --  Return all gpr files matching Source_Reference text

         ------------------------
         -- Get_Matching_Files --
         ------------------------

         function Get_Matching_Files
           (Projects : Source_Reference.Value.Object)
            return Path_Name.Set.Object
         is

            Files           : GPR2.Path_Name.Set.Object;
            --  matching files

            procedure Get_Files;

            procedure Log (Level : GPR2.Message.Level_Value; Msg : String);

            ---------------
            -- Get_Files --
            ---------------

            procedure Get_Files is

               View_Dir        : constant GPR2.Path_Name.Object :=
                                   Path_Name.Create_Directory
                                     (Filename_Optional
                                        (View.Path_Name.Dir_Name));
               --  View root directory

               Pattern         : constant GPR2.Path_Name.Object :=
                                   (if OS_Lib.Is_Absolute_Path (Projects.Text)
                                    then Path_Name.Create_File
                                      (Filename_Optional (Projects.Text),
                                       Path_Name.No_Resolution)
                                    else View_Dir.Compose
                                      (Filename_Optional (Projects.Text)));
               --  The absolute path pattern to get matching files

               Dir_Part        : constant Filename_Optional :=
                                   Filename_Optional (Pattern.Relative_Path
                                                      (View_Dir).Value);
               --  The dir part without the trailing directory separator

               Filename_Part   : constant Filename_Optional :=
                                   Filename_Optional (Pattern.Simple_Name);
               --  The filename pattern of matching files

               Filename        : constant Filename_Optional :=
                                   (if Ada.Strings.Fixed.Index
                                      (String (Filename_Part),
                                       Wildcards,
                                       Going => Ada.Strings.Backward) = 0
                                    then Filename_Part
                                    else "");
               --  "" if Filename part is a regular expression otherwise the
               --  filename to locate.

               Filename_Regexp : constant GNAT.Regexp.Regexp :=
                                   GPR2.Compile_Regexp (Filename_Part);
               --  regexp pattern for matching Filename

               procedure Handle_File
                 (File : GPR2.Path_Name.Object);
               procedure Is_Directory_Handled
                 (Directory       : GPR2.Path_Name.Object;
                  Is_Root_Dir     : Boolean;
                  Do_Dir_Visit    : in out Boolean;
                  Do_Subdir_Visit : in out Boolean);

               -----------------
               -- Handle_File --
               -----------------

               procedure Handle_File
                 (File : GPR2.Path_Name.Object) is
               begin
                  if GNAT.Regexp.Match (String (File.Simple_Name),
                                        Filename_Regexp)
                    and then not Files.Contains (File)
                  then
                     Files.Append (File);
                  end if;
               end Handle_File;

               --------------------------
               -- Is_Directory_Handled --
               --------------------------

               procedure Is_Directory_Handled
                 (Directory : GPR2.Path_Name.Object;
                  Is_Root_Dir     : Boolean;
                  Do_Dir_Visit    : in out Boolean;
                  Do_Subdir_Visit : in out Boolean) is
                  pragma Unreferenced (Is_Root_Dir, Do_Subdir_Visit);
               begin
                  if Filename /= "" then
                     Do_Dir_Visit := False;
                     declare
                        File : constant GPR2.Path_Name.Object :=
                                 Directory.Compose (Filename);
                        use Ada.Directories;
                     begin
                        if File.Exists
                          and then not Files.Contains (File)
                          and then Kind (File.Value) /= Directories.Directory
                        then
                           Files.Append (File);
                        end if;
                     end;
                  end if;
               end Is_Directory_Handled;

            begin
               View.Foreach
                 (Directory_Pattern => Dir_Part,
                  Source            => Projects,
                  File_CB           => Handle_File'Access,
                  Directory_CB      => Is_Directory_Handled'Access);
            end Get_Files;

            ---------
            -- Log --
            ---------

            procedure Log (Level : GPR2.Message.Level_Value; Msg : String) is
            begin
               Self.Append_Message (Message.Create (Level, Msg, Projects));
            end Log;

         begin
            if Projects.Text /= "" then
               Get_Files;
            end if;

            if Files.Is_Empty then
               Log (Message.Error, "file """ & Projects.Text & """ not found");
            end if;

            return Files;
         exception
            when Ada.IO_Exceptions.Name_Error =>
               Log (Message.Error,
                    Projects.Text & " contains an invalid directory");
               return Files;
         end Get_Matching_Files;

         function Is_Implicitly_Abstract
           (View : Project.View.Object) return Boolean;
         --  Returns True if project can be recognised as abstract project.
         --  I.e. not inherited from non abstract project and has empty one of
         --  source list defining attributes.

         ----------------------------
         -- Is_Implicitly_Abstract --
         ----------------------------

         function Is_Implicitly_Abstract
           (View : Project.View.Object) return Boolean
         is

            function Is_Defined_Empty (Attr : Attribute_Id) return Boolean;
            --  Returns True if attribute defined as empty list in view

            ----------------------
            -- Is_Defined_Empty --
            ----------------------

            function Is_Defined_Empty (Attr : Attribute_Id) return Boolean is
            begin
               return View.Check_Attribute (Attr, Result => Tmp_Attr)
                 and then Tmp_Attr.Values.Is_Empty;
            end Is_Defined_Empty;

         begin
            if View.Is_Abstract then
               return True;
            end if;

            if (View.Is_Extending
                and then not Is_Implicitly_Abstract (View.Extended_Root))
              or else View.Is_Externally_Built
            then
               --  Project extending non abstract one is not abstract

               return False;
            end if;

            --  We need at least Source_Dirs, Source_Files, or Languages
            --  explicitly empty.

            return Is_Defined_Empty (PRA.Source_Dirs)
              or else Is_Defined_Empty (PRA.Source_Files)
              or else Is_Defined_Empty (PRA.Languages);
         end Is_Implicitly_Abstract;

      begin
         Parser.Project.Process
           (P_Data.Trees.Project,
            Self,
            View.Context,
            View,
            P_Data.Attrs,
            P_Data.Vars,
            P_Data.Packs,
            P_Data.Types,
            Self.Pre_Conf_Mode);

         if View.Qualifier not in Aggregate_Kind then
            New_Signature := View.Context.Signature (P_Data.Externals);

         elsif not P_Data.Attrs.Contains (PRA.Project_Files) then
            --  Aggregate project must have Project_Files attribute

            Self.Error
              ("Attribute ""project_files"" must be specified in"
               & " aggregate project",
               Source_Reference.Create (View.Path_Name.Value, 0, 0));

         else
            --  If an aggregate project and an attribute external is defined
            --  then remove the dependency on the corresponding externals.

            if P_Data.Is_Root then
               for C in P_Data.Attrs.Iterate (Name => PRA.External) loop
                  declare
                     P : Containers.Name_Type_List.Cursor :=
                           P_Data.Externals.Find
                             (Name_Type (P_Data.Attrs (C).Index.Text));
                  begin
                     if Containers.Name_Type_List.Has_Element (P) then
                        P_Data.Externals.Delete (P);
                     end if;
                  end;
               end loop;
            end if;

            if P_Data.Is_Root then
               --  This is the root aggregate project which defines the context
               --  for all aggregated projects.

               --  Starts from the root context

               Self.Context (Aggregate) := Self.Context (GPR2.Context.Root);

               --  And then adjust context based on External attribute values
               --  inside the root aggregate project.

               for C in P_Data.Attrs.Iterate (PRA.External) loop
                  declare
                     use all type PRA.Value_Kind;

                     External : constant Attribute.Object := P_Data.Attrs (C);
                     Position : GPR2.Context.Key_Value.Cursor;
                     Inserted : Boolean;
                  begin
                     --  Check for the validity of the external attribute here
                     --  as the validity check will come after it is fully
                     --  loaded/resolved.

                     if External.Kind = Single then
                        Self.Context (Aggregate).Insert
                          (Name_Type (External.Index.Text),
                           External.Value.Text,
                           Position, Inserted);
                     end if;
                  end;
               end loop;
            end if;

            --  Now we can record the aggregated projects based on the possibly
            --  new Project_Files attribute value. This attribute may be set
            --  depending on the parsing of the imported projects.

            P_Data.Aggregated.Clear;

            --  Pathname for Project_Files projects are relative to the
            --  aggregate project only.

            Paths.Append (View.Path_Name);

            for Project of P_Data.Attrs.Element (PRA.Project_Files).Values loop
               declare
                  Found : Boolean := False;
               begin
                  for Pathname of Get_Matching_Files (Project) loop
                     if Pathname = View.Path_Name then
                        --  We are loading recursively the aggregate project
                        --  As in GPR1 ignore it.

                        Found := True;

                     elsif P_Data.Aggregated.Contains
                       (Name_Type (Pathname.Value))
                     then
                        --  Duplicate in the project_files attribute

                        Self.Messages.Append
                          (Message.Create
                             (Message.Warning,
                              "duplicate aggregated project "
                              & String (Pathname.Base_Name),
                              Project));
                        Found := True;

                     elsif Pathname.Exists then
                        declare
                           Context : constant Context_Kind :=
                                       (if View.Kind = K_Aggregate_Library
                                        then Definition.Get_RO
                                          (View).Context
                                        else Aggregate);
                           A_View  : constant GPR2.Project.View.Object :=
                                       Recursive_Load
                                         (Self          => Self,
                                          Filename      => Pathname,
                                          Context       => Context,
                                          Starting_From => View);
                        begin
                           --  If there was error messages during the parsing
                           --  of the aggregated project, just exit now.

                           if Self.Messages.Has_Error then
                              raise Project_Error
                                with Pathname.Value;
                           end if;

                           --  Record aggregated view into the aggregate's view

                           P_Data.Aggregated.Insert
                             (Name_Type (Pathname.Value), A_View);
                        end;

                        Found := True;
                     end if;
                  end loop;

                  if not Found then
                     Self.Error
                       ("file """ & Project.Text & """ not found", Project);
                  end if;
               end;
            end loop;

            New_Signature := View.Context.Signature (P_Data.Externals);
         end if;

         if not Has_Error
           and then P_Data.Kind not in K_Abstract | K_Configuration
         then
            P_Data.Signature := New_Signature;

            --  Let's compute the project kind if needed. A project without an
            --  explicit qualifier may actually be an abstract project or a
            --  library project.

            P_Data.Kind := P_Data.Trees.Project.Qualifier;

            if P_Data.Kind = K_Standard then
               if Is_Implicitly_Abstract (View) then
                  if P_Data.Trees.Project.Explicit_Qualifier then
                     --  Error message depend on Tmp_Attr because this
                     --  attribute was used to detect that project has no
                     --  sources.

                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "a standard project must have "
                           & (if Tmp_Attr.Name.Id = PRA.Source_Dirs
                              then "source directories"
                              elsif Tmp_Attr.Name.Id = PRA.Languages
                              then "languages"
                              else "sources"),
                           Tmp_Attr));
                  else
                     P_Data.Kind := K_Abstract;
                  end if;

               elsif View.Check_Attribute
                       (PRA.Library_Name,
                        Check_Extended => True,
                        Result         => Tmp_Attr)
                 and then Tmp_Attr.Value.Text /= ""
                 and then View.Check_Attribute
                            (PRA.Library_Dir, Result => Tmp_Attr)
                 and then Tmp_Attr.Value.Text /= ""
               then
                  --  If Library_Name, Library_Dir are declared, then the
                  --  project is a library project.
                  --  Note: Library_Name may be inherited from an extended
                  --  project while Library_Dir has to be defined in the
                  --  project.

                  if P_Data.Trees.Project.Explicit_Qualifier then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "a standard project cannot be a library project",
                           Tmp_Attr));
                  else
                     P_Data.Kind := K_Library;
                  end if;

               elsif View.Is_Extending and then View.Extended_Root.Is_Library
                 and then P_Data.Trees.Project.Explicit_Qualifier
               then
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "a standard project cannot extend a library project",
                        P_Data.Trees.Project.Extended));
               end if;
            end if;

            --  Signal project change if we have different signature.
            --  That is if there is at least some external used otherwise the
            --  project is stable and won't change.

            if Old_Signature /= New_Signature then
               if Changed /= null then
                  Changed (View);
               end if;

               Self.Invalidate_Sources (View);
            end if;
         end if;
      end Set_View;

      --------------------
      -- Validity_Check --
      --------------------

      procedure Validity_Check (View : Project.View.Object) is
         use type PRA.Index_Kind;
         use type PRA.Value_Kind;

         Check_Object_Dir_Exists : Boolean := True;
         Check_Exec_Dir_Exists   : Boolean := True;
         --  To avoid error on check Object_Dir and Exec_Dir existence when
         --  attribute is not correct.

         procedure Check_Def (Def : PRA.Def; A : Attribute.Object);
         --  Check if attribute definition is valid, record errors into the
         --  message log facility.

         ---------------
         -- Check_Def --
         ---------------

         procedure Check_Def (Def : PRA.Def; A : Attribute.Object) is
         begin
            if Def.Index = PRA.No and then A.Has_Index then
               Self.Error
                  ("attribute """ & Image (A.Name.Id)
                   & """ cannot have index", A);
            end if;

            if Def.Value = PRA.Single and then A.Kind = PRA.List then
               Self.Error
                 ("attribute """ & Image (A.Name.Id)
                  & """ cannot be a list",
                  A);

               if A.Name.Id = PRA.Object_Dir then
                  Check_Object_Dir_Exists := False;
               elsif A.Name.Id = PRA.Exec_Dir then
                  Check_Exec_Dir_Exists := False;
               end if;
            end if;

            if Def.Value = PRA.List and then A.Kind = PRA.Single then
               Self.Error
                 ("attribute """ & Image (A.Name.Id)
                  & """ must be a list",
                  A.Value);
            end if;
         end Check_Def;

         P_Kind : constant Project_Kind := View.Kind;
         P_Data : constant Definition.Const_Ref := Definition.Get_RO (View);

      begin
         --  Check packages

         for P of P_Data.Packs loop
            if Registry.Pack.Exists (P.Name) then
               --  Check the package itself

               if not Registry.Pack.Is_Allowed_In (P.Name, P_Kind) then
                  Self.Warning
                    ("package """ & Image (P.Name)
                     & """ cannot be used in " & Image (P_Kind) & 's',
                     P);
               end if;

               --  Check package's attributes

               for A of P.Attributes loop
                  declare
                     Q_Name : constant PRA.Qualified_Name :=
                                PRA.Create (A.Name.Id, P.Name);
                     Def    : PRA.Def;
                  begin
                     if PRA.Exists (Q_Name) then
                        Def := PRA.Get (Q_Name);

                        if not Def.Is_Allowed_In (P_Kind) then
                           Self.Warning
                             ("attribute """ & PRA.Image (Q_Name)
                              & """ cannot be used in " & Image (P_Kind),
                              A);
                        end if;

                        Check_Def (Def, A);

                     elsif PRP.Attributes_Are_Checked (P.Name) then
                        Self.Warning
                          ("attribute """ & Image (A.Name.Id)
                           & """ not supported in package "
                           & Image (P.Name),
                           A);
                     end if;
                  end;
               end loop;
            end if;
         end loop;

         --  Check top level attributes

         for A of P_Data.Attrs loop
            declare
               Q_Name : constant PRA.Qualified_Name :=
                          PRA.Create (A.Name.Id);
            begin
               if not PRA.Exists (Q_Name) then
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "unrecognized attribute """ & Image (A.Name.Id)
                        & '"',
                        A));

               else
                  declare
                     Allowed : constant PRA.Allowed_In :=
                                 PRA.Get (Q_Name).Is_Allowed_In;
                     Found : Natural := 0;
                     Allow : Project_Kind;
                  begin
                     if not Allowed (P_Kind) then
                        for A in Allowed'Range loop
                           if Allowed (A) then
                              Found := Found + 1;
                              exit when Found > 1;
                              Allow := A;
                           end if;
                        end loop;

                        pragma Assert (Found > 0);

                        if Found = 1 or else Allow = K_Aggregate then
                           --  If one or Aggregate_Kind allowed use including
                           --  error message.

                           Self.Warning
                             ('"' & Image (A.Name.Id)
                              & """ is only valid in "
                              & Image (Allow) & 's',
                              A);
                        else
                           --  If more than one is allowed use excluding
                           --  error message.

                           Self.Warning
                             ("attribute """ & Image (A.Name.Id)
                              & """ cannot be used in "
                              & Image (P_Kind) & 's',
                              A);
                        end if;
                     end if;
                  end;

                  Check_Def (PRA.Get (Q_Name), A);
               end if;
            end;
         end loop;

         --  Check Library_Version attribute format

         declare
            procedure Check_Directory
              (Attr_Name     : Attribute_Id;
               Human_Name    : String;
               Get_Directory : not null access function
                 (Self : Project.View.Object) return Path_Name.Object;
               Mandatory     : Boolean := False;
               Must_Exist    : Boolean := True);
            --  Check is directory exists and warn if there is try to relocate
            --  absolute path with --relocate-build-tree gpr tool command line
            --  parameter. Similar check for attributes with directory names.
            --
            --  Mandatory: when set, check that the attribute is defined.
            --  Must_Exist: when set, check that the directory exists on the
            --    filesystem.

            Attr : Attribute.Object;

            ---------------------
            -- Check_Directory --
            ---------------------

            procedure Check_Directory
              (Attr_Name     : Attribute_Id;
               Human_Name    : String;
               Get_Directory : not null access function
                 (Self : Project.View.Object) return Path_Name.Object;
               Mandatory     : Boolean := False;
               Must_Exist    : Boolean := True) is
            begin
               if View.Check_Attribute (Attr_Name, Result => Attr) then
                  declare
                     AV : constant Source_Reference.Value.Object := Attr.Value;
                     PN : constant Path_Name.Object := Get_Directory (View);
                  begin
                     if Must_Exist
                       and then not PN.Exists
                     then
                        Self.Messages.Append
                          (Message.Create
                             ((if Self.Absent_Dir_Error
                              then Message.Error
                              else Message.Warning),
                              (if Human_Name = ""
                               then "D"
                               else Human_Name & " d") & "irectory """
                              & AV.Text & """ not found",
                              Sloc => AV));

                     elsif Self.Build_Path.Is_Defined
                       and then OS_Lib.Is_Absolute_Path (AV.Text)
                     then
                        Self.Messages.Append
                          (Message.Create
                             (Message.Warning,
                                  '"'
                              & PN.Relative_Path (Self.Root.Path_Name).Value
                              & """ cannot relocate absolute "
                              & (if Human_Name = ""
                                then ""
                                else Human_Name & ' ')
                              & "directory",
                              Sloc => AV));
                     end if;
                  end;

               elsif Mandatory then
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "attribute " & Image (Attr_Name) & " not declared",
                        Source_Reference.Create (View.Path_Name.Value, 0, 0)));
               end if;
            end Check_Directory;

         begin
            if View.Kind in K_Standard | K_Library | K_Aggregate_Library
              and then Check_Object_Dir_Exists
            then
               Check_Directory
                 (PRA.Object_Dir, "object",
                  Project.View.Object_Directory'Access,
                  Must_Exist => not View.Is_Aggregated_In_Library
                                  and then not View.Is_Extended);
            end if;

            if View.Is_Library then
               Check_Directory
                 (PRA.Library_Dir, "library",
                  Project.View.Library_Directory'Access,
                  Mandatory  => True,
                  Must_Exist => not View.Is_Aggregated_In_Library
                                  and then not View.Is_Extended);

               Check_Directory
                 (PRA.Library_Ali_Dir, "library ALI",
                  Project.View.Library_Ali_Directory'Access,
                  Must_Exist => not View.Is_Aggregated_In_Library
                                  and then not View.Is_Extended);

               if View.Has_Library_Interface
                 or else View.Has_Attributes (PRA.Interfaces)
               then
                  Check_Directory
                    (PRA.Library_Src_Dir, "",
                     Project.View.Library_Src_Directory'Access);
               end if;

               if not View.Check_Attribute
                        (PRA.Library_Name,
                         Check_Extended => True, Result => Attr)
               then
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "attribute Library_Name not declared",
                        Source_Reference.Create (View.Path_Name.Value, 0, 0)));
               end if;
            end if;

            case View.Kind is
               when K_Standard =>
                  if Check_Exec_Dir_Exists then
                     Check_Directory
                       (PRA.Exec_Dir, "exec",
                        Project.View.Executable_Directory'Access);
                  end if;

               when Aggregate_Kind =>
                  for Agg of View.Aggregated loop
                     if Agg.Is_Externally_Built then
                        Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "cannot aggregate externally built project """
                              & String (Agg.Name) & '"',
                              Sloc => View.Attribute_Location
                                        (PRA.Project_Files)));
                     end if;
                  end loop;

                  --  aggregate library project can have regular imports,
                  --  while aggregate projects can't.

                  if View.Kind = K_Aggregate and then View.Has_Imports then
                     for Imported of View.Imports loop
                        if not Imported.Is_Abstract then
                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "can only import abstract projects, not """
                                 & String (Imported.Name) & '"',
                                 Sloc => View.Attribute_Location
                                           (PRA.Project_Files)));
                        end if;
                     end loop;
                  end if;

               when K_Abstract =>
                  declare
                     A1 : Attribute.Object;
                     H1 : constant Boolean :=
                            View.Check_Attribute
                              (PRA.Source_Dirs, Result => A1);
                     A2 : Attribute.Object;
                     H2 : constant Boolean :=
                            View.Check_Attribute
                              (PRA.Source_Files, Result => A2);
                     A3 : Attribute.Object;
                     H3 : constant Boolean :=
                            View.Check_Attribute
                              (PRA.Languages, Result => A3);
                  begin
                     if (H1 or else H2 or else H3)
                       and then not (H1 and then A1.Values.Is_Empty)
                       and then not (H2 and then A2.Values.Is_Empty)
                       and then not (H3 and then A3.Values.Is_Empty)
                     then
                        Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "non-empty set of sources can't be defined in an"
                              & " abstract project",
                              Source_Reference.Create
                                (View.Path_Name.Value, 0, 0)));
                     end if;
                  end;

               when others =>
                  null;
            end case;
         end;
      end Validity_Check;

   begin
      --  Now the first step is to set the configuration project view if any
      --  and to create the runtime project if possible.

      if Self.Has_Configuration then
         Set_View (Self.Conf.Corresponding_View);

         Self.Runtime := Create_Runtime_View (Self);
      end if;

      declare
         Closure_Found : Boolean := True;
         Closure       : GPR2.View_Ids.Set.Object;
         Position      : GPR2.View_Ids.Set.Set.Cursor;
         Inserted      : Boolean;
      begin
         --  First do a pass on the subtree that starts from root of
         --  projects not part of any aggregates. In case there is an
         --  aggregate, the root project will be an aggregate and after
         --  processing that subtree we are sure that aggregate context is
         --  set correctly.

         for View of Self.Ordered_Views loop
            if not View.Has_Aggregate_Context then
               Set_View (View);
               Closure.Insert (View.Id);
            end if;
         end loop;

         --  Now evaluate the remaining views

         loop
            for View of Self.Ordered_Views loop
               Closure.Insert (View.Id, Position, Inserted);

               if Inserted then
                  Closure_Found := False;
                  Set_View (View);
               end if;
            end loop;

            exit when Closure_Found;
            Closure_Found := True;
         end loop;
      end;

      if not Has_Error and then not Self.Pre_Conf_Mode then
         --  We now have an up-to-date tree, do some validity checks if there
         --  is no issue detected yet.

         for View of Self.Ordered_Views loop
            Validity_Check (View);
         end loop;
      end if;

      if Has_Error and then not Self.Pre_Conf_Mode then
         raise Project_Error
           with Self.Root.Path_Name.Value & " semantic error";
      end if;
   end Set_Context;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source (Source : Project.Source.Object) is

      Self : constant access Object := Source.View.Tree;

      procedure Insert_Or_Replace (Key : String);
      --  Insert or replace the source into Rooted_Sources container with given
      --  Key.

      -----------------------
      -- Insert_Or_Replace --
      -----------------------

      procedure Insert_Or_Replace (Key : String) is
         Position : Source_Maps.Cursor;
         Inserted : Boolean;
      begin
         Self.Rooted_Sources.Insert (Key, Source, Position, Inserted);

         if not Inserted
           and then Source.View.Is_Extending
                      (Source_Maps.Element (Position).View)
         then
            Self.Rooted_Sources.Replace_Element (Position, Source);
         end if;
      end Insert_Or_Replace;

   begin
      Insert_Or_Replace (Key (Source));

      if Source.Has_Units then
         for U of Source.Units loop
            Insert_Or_Replace (Key (Source.View, U));
         end loop;
      end if;
   end Set_Source;

   ------------
   -- Target --
   ------------

   function Target
     (Self : Object; Canonical : Boolean := False) return Name_Type
   is

      function Normalized (Target : Name_Type) return Name_Type
        with Inline;

      ----------------
      -- Normalized --
      ----------------

      function Normalized (Target : Name_Type) return Name_Type is
      begin
         if Self.Base.Is_Defined then
            declare
               Ret : constant Name_Type :=
                       Self.Base.Normalized_Target (Target);
            begin
               if Ret /= "unknown" then
                  return Ret;
               end if;
            end;
         end if;

         return Target;
      end Normalized;

      TA : Attribute.Object;

   begin
      if Self.Has_Configuration
        and then Self.Configuration.Corresponding_View.Check_Attribute
          ((if Canonical then PRA.Canonical_Target else PRA.Target),
           Result => TA)
      then
         return Name_Type (TA.Value.Text);

      elsif Self.Root.Is_Defined
        and then Self.Root.Check_Attribute
                   (PRA.Target, Check_Extended => True, Result => TA)
      then
         return Name_Type (TA.Value.Text);

      elsif Self.Base.Is_Defined then
         return Normalized (Target_Name);

      else
         --  Target name as specified during the build
         return Target_Name;
      end if;
   end Target;

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Object) is
   begin
      Self.Self             := Undefined.Self;
      Self.Root             := Undefined.Root;
      Self.Conf             := Undefined.Conf;
      Self.Runtime          := Undefined.Runtime;
      Self.Build_Path       := Undefined.Build_Path;
      Self.Subdirs          := Undefined.Subdirs;
      Self.Src_Subdirs      := Undefined.Src_Subdirs;
      Self.Check_Shared_Lib := Undefined.Check_Shared_Lib;
      Self.Search_Paths     := Undefined.Search_Paths;

      Self.Units.Clear;
      Self.Sources.Clear;
      Self.Rooted_Sources.Clear;
      Self.Messages.Clear;
      Self.Views.Clear;
      Self.View_Ids.Clear;
      Self.View_DAG.Clear;
      Self.Views_Set.Clear;
   end Unload;

   --------------------
   -- Update_Context --
   --------------------

   procedure Update_Context
     (Context   : in out GPR2.Context.Object;
      Externals : Containers.Name_List) is
   begin
      for External of Externals loop
         declare
            External_Value : constant String :=
                               Environment_Variables.Value
                                 (String (External), "");
            Position : GPR2.Context.Key_Value.Cursor;
            Inserted : Boolean;
         begin
            if External_Value /= "" then
               --  The external is not present in the current context. Try to
               --  fetch its value from the environment and insert it in the
               --  context.

               Context.Insert
                 (External, External_Value, Position, Inserted);
            end if;
         end;
      end loop;
   end Update_Context;

   --------------------------------------------
   -- Update_Project_Search_Path_From_Config --
   --------------------------------------------

   procedure Update_Project_Search_Path_From_Config
     (Self : in out Object;
      Conf : Project.Configuration.Object)
   is
      use OS_Lib;
      Compiler     : Project.Pack.Object;
      Drivers      : Attribute.Set.Object;

      PATH         : constant String := Environment_Variables.Value ("PATH");
      PATH_Subs    : String_Split.Slice_Set;

      Given_Target : Project.Attribute.Object;
      Canon_Target : Project.Attribute.Object;
      Canon_Set    : Boolean;
      Given_Set    : Boolean;
      Unique       : Containers.Filename_Set;

      function Is_Bin_Path (T_Path : String) return Boolean;

      procedure Append (Dir : String);

      ------------
      -- Append --
      ------------

      procedure Append (Dir : String) is
         Position : Containers.Filename_Type_Set.Cursor;
         Inserted : Boolean;
      begin
         Unique.Insert (Filename_Type (Dir), Position, Inserted);

         if Inserted then
            Self.Search_Paths.Append
              (Path_Name.Create_Directory (Filename_Type (Dir)));
         end if;
      end Append;

      -----------------
      -- Is_Bin_Path --
      -----------------

      function Is_Bin_Path (T_Path : String) return Boolean is
      begin
         return Directories.Simple_Name (T_Path) = "bin";
      exception
         when Ada.IO_Exceptions.Name_Error =>
            return False;
      end Is_Bin_Path;

   begin
      if Conf.Corresponding_View.Has_Packages (Registry.Pack.Compiler) then
         Compiler :=
           Conf.Corresponding_View.Packages.Element (Registry.Pack.Compiler);
      else
         return;
      end if;

      Canon_Set := Conf.Corresponding_View.Check_Attribute
                     (PRA.Canonical_Target, Result => Canon_Target);

      Given_Set := Conf.Corresponding_View.Check_Attribute
                     (PRA.Target, Result => Given_Target);

      String_Split.Create
        (PATH_Subs,
         PATH,
         (1 => Path_Separator),
         String_Split.Multiple);

      Drivers := Compiler.Attributes (Registry.Attribute.Driver);

      --  We need to arrange toolchains in the order of appearance on PATH

      for Sub of PATH_Subs loop
         if Is_Bin_Path (Sub) then
            for Driver of Drivers loop
               if Driver.Value.Text /= "" then
                  declare
                     Driver_Dir    : constant String :=
                                       Normalize_Pathname
                                         (Directories.Containing_Directory
                                            (String (Driver.Value.Text)),
                                          Case_Sensitive => False);
                     Toolchain_Dir : constant String :=
                                       Directories.Containing_Directory
                                         (Driver_Dir);
                     Index         : constant Language_Id :=
                                       +Name_Type (Driver.Index.Value);
                  begin
                     if Driver_Dir =
                       Normalize_Pathname (Sub, Case_Sensitive => False)
                     then
                        if Given_Set then
                           --  We only care for runtime if it is a simple
                           --  name. Runtime specific names go with explicitly
                           --  specified target (if it has been specifed).

                           if Conf.Runtime (Index) /= No_Name
                             and then not
                               (for some C of Conf.Runtime (Index) =>
                                      C in '/' | '\')
                           then
                              Append
                                (Toolchain_Dir
                                 & Directory_Separator
                                 & (String (Given_Target.Value.Text))
                                 & Directory_Separator
                                 & String (Conf.Runtime (Index))
                                 & Directory_Separator
                                 & "share"
                                 & Directory_Separator
                                 & "gpr");
                              Append
                                (Toolchain_Dir
                                 & Directory_Separator
                                 & (String (Given_Target.Value.Text))
                                 & Directory_Separator
                                 & String (Conf.Runtime (Index))
                                 & Directory_Separator
                                 & "lib"
                                 & Directory_Separator
                                 & "gnat");
                           end if;

                           --  Explicitly specified target may be not in
                           --  canonical form.

                           Append
                             (Toolchain_Dir
                              & Directory_Separator
                              & (String (Given_Target.Value.Text))
                              & Directory_Separator
                              & "share"
                              & Directory_Separator
                              & "gpr");
                           Append
                             (Toolchain_Dir
                              & Directory_Separator
                              & (String (Given_Target.Value.Text))
                              & Directory_Separator
                              & "lib"
                              & Directory_Separator
                              & "gnat");
                        end if;

                        if Canon_Set then
                           --  Old cgpr files can miss Canonical_Target

                           Append
                             (Toolchain_Dir
                              & Directory_Separator
                              & (String (Canon_Target.Value.Text))
                              & Directory_Separator
                              & "share"
                              & Directory_Separator
                              & "gpr");
                           Append
                             (Toolchain_Dir
                              & Directory_Separator
                              & (String (Canon_Target.Value.Text))
                              & Directory_Separator
                              & "lib"
                              & Directory_Separator
                              & "gnat");
                        end if;

                        Append
                          (Toolchain_Dir
                           & Directory_Separator
                           & "share"
                           & Directory_Separator
                           & "gpr");
                        Append
                          (Toolchain_Dir
                           & Directory_Separator
                           & "lib"
                           & Directory_Separator
                           & "gnat");
                     end if;
                  end;
               end if;
            end loop;
         end if;
      end loop;
   end Update_Project_Search_Path_From_Config;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources
     (Self          : Object;
      Stop_On_Error : Boolean := True;
      With_Runtime  : Boolean := False;
      Backends      : Source_Info.Backend_Set := Source_Info.All_Backends)
   is
      use type Source_Info.Backend_Set;
      Internal : constant Boolean := Backends = Source_Info.No_Backends;
      Has_RT   : Boolean := False; -- True if Tree has a runtime view
   begin
      if not Internal then
         Self.Self.Rooted_Sources.Clear;
         Self.Self.Sources_Loaded := True;
      end if;

      for V of reverse Self.Ordered_Views loop
         if V.Is_Runtime then
            Has_RT := True;
         end if;

         if With_Runtime or else not V.Is_Runtime then
            Definition.Get (V).Update_Sources (V, Stop_On_Error, Backends);
         end if;
      end loop;

      if With_Runtime and then not Has_RT and then Self.Runtime.Is_Defined then
         --  Runtime update sources is required, but runtime project is not in
         --  the Self.Ordered_Views.

         Definition.Get (Self.Runtime).Update_Sources
           (Self.Runtime, Stop_On_Error, Backends);
      end if;

      if not Internal and then Self.Check_Shared_Lib then
         for View of Self.Views_Set loop
            declare
               procedure Check_Shared_Lib (PV : Project.View.Object);
               --  Check that shared library project does not have in imports
               --  static library or standard projects.

               ----------------------
               -- Check_Shared_Lib --
               ----------------------

               procedure Check_Shared_Lib (PV : Project.View.Object) is
                  P_Data : constant Definition.Const_Ref :=
                             Definition.Get_RO (PV);

                  function Has_Essential_Sources
                    (V : Project.View.Object) return Boolean;
                  --  Returns True if V has Ada sources or non ada bodies

                  function Source_Loc
                    (Imp : Project.View.Object)
                     return Source_Reference.Object'Class;
                  --  Returns a source location for the import.
                  --
                  --  Can't rely on P_Data.Trees.Project.Imports as
                  --  in case of extended projects the import may be
                  --  implicit, so retrieval of the source location is not
                  --  easy.

                  ---------------------------
                  -- Has_Essential_Sources --
                  ---------------------------

                  function Has_Essential_Sources
                    (V : Project.View.Object) return Boolean is
                  begin
                     for S of V.Sources loop
                        if S.Language = Ada_Language
                          or else S.Kind not in GPR2.Unit.Spec_Kind
                        then
                           return True;
                        end if;
                     end loop;

                     return False;
                  end Has_Essential_Sources;

                  function Source_Loc
                    (Imp : Project.View.Object)
                     return Source_Reference.Object'Class
                  is
                     Imports  : constant Project.Import.Set.Object :=
                                  P_Data.Trees.Project.Imports;
                     Position : constant Project.Import.Set.Cursor :=
                                  Imports.Find (Imp.Path_Name);
                  begin
                     if Project.Import.Set.Has_Element (Position) then
                        return Project.Import.Set.Element (Position);
                     else
                        return Source_Reference.Create
                          (P_Data.Trees.Project.Path_Name.Value, 0, 0);
                     end if;
                  end Source_Loc;

               begin
                  for Imp of P_Data.Imports loop
                     if Imp.Kind = K_Abstract
                       or else not Has_Essential_Sources (Imp)
                     then
                        --  Check imports further in recursion

                        Check_Shared_Lib (Imp);

                     elsif not Imp.Is_Library then
                        Self.Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "shared library project """ & String (View.Name)
                              & """ cannot import project """
                              & String (Imp.Name)
                              & """ that is not a shared library project",
                              Source_Loc (Imp)));

                     elsif Imp.Is_Static_Library
                       and then View.Library_Standalone /= Encapsulated
                     then
                        Self.Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "shared library project """ &
                                String (View.Name)
                              & """ cannot import static library project """
                              & String (Imp.Name) & '"',
                              Source_Loc (Imp)));

                     elsif Imp.Is_Shared_Library
                       and then View.Library_Standalone = Encapsulated
                     then
                        Self.Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "encapsulated library project """
                              & String (View.Name)
                              & """ cannot import shared library project """
                              & String (Imp.Name) & '"',
                              Source_Loc (Imp)));
                     end if;
                  end loop;

                  if PV.Is_Library and then PV.Is_Shared_Library then
                     --  Also check value of liobrary_standalone if any

                     if PV.Has_Any_Interfaces
                       and then PV.Library_Standalone = No
                     then
                        Self.Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "wrong value for Library_Standalone when"
                              & " Library_Interface defined",
                              PV.Attribute_Location
                                (Registry.Attribute.Library_Standalone)));
                     end if;

                     --  And if a standalone library has interfaces

                     if not PV.Has_Any_Interfaces
                       and then PV.Library_Standalone /= No
                     then
                        Self.Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "Library_Standalone valid only if library"
                              & " has Ada interfaces",
                              PV.Attribute_Location
                                (Registry.Attribute.Library_Standalone)));
                     end if;
                  end if;
               end Check_Shared_Lib;

            begin
               if View.Is_Library then
                  if View.Is_Shared_Library then
                     Check_Shared_Lib (View);

                  elsif View.Library_Standalone = Encapsulated then
                     --  Static standalone library is not possible
                     Self.Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "encapsulated library project """ &
                             String (View.Name) &
                             """ must be a shared library project",
                           View.Attribute_Location
                             (Registry.Attribute.Library_Standalone)));
                  end if;
               end if;
            end;
         end loop;

         if Stop_On_Error and then Self.Messages.Has_Error then
            raise Project_Error;
         end if;
      end if;
   end Update_Sources;

   --------------
   -- View_For --
   --------------

   function View_For
     (Self    : Object;
      Name    : Name_Type;
      Context : Context_Kind) return View.Object
   is
      View : Project.View.Object := Self.Get (Name, Context);
   begin
      if not View.Is_Defined then
         declare
            CV : constant Project.View.Object :=
                   (if Self.Has_Configuration
                    then Self.Conf.Corresponding_View
                    else Project.View.Undefined);
         begin
            --  If not found let's check if it is the configuration or runtime
            --  project. Note that this means that any Runtime or Config user's
            --  project name will have precedence.

            if CV.Is_Defined and then CV.Name = Name then
               View := CV;

            elsif Self.Has_Runtime_Project
              and then Self.Runtime.Name = Name
            then
               View := Self.Runtime;
            end if;
         end;
      end if;

      return View;
   end View_For;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Self : in out Object;
      Msg  : String;
      Sloc : Source_Reference.Object'Class) is
   begin
      Self.Messages.Append (Message.Create (Message.Warning, Msg, Sloc));
   end Warning;

begin
   --  Export routines to Definitions to avoid cyclic dependencies

   Definition.Register           := Register_View'Access;
   Definition.Check_Source       := Check_Source'Access;
   Definition.Check_Source_Unit  := Check_Source'Access;
   Definition.Has_Source         := Has_Source'Access;
   Definition.Set_Source         := Set_Source'Access;
   Definition.Remove_Source      := Remove_Source'Access;
   Definition.Get_Context        := Get_Context'Access;
   Definition.Enable_Ali_Parser  := Enable_Ali_Parser'Access;
   Definition.Ali_Parser_Is_On   := Ali_Parser_Is_Enabled'Access;
   Definition.Are_Sources_Loaded := Are_Sources_Loaded'Access;
end GPR2.Project.Tree;
