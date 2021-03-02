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

with Ada.Containers.Ordered_Maps;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GPR2.Parser.Project.Create;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Definition;
with GPR2.Project.Import.Set;
with GPR2.Project.Name_Values;
with GPR2.Project.Registry.Pack;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;
with GPR2.View_Ids.Set;
with GNAT.OS_Lib;
with GNAT.Regexp;

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

   Version_Regexp  : constant Regexp.Regexp :=
                      Regexp.Compile (".[0-9]+(.[0-9]+)?");

   Wildcards       : constant Ada.Strings.Maps.Character_Set :=
                       Ada.Strings.Maps.To_Set ("?*");
   --  Wild chars for filename pattern

   procedure Error
      (Self : in out Object;
       Msg  : String;
       Sloc : Source_Reference.Object'Class);
   --  Append an error to Self.Messages

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
   --  Get source by unit name and kind from the same subtree with the View.

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
     (Self          : Object;
      Filename      : Path_Name.Object;
      Context       : Context_Kind;
      Root_Context  : in out GPR2.Context.Object;
      Messages      : out Log.Object;
      Starting_From : View.Object := View.Undefined) return View.Object
     with Pre =>
       (if Starting_From.Is_Defined
        then Starting_From.Qualifier in Aggregate_Kind);
   --  Load a project Filename recursively and returns the corresponding root
   --  view.
   --  Context indicates if project is loaded using the root context or the
   --  root aggregate context.
   --  Root_Context is the new context (either the root one of the aggregate
   --  one). Note that it might be different from Self.Context (Root) and
   --  Self.Context(Aggregate) which are containing the previous context at
   --  this stage.
   --  Messages is the message handler that receives all messages emited
   --  during the recursive load
   --  Starting_From if set is the aggregate library starting point for
   --  the parsing. It is passed here for detecting circular dependencies.

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

   function Get_View_By_Id
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
         return False;
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
      pragma Unreferenced (Self);
   begin
      --  Constant reference is given by the constant reference of the
      --  element contained in the Views set at the current location.
      return Constant_Reference_Type'
        (View =>
           Project_View_Store.Constant_Reference
             (Position.Views, Position.Current).Element);
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

      procedure Add_Attribute (Name : Name_Type; Value : Value_Type);
      --  Add builtin attribute into Data.Attrs

      -------------------
      -- Add_Attribute --
      -------------------

      procedure Add_Attribute (Name : Name_Type; Value : Value_Type) is
      begin
         Data.Attrs.Insert
           (Project.Attribute.Create
              (Name  => Source_Reference.Identifier.Object
                          (Source_Reference.Identifier.Create
                             (Source_Reference.Builtin, Name)),
               Value => Source_Reference.Value.Object
                          (Source_Reference.Value.Create
                             (GPR2.Source_Reference.Object
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

         Add_Attribute (PRA.Source_Dirs, RTD.Value.Text & DS & "adainclude");
         Add_Attribute (PRA.Object_Dir,  RTD.Value.Text & DS & "adalib");

         --  The only language supported is Ada

         Add_Attribute (PRA.Languages, "ada");

         Data.Tree    := Self.Self;
         Data.Kind    := K_Standard;
         Data.Path    := Path_Name.Create_Directory
                           (Filename_Type (RTD.Value.Text));
         Data.Is_Root := True;

         Data.Trees.Project := Parser.Project.Create
           (Name      => PRA.Runtime,
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
      begin
         if not Seen.Contains (View) then
            Seen.Insert (View);

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
                  if Data.Extended.Is_Defined then
                     if Iter.Kind (I_Recursive) then
                        For_Project (Data.Extended);
                     else
                        Append (Data.Extended);
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
            if View /= Project.View.Undefined then
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
                          View.Source_Path
                            (Base_Name,
                             Need_Update =>
                                not Definition.Is_Sources_Loaded (View));
         begin
            if Full_Path /= Path_Name.Undefined then
               Add_File (Full_Path, False);
            end if;
         end Handle_Source_File_In_View;

      begin
         if not Predefined_Only then
            if View /= Project.View.Undefined then
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
      Filename : constant Filename_Type :=
                   (if Source.Has_Dir_Name
                    then Filename_Type (Source.Value)
                    else Source.Simple_Name);
      Pos      : Filename_View.Cursor := Self.Sources.Find (Filename);
   begin
      if Filename_View.Has_Element (Pos) then
         return Filename_View.Element (Pos);

      else
         --  Try to update sources and check again

         Update_Sources (Self);
         Pos := Self.Sources.Find (Filename);

         if Filename_View.Has_Element (Pos) then
            return Filename_View.Element (Pos);
         else
            return Project.View.Undefined;
         end if;
      end if;
   end Get_View;

   function Get_View
     (Self : Object;
      Unit : Name_Type) return Project.View.Object
   is
      Pos : Name_View.Cursor := Self.Units.Find (Unit);
   begin
      if Name_View.Has_Element (Pos) then
         return Name_View.Element (Pos);

      else
         --  Try to update the sources and check again

         Update_Sources (Self);
         Pos := Self.Units.Find (Unit);

         if Name_View.Has_Element (Pos) then
            return Name_View.Element (Pos);
         else
            return Project.View.Undefined;
         end if;
      end if;
   end Get_View;

   --------------------
   -- Get_View_By_Id --
   --------------------

   function Get_View_By_Id
     (Tree : Project.Tree.Object;
      Id   : IDS.View_Id) return Project.View.Object is
   begin
      if Tree.View_Ids.Contains (Id) then
         return Tree.View_Ids (Id);
      else
         return Project.View.Undefined;
      end if;
   end Get_View_By_Id;

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
      return Self.View_Instances.Element (Instance_Id);
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
      else
         Definition.Get (View).Sources_Signature :=
           GPR2.Context.Default_Signature;
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
                           GPR2.Path_Name.Set.Empty_Set)
   is
      procedure Set_Project_Search_Paths;
      --  Set project search path for the tree

      ------------------------------
      -- Set_Project_Search_Paths --
      ------------------------------

      procedure Set_Project_Search_Paths is

         GNAT_Prefix : constant String := Get_Tools_Directory;

         procedure Append (Dir1, Dir2  : String)
           with Post => Self.Search_Paths'Old.Length + 1
                        = Self.Search_Paths.Length;

         ------------
         -- Append --
         ------------

         procedure Append (Dir1, Dir2  : String) is
         begin
            Self.Search_Paths.Append
              (Path_Name.Create_Directory
                 (Filename_Type
                    (Directories.Compose
                       (Directories.Compose
                          (Directories.Compose
                             (GNAT_Prefix, String (Self.Target)), Dir1),
                        Dir2))));
         end Append;

      begin
         if GNAT_Prefix /= "" then
            --  <prefix>/<target>/share/gpr

            Append ("share", "gpr");

            --  <prefix>/<target>/lig/gnat

            Append ("lib", "gnat");
         end if;
      end Set_Project_Search_Paths;

      Project_Path  : Path_Name.Object;
      Root_Context  : GPR2.Context.Object := Context;
      Def           : Definition.Ref;

   begin
      Self.Self := Self'Unchecked_Access;

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

            pragma Assert (P_Data.Kind = K_Configuration, P_Data.Kind'Img);
         end;
      end if;

      Self.Project_Dir      := Project_Dir;
      Self.Build_Path       := Build_Path;
      Self.Subdirs          := To_Unbounded_String (String (Subdirs));
      Self.Src_Subdirs      := To_Unbounded_String (String (Src_Subdirs));
      Self.Check_Shared_Lib := Check_Shared_Lib;
      Self.Implicit_With    := Implicit_With;
      Self.Absent_Dir_Error := Absent_Dir_Error;

      --  Now we can initialize the project search paths

      Set_Project_Search_Paths;

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

      --  Add all search paths into the message log

      for P of Self.Search_Paths loop
         Self.Messages.Append
           (Message.Create
              (Message.Information,
               P.Value,
               Source_Reference.Create (Project_Path.Value, 0, 0)));
      end loop;

      Self.Root := Recursive_Load
        (Self,
         Filename      => Project_Path,
         Context       => Root,
         Root_Context  => Self.Context (Root),
         Messages      => Self.Messages);

      --  Do nothing more if there are errors during the parsing

      if not Self.Messages.Has_Error then
         --  Add to root view's externals, configuration project externals

         if Config.Is_Defined and then Config.Has_Externals then
            Def := Definition.Get (Self.Root);

            for E of Config.Externals loop
               if not Def.Externals.Contains (E) then
                  Def.Externals.Append (E);
               end if;
            end loop;
         end if;

         for V_Data of Self.Views_Set loop
            --  Compute the external dependencies for the views. This
            --  is the set of external used in the project and in all
            --  imported project.

            Def := Definition.Get (V_Data);

            for V of Def.Imports loop
               for E of Definition.Get_RO (V).Externals loop
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
         end loop;

         Set_Context (Self, Context);

         Definition.Check_Same_Name_Extended (Self.Root);
         Definition.Check_Aggregate_Library_Dirs (Self.Root);
         Definition.Check_Package_Naming (Self.Root);
      end if;

      if Self.Messages.Has_Error then
         raise Project_Error with Project_Path.Value & " syntax error";
      end if;

      pragma Assert (Definition.Check_Circular_References (Self.Root));
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
      Language_Runtimes : Containers.Name_Value_Map :=
                            Containers.Name_Value_Map_Package.Empty_Map;
      Base              : GPR2.KB.Object            := GPR2.KB.Undefined)
   is
      Languages   : Containers.Source_Value_Set;
      Conf        : Project.Configuration.Object;
      GNAT_Prefix : constant String := Get_Tools_Directory;
      Default_Cfg : Path_Name.Object;
      Lang_Sloc   : Attribute.Object;
      --  Keep languages attribute for Sloc parameter in error message

      function Actual_Target return Name_Type;
      --  Returns the target, depending on the parsing stage

      procedure Add_Languages (View : Project.View.Object);
      --  Add project languages into the Languages container to configure.
      --  Warn about project has no languages.

      function Default_Config_File return Filename_Type;
      --  Returns default config filename

      function Runtime
        (Language : Source_Reference.Value.Object) return Optional_Name_Type;
      --  Returns the runtime to use during configuration for the specified
      --  language.

      function Toolchain_Name
        (Language : Source_Reference.Value.Object) return Optional_Name_Type;
      --  Returns toolchain name specified by Toolchain_Name attribute

      function Toolchain_Version
        (Language : Source_Reference.Value.Object) return Optional_Name_Type;
      --  Returns toolchain version specified by Required_Toolchain_Version
      --  attribute.

      function Toolchain_Path
        (Language : Source_Reference.Value.Object) return Optional_Name_Type;
      --  Returns toolchain search path specified by Toolchain_Path attribute

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

         if Self.Root.Check_Attribute
           (PRA.Target, Check_Extended => True, Result => Tmp_Attr)
         then
            --  Check if the project explicitly defines the attribute or if
            --  this comes from a default value
            if not Tmp_Attr.Is_Default then
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
         if View.Languages.Length = 0
           and then not View.Is_Abstract
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
               Languages.Include (L);
            end loop;

            --  Keep languages attribute for possible error message Sloc
            --  parameter.

            Lang_Sloc := View.Attributes.Languages;
         end if;
      end Add_Languages;

      -------------------------
      -- Default_Config_File --
      -------------------------

      function Default_Config_File return Filename_Type is
         Ada_RTS : constant Filename_Optional :=
                     Filename_Optional
                       (Containers.Value_Or_Default
                          (Language_Runtimes, "Ada"));
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
        (Language : Source_Reference.Value.Object) return Optional_Name_Type
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
                        (Language_Runtimes, Name_Type (Language.Text));

      begin
         if LRT /= No_Value then
            --  Return the value given as parameter
            return Name_Type (LRT);
         end if;

         if Self.Root.Check_Attribute
           (PRA.Runtime, Attribute_Index.Create (Language.Text),
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
        (Language : Source_Reference.Value.Object) return Optional_Name_Type
      is
         Tmp_Attr : GPR2.Project.Attribute.Object;
      begin
         if Self.Root.Check_Attribute
           (PRA.Toolchain_Name, Attribute_Index.Create (Language.Text),
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
        (Language : Source_Reference.Value.Object) return Optional_Name_Type
      is
         Tmp_Attr : GPR2.Project.Attribute.Object;
      begin
         if Self.Root.Check_Attribute
           (PRA.Toolchain_Path, Attribute_Index.Create (Language.Text),
            Check_Extended => True, Result => Tmp_Attr)
           and then Tmp_Attr.Value.Text /= ""
         then
            return Name_Type (GNAT.OS_Lib.Normalize_Pathname
                              (Tmp_Attr.Value.Text, Self.Root.Dir_Name.Value));
         end if;

         return No_Name;
      end Toolchain_Path;

      -----------------------
      -- Toolchain_Version --
      -----------------------

      function Toolchain_Version
        (Language : Source_Reference.Value.Object) return Optional_Name_Type
      is
         Tmp_Attr : GPR2.Project.Attribute.Object;
      begin
         if Self.Root.Check_Attribute
           (PRA.Required_Toolchain_Version,
            Attribute_Index.Create (Language.Text),
            Check_Extended => True, Result => Tmp_Attr)
           and then Tmp_Attr.Value.Text /= ""
         then
            return Name_Type (Tmp_Attr.Value.Text);
         end if;

         return No_Name;
      end Toolchain_Version;

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
            Absent_Dir_Error => False, --  Ignore obj dir for this first load
            Implicit_With    => Implicit_With);

         --  Ignore messages issued with this initial load: as we don't have
         --  a valid configuration here, we can't really know whether they
         --  are meaningful or not
         Self.Messages.Clear;

         if Self.Root.Is_Externally_Built then
            --  If we have externally built project, configure only the root
            --  one.

            Add_Languages (Self.Root);

         else
            --  If we have non externally built project, configure the none
            --  externally built tree part.

            for C in Self.Iterate
              (Filter =>
                 (F_Aggregate | F_Aggregate_Library => False, others => True),
               Status => (S_Externally_Built => False))
            loop
               Add_Languages (Element (C));
            end loop;
         end if;

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

         declare
            Descr_Index       : Natural := 0;
            Conf_Descriptions : Project.Configuration.Description_Set
                                  (1 .. Positive (Languages.Length));
         begin
            for L of Languages loop
               Descr_Index := Descr_Index + 1;

               Conf_Descriptions (Descr_Index) :=
                 Project.Configuration.Create
                   (Language => Name_Type (L.Text),
                    Version  => Toolchain_Version (L),
                    Runtime  => Runtime (L),
                    Path     => Toolchain_Path (L),
                    Name     => Toolchain_Name (L));
            end loop;

            if not Self.Base.Is_Defined then
               Self.Base := GPR2.KB.Create (GPR2.KB.Default_Flags);
            end if;

            Conf := Project.Configuration.Create
              (Conf_Descriptions,
               Actual_Target,
               Self.Root.Path_Name,
               Self.Base);
         end;

         --  Unload the project that was loaded without configuration.
         --  We need to backup the messages and default search path:
         --  messages issued during configuration are relevant, together with
         --  already computed search paths
         declare
            Old_Messages : constant Log.Object := Self.Messages;
            Old_Paths    : constant Path_Name.Set.Object := Self.Search_Paths;
         begin
            Self.Unload;
            Self.Messages := Old_Messages;
            Self.Search_Paths := Old_Paths;
         end;
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

      procedure Insert (V : View.Object);

      ------------
      -- Insert --
      ------------

      procedure Insert (V : View.Object) is
      begin
         if V.Is_Extending and then V.Instance_Of = V.Extended.Instance_Of
         then
            Insert (V.Extended);
         end if;
         Result.Append (V);
      end Insert;

   begin
      for Id of Self.View_DAG.Topological_Sort loop
         Insert (Self.Instance_Of (Id));
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
      Self.Units.Include (Unit, View);
      Self.Sources.Include (Filename_Type (Source.Value), View);
      Self.Sources.Include (Source.Simple_Name, View);
   end Record_View;

   --------------------
   -- Recursive_Load --
   --------------------

   function Recursive_Load
     (Self          : Object;
      Filename      : Path_Name.Object;
      Context       : Context_Kind;
      Root_Context  : in out GPR2.Context.Object;
      Messages      : out Log.Object;
      Starting_From : View.Object := View.Undefined) return View.Object
   is
      Search_Path : Path_Name.Set.Object := Self.Search_Paths;
      PP          : Attribute.Object;

      function Load (Filename : Path_Name.Object) return Definition.Data;
      --  Returns the Data definition for the given project

      function Internal
        (Filename : Path_Name.Object;
         Status   : Relation_Status;
         Parent   : View.Object) return View.Object;

      function Is_Limited
         (View         : GPR2.Project.View.Object;
          Import_Path  : Path_Name.Object) return Boolean;
      --  Returns True if the Import_Path is a limited with in View.

      procedure Add_Paths_Messages;
      --  Add into Messages the path of the detected circularity

      type Data is record
         Project  : GPR2.Project.Import.Object;
         Extended : Boolean;
      end record;

      package Data_Set is new Ada.Containers.Ordered_Maps
        (GPR2.Path_Name.Object, Data, "<" => GPR2.Path_Name."<");

      Sets          : Data_Set.Map;
      Project_Stack : Path_Name.Set.Object;
      --  Path to the root of the tree from the currently processing project

      ------------------------
      -- Add_Paths_Messages --
      ------------------------

      procedure Add_Paths_Messages is
      begin
         for Import of Project_Stack loop
            declare
               Def : constant Data := Sets.Element (Import);
            begin
               Messages.Append
                 (Message.Create
                    (Message.Error,
                     (if Def.Extended then "extends" else "imports")
                      & " " & Import.Value,
                     Def.Project));
            end;
         end loop;
      end Add_Paths_Messages;

      --------------
      -- Internal --
      --------------

      function Internal
        (Filename : Path_Name.Object;
         Status   : Relation_Status;
         Parent   : View.Object) return View.Object
      is
         Id   : constant IDS.View_Id :=
                  IDS.Create
                    (Project_File => Filename, Context => Context);
         View : Project.View.Object := Self.Get_View_By_Id (Id);

      begin
         --  If the view is already defined just return it

         if not View.Is_Defined then
            declare
               Data : Definition.Data := Load (Filename);
            begin
               --  If there are parsing errors, do not go further

               if Messages.Has_Error then
                  return View;
               end if;

               --  Compute directory used as project file directory
               --  This is influenced by the Project_Dir parameters used on
               --  load to simulate that a project is in another location.

               Data.Path := (if Self.Project_Dir.Is_Defined
                             then Self.Project_Dir
                             else Path_Name.Create_Directory
                                    (Filename_Type (Filename.Dir_Name)));

               --  If parent view is an aggregate or an extending project keep
               --  track of the relationship.

               case Status is
                  when Extended =>
                     Data.Extending := Definition.Weak (Parent);
                  when Aggregated =>
                     Data.Aggregate := Definition.Weak (Parent);
                  when others =>
                     null;
               end case;

               --  Update context associated with the the list of
               --  externals defined in that project file.

               Update_Context (Root_Context, Data.Externals);

               --  At this stage even if not complete we can create the view
               --  and register it so that we can have references to it.

               if Status = Root then
                  Data.Is_Root := True;
               end if;

               Data.Context     := Context;
               Data.Unique_Id   := Id;
               Data.Instance_Of := Id;
               View := Register_View (Data);

               --  Keep track of the view in View_Ids
               --  It is important to note that this ensures we don't start to
               --  recurse infinitely while loading a project tree.

               View.Tree.View_Ids.Include (Data.Unique_Id, View);
            end;

            declare
               Data : constant Definition.Ref := Definition.Get_RW (View);
            begin

               --  Set root view regarding context namespace
               --  ??? (need more explanation)

               if Status = Root
                  or else (Status = Aggregated and then not Parent.Is_Library)
               then
                  --  This is the root project or an aggregate project. This
                  --  create a new namespace (i.e root in the subtree)

                  Data.Root_View := Definition.Weak (View);

               else
                  --  ??? what happens if a project is withed inside several
                  --  aggregate projects ???

                  Data.Root_View := Definition.Get_RO (Parent).Root_View;
               end if;

               --  Load all imported projects

               for Project of Data.Trees.Imports loop
                  declare
                     Imported_View : constant GPR2.Project.View.Object :=
                        Internal
                          (Project.Path_Name,
                           Status    => Imported,
                           Parent    => View);
                  begin
                     if Imported_View.Is_Defined then
                        --  limited with and with are tracked separately due
                        --  their very distinct nature.

                        if Is_Limited (View, Project.Path_Name) then
                           Data.Limited_Imports.Insert
                              (Project.Name, Imported_View);
                        else

                           Data.Imports.Insert (Project.Name, Imported_View);
                        end if;
                     end if;
                  end;
               end loop;

               --  Load the extended project if any
               if Data.Trees.Extended.Is_Defined then
                  declare
                     Extended_View : constant GPR2.Project.View.Object :=
                                       Internal
                                         (Data.Trees.Extended.Path_Name,
                                          Status => Extended,
                                          Parent => View);

                  begin
                     if Extended_View.Is_Defined then
                        Data.Extended := Extended_View;

                        --  If this is an extending all project in that case
                        --  current project is an instance of the extended
                        --  project.

                        if Data.Trees.Project.Is_Extending_All then
                           Data.Instance_Of :=
                              Definition.Get_RO (Data.Extended).Instance_Of;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;

         --  At this stage the view is complete. Update mappings
         --  (i.e effective view for extends and extends all) and DAG to
         --  order the views.

         declare
            use IDS;
            Unique_ID   : constant View_Id := View.Id;
            Instance_Of : constant View_Id := View.Instance_Of;
         begin
            pragma Assert (Is_Defined (Unique_ID));
            pragma Assert (Is_Defined (Instance_Of));

            --  Update View_Instances mapping

            if View.Tree.View_Instances.Contains (Instance_Of) then
               --  An instance is already registed
               declare
                  Previous_View : constant GPR2.Project.View.Object :=
                     View.Tree.View_Instances.Element (Instance_Of);
               begin
                  if View.Is_Extension_Of (Previous_View) then
                     --  If the current view is an extension of the previously
                     --  registered extension, then the current view is now
                     --  the official instance.

                     View.Tree.View_Instances.Include (Instance_Of, View);

                  elsif not View.Is_Extended_By (Previous_View) then
                     --  If the current view is not extendedby the previous and
                     --  not an extension of it then it means that the project
                     --  structure leads to incompatible instances of a view.
                     --  In that case raise an error.

                     raise Project_Error with "invalid structure";
                  end if;
               end;
            else
               View.Tree.View_Instances.Include (Instance_Of, View);
            end if;

            --  Finally update the DAG structure that will define the
            --  processing order for the views.

            declare
               Predecessors : GPR2.View_Ids.Set.Object;
            begin
               if View.Has_Imports then
                  for Import of View.Imports loop
                     Predecessors.Include (Import.Instance_Of);
                  end loop;
               end if;

               View.Tree.View_DAG.Update_Vertex
                  (Vertex       => Instance_Of,
                   Predecessors => Predecessors);
            end;

            --  Add aggregate dependency

            if Parent.Is_Defined and then Status = Aggregated then
               View.Tree.View_DAG.Update_Vertex
                  (Vertex      => Parent.Instance_Of,
                   Predecessor => Instance_Of);
            end if;

            --  Add dependency on extended if not a "extends all"

            if View.Is_Extending
              and then View.Extended.Instance_Of /= Instance_Of
            then
               View.Tree.View_DAG.Update_Vertex
                  (Vertex      => Instance_Of,
                   Predecessor => View.Extended.Instance_Of);
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
                       (Filename, Self.Implicit_With, Messages);
         Data    : Definition.Data;
      begin
         Data.Trees.Project := Project;

         --  Record the project tree for this view

         Data.Tree := Self.Self;
         Data.Kind := K_Standard;

         --  Do the following only if there are no error messages

         if not Messages.Has_Error then
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
                          (Import_Filename, Self.Implicit_With, Messages));

                  else
                     Add_Paths_Messages;

                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
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
                        (Extended_Filename, Self.Implicit_With, Messages);
                  else
                     Add_Paths_Messages;

                     Messages.Append
                       (GPR2.Message.Create
                          (Level   => Message.Error,
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

   begin
      if Starting_From.Is_Defined
        and then Starting_From.Check_Attribute (PRA.Project_Path, Result => PP)
      then
         for P of PP.Values loop
            Search_Path.Append
              (Path_Name.Create_Directory
                 (Filename_Type (P.Text),
                  Filename_Type (Starting_From.Dir_Name.Value)));
         end loop;
      end if;

      return Internal
         (Filename => Filename,
          Status   => (if Context = Aggregate then Aggregated else Root),
          Parent   => Starting_From);
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
      if Def.Tree.Views_Set.Is_Empty then
         Def.Id := 1;
      else
         Def.Id := Definition.Get_RO (Def.Tree.Views_Set.Last_Element).Id + 1;
      end if;

      Definition.Set (View, Def);

      Def.Tree.Views_Set.Insert (View);

      pragma Assert (Definition.Refcount (View) = 2);

      Add_View (Path_Name.To_OS_Case (View.Path_Name.Value));

      pragma Assert (Definition.Refcount (View) = 3);

      Add_View (To_Lower (View.Name));

      pragma Assert (Definition.Refcount (View) = 4);

      return View;
   end Register_View;

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

      if Source.Source.Has_Units then
         for U of Source.Source.Units loop
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
     (Self : Object; Language : Name_Type) return Optional_Name_Type
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
           Index => GPR2.Project.Attribute_Index.Create
             (Value_Type (Language), Case_Sensitive => False),
           Check_Extended => True,
           Result         => TA)
      then
         return Name_Type (TA.Value.Text);

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

            function Is_Defined_Empty (Attr : Name_Type) return Boolean;
            --  Returns True if attribute defined as empty list in view

            ----------------------
            -- Is_Defined_Empty --
            ----------------------

            function Is_Defined_Empty (Attr : Name_Type) return Boolean is
            begin
               return View.Check_Attribute (Attr, Result => Tmp_Attr)
                 and then Tmp_Attr.Values.Is_Empty;
            end Is_Defined_Empty;

         begin
            if View.Is_Abstract then
               return True;
            end if;

            if View.Is_Extending
              and then not Is_Implicitly_Abstract (View.Extended)
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
            P_Data.Types);

         if View.Qualifier not in Aggregate_Kind then
            if P_Data.Attrs.Contains (PRA.Project_Files) then
               Self.Error
                  ("""project_files"" is only valid in aggregate projects",
                   P_Data.Attrs.Element (PRA.Project_Files));
            else
               New_Signature := View.Context.Signature (P_Data.Externals);
            end if;

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
               for Pathname of Get_Matching_Files (Project) loop
                  if Pathname = View.Path_Name then
                     --  We are loading recursively the aggregate project
                     --  As in GPR1 ignore it.
                     null;

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

                  elsif Pathname.Exists then
                     declare
                        Messages      : Log.Object;
                        A_View        : constant GPR2.Project.View.Object :=
                                          Recursive_Load
                                            (Self          => Self,
                                             Filename      => Pathname,
                                             Context       => Aggregate,
                                             Root_Context  => Self.Context
                                                                (Aggregate),
                                             Messages      => Messages,
                                             Starting_From => View);
                     begin
                        --  If there was error messages during the parsing of
                        --  the aggregated project, just return now.

                        if Messages.Has_Error then
                           Self.Error ("aggregate " & Project.Text, Project);

                           --  And copy back all messages from the recursive
                           --  load routine above.

                           for M of Messages loop
                              Self.Messages.Append (M);
                           end loop;

                           return;
                        end if;

                        --  Record aggregated view into the aggregate's view

                        P_Data.Aggregated.Insert
                          (Name_Type (Pathname.Value), A_View);
                     end;

                  else
                     Self.Error
                        ("file """ & Project.Text & """ not found", Project);
                     exit;
                  end if;
               end loop;
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
                           & (if Tmp_Attr.Name.Text = PRA.Source_Dirs
                              then "source directories"
                              elsif Tmp_Attr.Name.Text = PRA.Languages
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

               elsif View.Is_Extending and then View.Extended.Is_Library
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

            if Changed /= null and then Old_Signature /= New_Signature then
               Changed (View);
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
                  ("attribute """ & String (A.Name.Text)
                   & """ cannot have index", A);
            end if;

            if Def.Value = PRA.Single and then A.Kind = PRA.List then
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & String (A.Name.Text)
                     & """ cannot be a list",
                     A));

               if A.Name.Text = PRA.Object_Dir then
                  Check_Object_Dir_Exists := False;
               elsif A.Name.Text = PRA.Exec_Dir then
                  Check_Exec_Dir_Exists := False;
               end if;
            end if;

            if Def.Value = PRA.List and then A.Kind = PRA.Single then
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & String (A.Name.Text)
                     & """ must be a list",
                     A.Value));
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
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "package """ & String (P.Name)
                        & """ cannot be used in " & Image (P_Kind) & 's',
                        P));
               end if;

               --  Check package's attributes

               for A of P.Attributes loop
                  declare
                     Q_Name : constant PRA.Qualified_Name :=
                                PRA.Create (A.Name.Text, P.Name);
                     Def    : PRA.Def;
                  begin
                     if PRA.Exists (Q_Name) then
                        Def := PRA.Get (Q_Name);

                        if not Def.Is_Allowed_In (P_Kind) then
                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "attribute """ & String (A.Name.Text)
                                 & """ cannot be used in package "
                                 & String (P.Name),
                                 A));
                        end if;

                        Check_Def (Def, A);

                     elsif PRP.Attributes_Are_Checked (P.Name) then
                        Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "attribute """ & String (A.Name.Text)
                              & """ not supported in package "
                              & String (P.Name),
                              A));
                     end if;
                  end;
               end loop;
            end if;
         end loop;

         --  Check top level attributes

         for A of P_Data.Attrs loop
            declare
               Q_Name : constant PRA.Qualified_Name :=
                          PRA.Create (A.Name.Text);
            begin
               if not PRA.Exists (Q_Name) then
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "unrecognized attribute """ & String (A.Name.Text)
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

                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 '"' & String (A.Name.Text)
                                 & """ is only valid in "
                                 & Image (Allow) & 's',
                                 A));
                        else
                           --  If more than one is allowed use excluding
                           --  error message.

                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "attribute """ & String (A.Name.Text)
                                 & """ cannot be used in "
                                 & Image (P_Kind) & 's',
                                 A));
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
              (Attr_Name     : Name_Type;
               Human_Name    : String;
               Get_Directory : not null access function
                 (Self : Project.View.Object) return Path_Name.Object;
               Mandatory     : Boolean := False);
            --  Check is directory exists and warn if there is try to relocate
            --  absolute path with --relocate-build-tree gpr tool command line
            --  parameter. Similar check for attributes with directory names.

            Attr : Attribute.Object;

            ---------------------
            -- Check_Directory --
            ---------------------

            procedure Check_Directory
              (Attr_Name     : Name_Type;
               Human_Name    : String;
               Get_Directory : not null access function
                 (Self : Project.View.Object) return Path_Name.Object;
               Mandatory     : Boolean := False) is
            begin
               if View.Check_Attribute (Attr_Name, Result => Attr) then
                  declare
                     AV : constant Source_Reference.Value.Object := Attr.Value;
                     PN : constant Path_Name.Object := Get_Directory (View);
                  begin
                     if not PN.Exists then
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
                        "attribute " & String (Attr_Name) & " not declared",
                        Source_Reference.Create (View.Path_Name.Value, 0, 0)));
               end if;
            end Check_Directory;

         begin
            if View.Kind in K_Standard | K_Library | K_Aggregate_Library
              and then Check_Object_Dir_Exists
            then
               Check_Directory
                 (PRA.Object_Dir, "object",
                  Project.View.Object_Directory'Access);
            end if;

            if View.Is_Library then
               --  Library_Version attribute has no effect on Windows

               if not View.Tree.Is_Windows_Target
                 and then View.Is_Shared_Library
                 and then View.Check_Attribute
                            (PRA.Library_Version,
                             Check_Extended => True, Result => Attr)
               then
                  declare
                     AV      : constant Source_Reference.Value.Object :=
                                 Attr.Value;
                     Lib_Ver : constant Value_Type := AV.Text;
                     Lib_Fn  : constant Value_Type :=
                                 Value_Type (View.Library_Filename.Name);
                  begin
                     if not GNATCOLL.Utils.Starts_With (Lib_Ver, Lib_Fn)
                       or else not Regexp.Match
                         (Lib_Ver
                            (Lib_Ver'First + Lib_Fn'Length .. Lib_Ver'Last),
                          Version_Regexp)
                     then
                        Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              '"' & Lib_Ver
                              & """ not correct format for Library_Version",
                              Sloc => AV));
                     end if;
                  end;
               end if;

               Check_Directory
                 (PRA.Library_Dir, "library",
                  Project.View.Library_Directory'Access,
                  Mandatory => True);

               Check_Directory
                 (PRA.Library_Ali_Dir, "library ALI",
                  Project.View.Library_Ali_Directory'Access);

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
                              Sloc => View.Attribute (PRA.Project_Files)));
                     end if;
                  end loop;

                  if View.Has_Imports then
                     for Imported of View.Imports loop
                        if not Imported.Is_Abstract then
                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "can only import abstract projects, not """
                                 & String (Imported.Name) & '"',
                                 Sloc => View.Attribute (PRA.Project_Files)));
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

      begin
         declare
            Closure_Found : Boolean := True;
            Closure       : GPR2.View_Ids.Set.Object;
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
                  if not Closure.Contains (View.Id) then
                     Closure_Found := False;
                     Closure.Insert (View.Id);
                     Set_View (View);
                  end if;
               end loop;

               exit when Closure_Found;
               Closure_Found := True;
            end loop;
         end;

      exception
         when GPR2.View_Ids.DAGs.DAG_Error =>
            Self.Messages.Append
               (Message.Create
                  (Message.Error, "circular dependency detected",
                   Source_Reference.Create
                      (Self.Root_Project.Path_Name.Value, 0, 0)));
      end;

      if not Has_Error then
         --  We now have an up-to-date tree, do some validity checks if there
         --  is no issue detected yet.

         for View of Self.Ordered_Views loop
            Validity_Check (View);
         end loop;
      end if;

      if Has_Error then
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

      if Source.Source.Has_Units then
         for U of Source.Source.Units loop
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
      Self.Messages.Clear;
      Self.Views.Clear;
      Self.View_Ids.Clear;
      Self.View_Instances.Clear;
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
         if not Context.Contains (External) then
            --  The external is not present in the current context. Try to
            --  fetch its value from the environment and insert it in the
            --  context.

            declare
               External_Value : constant String :=
                                  Environment_Variables.Value
                                    (String (External), "");
            begin
               if External_Value /= "" then
                  Context.Insert (External, External_Value);
               end if;
            end;
         end if;
      end loop;
   end Update_Context;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources
     (Self          : Object;
      Stop_On_Error : Boolean := True;
      With_Runtime  : Boolean := False;
      Backends      : Source_Info.Backend_Set := Source_Info.All_Backends) is
   begin
      Self.Self.Rooted_Sources.Clear;

      for V of Self.Views_Set loop
         if With_Runtime or else not V.Is_Runtime then
            Definition.Get (V).Update_Sources (V, Stop_On_Error, Backends);
         end if;
      end loop;

      if Self.Check_Shared_Lib then
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

                  ---------------------------
                  -- Has_Essential_Sources --
                  ---------------------------

                  function Has_Essential_Sources
                    (V : Project.View.Object) return Boolean is
                  begin
                     for S of V.Sources (Need_Update => False) loop
                        if S.Source.Language = "Ada"
                          or else S.Source.Kind not in GPR2.Unit.Spec_Kind
                        then
                           return True;
                        end if;
                     end loop;

                     return False;
                  end Has_Essential_Sources;

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
                              P_Data.Trees.Project.Imports.Element
                                (Imp.Path_Name)));

                     elsif Imp.Is_Static_Library
                       and then View.Library_Standalone /= Encapsulated
                     then
                        Self.Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "shared library project """ & String (View.Name)
                              & """ cannot import static library project """
                              & String (Imp.Name) & '"',
                              P_Data.Trees.Project.Imports.Element
                                (Imp.Path_Name)));

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
                              P_Data.Trees.Project.Imports.Element
                                (Imp.Path_Name)));
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
                              PV.Attribute
                                (Registry.Attribute.Library_Standalone)));
                     end if;

                     --  And if a standalone library has interfaces

                     if not PV.Has_Any_Interfaces
                       and then PV.Has_Attributes
                         (Project.Registry.Attribute.Library_Standalone)
                       and then PV.Library_Standalone /= No
                     then
                        Self.Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "Library_Standalone valid only if library"
                              & " has Ada interfaces",
                              PV.Attribute
                                (Registry.Attribute.Library_Standalone)));
                     end if;
                  end if;
               end Check_Shared_Lib;

            begin
               if View.Is_Library and then View.Is_Shared_Library then
                  Check_Shared_Lib (View);
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

begin
   --  Export routines to Definitions to avoid cyclic dependencies

   Definition.Register          := Register_View'Access;
   Definition.Check_Source      := Check_Source'Access;
   Definition.Check_Source_Unit := Check_Source'Access;
   Definition.Has_Source        := Has_Source'Access;
   Definition.Set_Source        := Set_Source'Access;
   Definition.Remove_Source     := Remove_Source'Access;
   Definition.Get_Context       := Get_Context'Access;
end GPR2.Project.Tree;
