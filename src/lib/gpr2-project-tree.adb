------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                     Copyright (C) 2019-2020, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Containers.Ordered_Maps;
with Ada.Environment_Variables;
with Ada.Directories;

with GPR2.Parser.Project.Create;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Definition;
with GPR2.Project.Import.Set;
with GPR2.Project.Name_Values;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source;
with GPR2.Project.Source.Set;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

with GNAT.OS_Lib;
with GNAT.Regexp;

with GNATCOLL.OS.Constants;
with GNATCOLL.Utils;

package body GPR2.Project.Tree is

   use GNAT;
   use type GPR2.Path_Name.Object;
   use type GNATCOLL.OS.OS_Type;

   package PC renames Project.Configuration;
   package PRA renames Project.Registry.Attribute;
   package PRP renames Project.Registry.Pack;

   Is_Windows_Host : constant Boolean :=
                       GNATCOLL.OS.Constants.OS = GNATCOLL.OS.Windows
                         with Warnings => Off;

   Version_Regexp  : constant Regexp.Regexp :=
                      Regexp.Compile (".[0-9]+(.[0-9]+)?");

   function Register_View
     (Def : in out Definition.Data) return Project.View.Object
     with Post => Register_View'Result.Is_Defined;
   --  Register view definition in the Tree and return the View object

   procedure Copy_Definition
     (Target : Definition.Ref; Source : Definition.Data)
     with Inline, Pre => Target.Id = Source.Id;
   --  Copy definition fields

   procedure Set_Context
     (Self    : in out Object;
      Changed : access procedure (Project : View.Object) := null);
   --  Update project tree with updated context

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
     (Self             : Object;
      Filename         : Path_Name.Object;
      Context_View     : View.Object;
      Status           : Relation_Status;
      Root_Context     : out GPR2.Context.Object;
      Messages         : out Log.Object;
      Circularities    : out Boolean;
      Starting_From    : View.Object := View.Undefined;
      Implicit_Project : Boolean     := False) return View.Object
     with Pre =>
       (if Starting_From.Is_Defined
        then Starting_From.Qualifier in Aggregate_Kind);
   --  Load a project filename recursively and returns the corresponding root
   --  view. Starting_From if set is the aggregate library starting point for
   --  the parsing. It is passed here for detecting circular dependencies.

   function Create_Runtime_View (Self : Object) return View.Object
     with Pre => Self.Is_Defined
                 and then Self.Has_Configuration;
   --  Create the runtime view given the configuration project

   function Get
     (Tree         : Project.Tree.Object;
      Path_Name    : GPR2.Path_Name.Object;
      Context_View : Project.View.Object;
      Status       : Relation_Status) return Project.View.Object;
   --  Returns the project view corresponding to Path_Name, Status and
   --  Context_View in the given Tree or Undefined if this project is not
   --  yet registered.

   function Get
     (Tree         : Project.Tree.Object;
      Name         : Name_Type;
      Context_View : Project.View.Object) return Project.View.Object;
   --  Returns the project view corresponding to Name and Context in the given
   --  Tree or Undefined if this project is not yet registered.

   procedure Fill_Externals_From_Environment
     (Context   : in out GPR2.Context.Object;
      Externals : Containers.Name_List);
   --  If any of externals is not available in context, try to get it from
   --  process environment and put into the context.

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
                  Driver : constant String :=
                             (if View.Pack (PRP.Compiler).Has_Attributes
                              (PRA.Driver, Language.Text) then
                                 String (Path_Name.Create_File
                                (Name_Type (View.Pack (
                                   PRP.Compiler).Attribute
                                     (PRA.Driver,
                                      Language.Text).Value.Text)).Base_Name)
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
                           GNAT_Compilers_Prefix (Self.Root_Project);
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

   ----------------
   -- Clear_View --
   ----------------

   procedure Clear_View
     (Self : in out Object;
      Unit : Unit_Info.Object) is
   begin
      --  Clear the corresponding sources

      --  TODO: Self.Sources should contain a case-sensitive string type
      --        instead of Name_Type.

      if Unit.Spec.Is_Defined then
         Self.Sources.Exclude
           (Name_Type (Unit.Spec.Value));
      end if;

      if Unit.Main_Body.Is_Defined then
         Self.Sources.Exclude
           (Name_Type (Unit.Main_Body.Value));
      end if;

      for S of Unit.Separates loop
         Self.Sources.Exclude (Name_Type (S.Value));
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
      return Self.Root_Project.Context;
   end Context;

   ---------------------
   -- Copy_Definition --
   ---------------------

   procedure Copy_Definition
     (Target : Definition.Ref; Source : Definition.Data) is
   begin
      Target.all := Source;
   end Copy_Definition;

   -------------------------
   -- Create_Runtime_View --
   -------------------------

   function Create_Runtime_View (Self : Object) return View.Object is
      CV   : constant View.Object := Self.Conf.Corresponding_View;
      DS   : Character renames OS_Lib.Directory_Separator;
      Data : Project.Definition.Data (Has_Context => False);
      RTD  : Attribute.Object;

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
                             (Source_Reference.Builtin, Value))));
      end Add_Attribute;

   begin
      --  Check runtime path

      if CV.Check_Attribute (PRA.Runtime_Dir, "ada", Result => RTD)
        and then RTD.Value.Text /= ""
      then
         --  Runtime_Dir (Ada) exists, this is used to compute the Source_Dirs
         --  and Object_Dir for the Runtime project view.

         Add_Attribute (PRA.Source_Dirs, RTD.Value.Text & DS & "adainclude");
         Add_Attribute (PRA.Object_Dir,  RTD.Value.Text & DS & "adalib");

         --  The only language supported is Ada

         Add_Attribute (PRA.Languages, "ada");

         Data.Tree   := Self.Self;
         Data.Status := Root;
         Data.Kind   := K_Standard;
         Data.Path   := Path_Name.Create_Directory
                          (Name_Type (RTD.Value.Text));

         Data.Trees.Project := Parser.Project.Create
           (Name      => PRA.Runtime,
            File      => Path_Name.Create_File ("runtime.gpr"),
            Qualifier => K_Standard);

         return Result : constant View.Object := Register_View (Data) do
            --  If we simply return Register_View (Data) the reference counter
            --  will be one more than should be, see T709-001.
            null;
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

   -------------------------------------
   -- Fill_Externals_From_Environment --
   -------------------------------------

   procedure Fill_Externals_From_Environment
     (Context   : in out GPR2.Context.Object;
      Externals : Containers.Name_List) is
   begin
      for E of Externals loop
         --  Fill all known external in the environment variables

         if not Context.Contains (E)
           and then Environment_Variables.Exists (String (E))
         then
            declare
               V : constant String := Environment_Variables.Value (String (E));
            begin
               if V /= "" then
                  --  Treat empty environment valiable like absent

                  Context.Insert (E, V);
               end if;
            end;
         end if;
      end loop;
   end Fill_Externals_From_Environment;

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
     (Tree         : Project.Tree.Object;
      Path_Name    : GPR2.Path_Name.Object;
      Context_View : Project.View.Object;
      Status       : Relation_Status) return Project.View.Object
   is
      Key : constant Name_Type := Name_Type (Path_Name.Value);
   begin
      if Tree.Views.Contains (Key) then
         for V of Tree.Views (Key) loop
            declare
               Defs : constant Definition.Const_Ref := Definition.Get_RO (V);
            begin
               pragma Assert (Defs.Tree.all = Tree);

               if Definition.Strong (Defs.Context_View) = Context_View
                 and then (Defs.Status = Status or else Status /= Aggregated)
               then
                  return V;
               end if;
            end;
         end loop;
      end if;

      return Project.View.Undefined;
   end Get;

   function Get
     (Tree         : Project.Tree.Object;
      Name         : Name_Type;
      Context_View : Project.View.Object) return Project.View.Object is
   begin
      if Tree.Views.Contains (Name) then
         for V of Tree.Views.Constant_Reference (Name) loop
            declare
               Defs : constant Definition.Const_Ref := Definition.Get_RO (V);
            begin
               pragma Assert (Defs.Tree.all = Tree);

               if Definition.Strong (Defs.Context_View) = Context_View then
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
                          View.Source_Path (Base_Name, Need_Update => False);
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
      Filename : constant Name_Type :=
                   (if Source.Has_Dir_Name
                    then Name_Type (Source.Value)
                    else Source.Simple_Name);
      Pos      : Name_View.Cursor := Self.Sources.Find (Filename);
   begin
      if Name_View.Has_Element (Pos) then
         return Name_View.Element (Pos);

      else
         --  Try to update sources and check again

         Update_Sources (Self);
         Pos := Self.Sources.Find (Filename);

         if Name_View.Has_Element (Pos) then
            return Name_View.Element (Pos);
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
      return not Self.Root_Project.Context.Is_Empty;
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
     (Self         : Object;
      Name         : Name_Type;
      Context_View : View.Object) return Boolean
   is
      View : constant Project.View.Object := Self.Get (Name, Context_View);
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
      Config           : PC.Object            := PC.Undefined;
      Build_Path       : Path_Name.Object     := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type   := No_Name;
      Src_Subdirs      : Optional_Name_Type   := No_Name;
      Check_Shared_Lib : Boolean              := True;
      Implicit_Project : Boolean              := False;
      Absent_Dir_Error : Boolean              := False;
      Implicit_With    : Containers.Name_Set  := Containers.Empty_Name_Set)
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
                 (Name_Type
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
      Circularities : Boolean;
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

         if Config.Is_Defined and then Config.Has_Externals then
            Fill_Externals_From_Environment (Root_Context, Config.Externals);
         end if;

         Definition.Bind_Configuration_To_Tree (Self.Conf, Self.Self);

         declare
            C_View : Project.View.Object := Self.Conf.Corresponding_View;
            P_Data : constant Definition.Ref := Definition.Get_RW (C_View);
         begin
            --  Set and record the tree now, needed for the parsing

            P_Data.Tree := Self.Self;

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

            P_Data.Kind := P_Data.Trees.Project.Qualifier;
         end;
      end if;

      Self.Build_Path       := Build_Path;
      Self.Subdirs          := To_Unbounded_String (String (Subdirs));
      Self.Src_Subdirs      := To_Unbounded_String (String (Src_Subdirs));
      Self.Check_Shared_Lib := Check_Shared_Lib;
      Self.Implicit_With    := Implicit_With;
      Self.Absent_Dir_Error := Absent_Dir_Error;

      --  Now we can initialize the project search paths

      Set_Project_Search_Paths;

      if Filename.Has_Dir_Name then
         Project_Path := Filename;

      else
         --  If project directory still not defined, search it in full set of
         --  search paths.

         Project_Path := Create (Filename.Name, Self.Search_Paths);

         if not Build_Path.Is_Defined then
            Self.Build_Path := Path_Name.Create_Directory
              (Name_Type (Project_Path.Dir_Name));
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
        (Self, Project_Path, View.Undefined, Root, Root_Context, Self.Messages,
         Circularities, Implicit_Project => Implicit_Project);

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

      else
         raise Project_Error with Project_Path.Value & " syntax error";
      end if;

      pragma Assert (Definition.Check_Circular_References (Self.Root_Project));
   end Load;

   -------------------
   -- Load_Autoconf --
   -------------------

   procedure Load_Autoconf
     (Self              : in out Object;
      Filename          : Path_Name.Object;
      Context           : GPR2.Context.Object;
      Build_Path        : Path_Name.Object     := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type   := No_Name;
      Src_Subdirs       : Optional_Name_Type   := No_Name;
      Check_Shared_Lib  : Boolean              := True;
      Implicit_Project  : Boolean              := False;
      Absent_Dir_Error  : Boolean              := False;
      Implicit_With     : Containers.Name_Set  := Containers.Empty_Name_Set;
      Target            : Optional_Name_Type   := No_Name;
      Language_Runtimes : Containers.Name_Value_Map :=
                            Containers.Name_Value_Map_Package.Empty_Map)
   is
      Languages   : Containers.Source_Value_Set;
      Descr_Index : Natural := 0;
      Conf        : Project.Configuration.Object;

      procedure Add_Languages (View : Project.View.Object);
      --  Add project languages into the Languages container to configure.
      --  Warn about project has no languages.

      -------------------
      -- Add_Languages --
      -------------------

      procedure Add_Languages (View : Project.View.Object) is
      begin
         if View.Languages.Length = 0 then
            Self.Append_Message
              (Message.Create
                 (Level   => Message.Warning,
                  Message => "no language for the project "
                  & String (View.Name),
                  Sloc    => View.Attributes.Languages));
         end if;

         for L of View.Languages loop
            Languages.Include (L);
         end loop;
      end Add_Languages;

   begin
      Self.Load
        (Filename, Context,
         Build_Path       => Build_Path,
         Subdirs          => Subdirs,
         Src_Subdirs      => Src_Subdirs,
         Check_Shared_Lib => Check_Shared_Lib,
         Implicit_Project => Implicit_Project,
         Absent_Dir_Error => Absent_Dir_Error,
         Implicit_With    => Implicit_With);

      if Self.Root_Project.Is_Externally_Built then
         --  If we have externally built project, configure only the root one

         Add_Languages (Self.Root_Project);

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
               Sloc    => Self.Root_Project.Attributes.Languages));
         return;
      end if;

      declare
         Tmp_Attr      : Attribute.Object;
         Actual_Target : constant Name_Type :=
                           (if Target /= No_Name then Target
                            elsif Self.Root_Project.Check_Attribute
                                    (PRA.Target, Result => Tmp_Attr)
                            then Name_Type (Tmp_Attr.Value.Text)
                            else "all");

         Conf_Descriptions : Project.Configuration.Description_Set
                               (1 .. Positive (Languages.Length));

      begin
         for L of Languages loop
            Descr_Index := Descr_Index + 1;

            declare
               LRT : constant Value_Type :=
                       Containers.Value_Or_Default
                         (Language_Runtimes, Name_Type (L.Text));
               RTS : constant Optional_Name_Type :=
                       Optional_Name_Type
                         (if LRT = No_Value
                            and then Self.Root_Project.Check_Attribute
                                       (PRA.Runtime, L.Text,
                                        Result => Tmp_Attr)
                          then Tmp_Attr.Value.Text
                          else LRT);
            begin
               Conf_Descriptions (Descr_Index) :=
                 Project.Configuration.Create
                   (Language => Name_Type (L.Text),
                    Version  => No_Name,
                    Runtime  => RTS,
                    Path     => No_Name,
                    Name     => No_Name);
            end;
         end loop;

         Conf := Project.Configuration.Create
           (Conf_Descriptions, Actual_Target, Self.Root_Project.Path_Name);

         Self.Load
           (Self.Root_Project.Path_Name, Context, Conf, Build_Path,
            Subdirs          => Subdirs,
            Src_Subdirs      => Src_Subdirs,
            Check_Shared_Lib => Check_Shared_Lib,
            Implicit_Project => Implicit_Project,
            Implicit_With    => Implicit_With);
      end;
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
      Self.Sources.Include (Name_Type (Source.Value), View);
      Self.Sources.Include (Source.Simple_Name, View);
   end Record_View;

   --------------------
   -- Recursive_Load --
   --------------------

   function Recursive_Load
     (Self             : Object;
      Filename         : Path_Name.Object;
      Context_View     : View.Object;
      Status           : Relation_Status;
      Root_Context     : out GPR2.Context.Object;
      Messages         : out Log.Object;
      Circularities    : out Boolean;
      Starting_From    : View.Object := View.Undefined;
      Implicit_Project : Boolean := False) return View.Object
   is

      function Load (Filename : Path_Name.Object) return Definition.Data;
      --  Returns the Data definition for the given project

      function Internal
        (Filename : Path_Name.Object;
         Status   : Relation_Status;
         Parent   : View.Object) return View.Object;

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

      Limited_Count : Natural := 0;
      --  Number of limited imports in the Paths

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
         View : Project.View.Object :=
                  Self.Get (Filename, Context_View, Status);
      begin
         if not View.Is_Defined then
            declare
               Data : Definition.Data := Load (Filename);
            begin
               --  If there are parsing errors, do not go further

               if Messages.Has_Element
                 (Information => False, Warning => False)
               then
                  return View;
               end if;

               Data.Path := Path_Name.Create_Directory
                 (Name_Type
                    (if Implicit_Project
                     then Ada.Directories.Current_Directory
                     else Filename.Dir_Name));

               case Status is
                  when Extended =>
                     Data.Extending := Definition.Weak (Parent);
                  when Aggregated =>
                     Data.Aggregate := Definition.Weak (Parent);
                  when others =>
                     null;
               end case;

               --  Let's setup the full external environment for project

               Fill_Externals_From_Environment (Root_Context, Data.Externals);

               --  If we have the root project, record the global context

               if Data.Has_Context and then Status = Root then
                  --  This is the root-view, assign the corresponding context
                  Data.Context := Root_Context;
               end if;

               --  Create the view, needed to be able to reference it if it is
               --  an aggregate project as it becomes the new View_Context.

               Data.Context_View := Definition.Weak (Context_View);
               Data.Status       := Status;

               View := Register_View (Data);
            end;

            declare
               Data : constant Definition.Ref := Definition.Get_RW (View);

               procedure Push
                 (Path_Name   : GPR2.Path_Name.Object;
                  Project     : GPR2.Project.Import.Object;
                  Is_Extended : Boolean := False);
               --  Record a new project as seen and record path

               procedure Pop;
               --  Remove last record pushed

               function Is_Limited (Item : Path_Name.Object) return Boolean is
                 (Data.Trees.Project.Imports.Element (Item).Is_Limited);

               ---------
               -- Pop --
               ---------

               procedure Pop is
                  Last : constant Path_Name.Object :=
                           Project_Stack.Last_Element;
               begin
                  if not Sets (Last).Extended and then Is_Limited (Last) then
                     Limited_Count := Limited_Count - 1;
                  end if;

                  Project_Stack.Delete_Last;
                  Sets.Delete (Last);
               end Pop;

               ----------
               -- Push --
               ----------

               procedure Push
                 (Path_Name   : GPR2.Path_Name.Object;
                  Project     : GPR2.Project.Import.Object;
                  Is_Extended : Boolean := False) is
               begin
                  if not Is_Extended and then Is_Limited (Path_Name) then
                     Limited_Count := Limited_Count + 1;
                  end if;

                  Sets.Insert
                    (Path_Name, Recursive_Load.Data'(Project, Is_Extended));
                  Project_Stack.Append (Path_Name);
               end Push;

            begin
               --  Now load all imported projects. If we are parsing the root
               --  project or an aggregate project then the context view become
               --  this project.

               for Project of Data.Trees.Imports loop
                  declare
                     Is_Limited : constant Boolean :=
                                    Data.Trees.Project.Imports.Element
                                      (Project.Path_Name).Is_Limited;
                  begin
                     if Recursive_Load.Filename = Project.Path_Name then
                        --  We are importing the root-project

                        if not Is_Limited and then Limited_Count = 0 then
                           Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "circular dependency detected",
                                 Sets.Element
                                   (Project_Stack.First_Element).Project));

                           Add_Paths_Messages;

                           --  Then finally add current project which is
                           --  the root of the circularity.

                           Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "imports " & Project.Path_Name.Value,
                                 Data.Trees.Project.Imports.Element
                                   (Project.Path_Name)));

                           Circularities := True;
                        end if;

                     elsif Sets.Contains (Project.Path_Name) then
                        --  We are importing a project already imported

                        if not Is_Limited and then Limited_Count = 0 then
                           Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "circular dependency detected",
                                 Data.Trees.Project.Imports.Element
                                        (Project.Path_Name)));

                           Add_Paths_Messages;

                           Circularities := True;
                        end if;

                     elsif Starting_From.Is_Defined
                       and then Starting_From.Path_Name = Project.Path_Name
                     then
                        --  We are importing Starting_From which is an
                        --  aggregate project taken as root project.

                        if not Is_Limited and then Limited_Count = 0 then
                           Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "imports " & Project.Path_Name.Value,
                                 Data.Trees.Project.Imports.Element
                                   (Project.Path_Name)));

                           Add_Paths_Messages;

                           Circularities := True;
                        end if;

                     else
                        Push
                          (Project.Path_Name,
                           Data.Trees.Project.Imports.Element
                             (Project.Path_Name));

                        Data.Imports.Insert
                          (Project.Name,
                           Internal
                             (Project.Path_Name,
                              Status    => Imported,
                              Parent    => GPR2.Project.View.Undefined));

                        Pop;
                     end if;
                  end;
               end loop;

               --  Load the extended project if any

               if Data.Trees.Project.Has_Extended then
                  declare
                     Path_Name : constant GPR2.Path_Name.Object :=
                                   Create
                                     (Data.Trees.Project.Extended.Path_Name
                                      .Name,
                                      Search_Paths
                                        (Filename, Self.Search_Paths));

                  begin
                     if Path_Name.Exists then
                        Push (Path_Name, Data.Trees.Project.Extended, True);

                        Data.Extended :=
                          Internal
                            (Path_Name,
                             Status    => Extended,
                             Parent    => View);

                        Pop;

                     else
                        Add_Paths_Messages;
                        Messages.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Message => "extended project file "
                              & String (Path_Name.Name)
                              & " not found",
                              Sloc    => Data.Trees.Project.Extended));
                     end if;
                  end;
               end if;
            end;
         end if;

         return View;
      end Internal;

      ----------
      -- Load --
      ----------

      function Load (Filename : Path_Name.Object) return Definition.Data is

         Paths   : constant Path_Name.Set.Object :=
                     GPR2.Project.Search_Paths (Filename, Self.Search_Paths);
         Project : constant Parser.Project.Object :=
                     Parser.Project.Parse
                       (Filename, Self.Implicit_With, Messages);
         Data    : Definition.Data
                       (Has_Context =>
                          Project.Is_Defined
                            and then
                          (not Context_View.Is_Defined
                           or else Project.Qualifier = K_Aggregate));
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
                           Message => "imported project file "
                                        & String (Import.Path_Name.Name)
                                        & " not found",
                           Sloc    => Import));
                     exit;
                  end if;
               end;
            end loop;
         end if;

         return Data;
      end Load;

   begin
      Circularities := False;

      return Internal (Filename, Status, Starting_From);
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
      Name      : constant Name_Type := Def.Trees.Project.Name;
      Path_Name : constant Name_Type :=
                    Name_Type (Def.Trees.Project.Path_Name.Value);
      View      : Project.View.Object;

      procedure Add_View (Key : Name_Type);
      --  Add view to the Def.Tree.Views with the Key index

      --------------
      -- Add_View --
      --------------

      procedure Add_View (Key : Name_Type) is
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

      Add_View (Path_Name);

      pragma Assert (Definition.Refcount (View) = 3);

      Add_View (Name);

      pragma Assert (Definition.Refcount (View) = 4);

      return View;
   end Register_View;

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
     (Self : Object; Language : Name_Type) return Optional_Name_Type is
   begin
      if Self.Has_Configuration
        and then Self.Conf.Runtime (Language) /= ""
      then
         return Self.Conf.Runtime (Language);

      else
         return "";
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

      Root.Context := Context;

      --  Take missing external values from environment

      Fill_Externals_From_Environment (Root.Context, Root.Externals);

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
      Root : constant Definition.Ref := Definition.Get_RW (Self.Root);

      procedure Set_View
        (View           : Project.View.Object;
         Aggregate_Only : Boolean := False);
      --  Set the context for the given view

      procedure Validity_Check (View : Project.View.Object);
      --  Do validity check on the given view

      function Has_Error return Boolean is
        (Self.Messages.Has_Error);

      --------------
      -- Set_View --
      --------------

      procedure Set_View
        (View           : Project.View.Object;
         Aggregate_Only : Boolean := False)
      is
         use type GPR2.Context.Binary_Signature;

         P_Data        : Definition.Data := Definition.Get_RO (View).all;
         Old_Signature : constant GPR2.Context.Binary_Signature :=
                           P_Data.Signature;
         New_Signature : constant GPR2.Context.Binary_Signature :=
                           Root.Context.Signature (P_Data.Externals);
         Context       : constant GPR2.Context.Object := View.Context;
         Paths         : Path_Name.Set.Object;
      begin
         Parser.Project.Process
           (P_Data.Trees.Project,
            Self,
            Context,
            View,
            P_Data.Attrs,
            P_Data.Vars,
            P_Data.Packs,
            P_Data.Types);

         --  If an aggregate project and an attribute external is defined then
         --  remove the dependency on the corresponding externals.

         if View.Qualifier = K_Aggregate then
            for C in P_Data.Attrs.Iterate (Name => PRA.External) loop
               declare
                  E : constant Name_Type :=
                        Name_Type (P_Data.Attrs (C).Index.Text);
                  P : Containers.Name_Type_List.Cursor :=
                        P_Data.Externals.Find (E);
               begin
                  if Containers.Name_Type_List.Has_Element (P) then
                     P_Data.Externals.Delete (P);
                  end if;
               end;
            end loop;
         end if;

         --  Now we can record the aggregated projects based on the possibly
         --  new Project_Files attribute value. This attribute may be set
         --  depending on the parsing of the imported projects.

         if View.Qualifier in Aggregate_Kind then
            P_Data.Aggregated.Clear;

            --  Pathname for Project_Files projects are relative to the
            --  aggregate project only.

            Paths.Append (View.Path_Name);

            for Project of P_Data.Attrs.Element (PRA.Project_Files).Values loop
               declare
                  Pathname : constant Path_Name.Object :=
                               Create (Name_Type (Project.Text), Paths);
               begin
                  if Pathname = View.Path_Name then
                     --  We are loading recursively the aggregate project

                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "project cannot aggregate itself "
                           & String (Pathname.Base_Name),
                           Project));

                  elsif P_Data.Aggregated.Contains
                    (Name_Type (Pathname.Value))
                  then
                     --  Duplicate in the project_files attribute

                     if Aggregate_Only then
                        Self.Messages.Append
                          (Message.Create
                             (Message.Warning,
                              "duplicate aggregated project "
                              & String (Pathname.Base_Name),
                              Project));
                     end if;

                  else
                     declare
                        Ctx           : GPR2.Context.Object;
                        Messages      : Log.Object;
                        Circularities : Boolean;
                        A_View        : constant GPR2.Project.View.Object :=
                                          Recursive_Load
                                            (Self          => Self,
                                             Filename      => Pathname,
                                             Context_View  => View,
                                             Status        => Aggregated,
                                             Root_Context  => Ctx,
                                             Messages      => Messages,
                                             Circularities => Circularities,
                                             Starting_From => View);
                     begin
                        --  If there was error messages during the parsing of
                        --  the aggregated project, just return now.

                        if Messages.Has_Error or else Circularities then
                           if Circularities then
                              Self.Messages.Append
                                (Message.Create
                                   (Message.Error,
                                    "circular dependency detected",
                                    Project));
                           end if;

                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "aggregate " & Project.Text,
                                 Project));

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
                  end if;
               end;
            end loop;

            --  And finaly also record the External definition if any into
            --  the aggregate project context.

            for C in P_Data.Attrs.Iterate (PRA.External) loop
               declare
                  use all type PRA.Value_Kind;

                  External : constant Attribute.Object := P_Data.Attrs (C);
               begin
                  --  Check for the validity of the external attribute here
                  --  as the validity check will come after it is fully
                  --  loaded/resolved.
                  if External.Kind = Single then
                     P_Data.A_Context.Include
                       (Name_Type (External.Index.Text), External.Value.Text);
                  end if;
               end;
            end loop;
         end if;

         if not Has_Error then
            P_Data.Signature := New_Signature;

            --  Let's compute the project kind if needed. A project without
            --  an explicit qualifier may actually be a library project if
            --  Library_Name, Library_Kind is declared.

            P_Data.Kind := P_Data.Trees.Project.Qualifier;

            if P_Data.Kind = K_Standard then
               if P_Data.Attrs.Contains (PRA.Library_Name)
                 and then
                   P_Data.Attrs.Element (PRA.Library_Name).Value.Text /= ""
                 and then
                   P_Data.Attrs.Contains (PRA.Library_Dir)
               then
                  P_Data.Kind := K_Library;
               end if;
            end if;

            Copy_Definition (Definition.Get (View), P_Data);

            --  Signal project change only if we have different and non default
            --  signature. That is if there is at least some external used
            --  otherwise the project is stable and won't change.

            if Old_Signature /= New_Signature
              and then P_Data.Signature /= GPR2.Context.Default_Signature
              and then Changed /= null
            then
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
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & String (A.Name.Text)
                     & """ cannot have index",
                     A));
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
                        "package " & String (P.Name) & " cannot be used in "
                        & Image (P_Kind),
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
                  if not PRA.Get (Q_Name).Is_Allowed_In (P_Kind) then
                     --  for backward compatibility, emit warnings
                     Self.Messages.Append
                       (Message.Create
                          (Message.Warning,
                           "attribute """ & String (A.Name.Text)
                           & """ cannot be used in " & Image (P_Kind),
                           A));
                  end if;

                  Check_Def (PRA.Get (Q_Name), A);
               end if;
            end;
         end loop;

         --  Check Library_Version attribute format

         declare
            procedure Check_Shared_Lib (PV : Project.View.Object);
            --  Check that shared library project does not have in imports
            --  static library or standard projects.

            procedure Check_Directory
              (Attr_Name     : Name_Type;
               Human_Name    : String;
               Get_Directory : not null access function
                 (Self : Project.View.Object) return Path_Name.Object);
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
                 (Self : Project.View.Object) return Path_Name.Object) is
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
                              '"' & PN.Relative_Path
                                      (Self.Root_Project.Path_Name).Value
                              & """ cannot relocate absolute "
                              & (if Human_Name = ""
                                 then ""
                                 else Human_Name & ' ')
                              & "directory",
                              Sloc => AV));
                     end if;
                  end;
               end if;
            end Check_Directory;

            ----------------------
            -- Check_Shared_Lib --
            ----------------------

            procedure Check_Shared_Lib (PV : Project.View.Object) is
               P_Data : constant Definition.Const_Ref :=
                          Definition.Get_RO (PV);
            begin
               for Imp of P_Data.Imports loop
                  if Imp.Kind = K_Abstract then
                     --  Check imports further in recursion

                     Check_Shared_Lib (Imp);

                  elsif not Imp.Is_Library then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "shared library project """ & String (View.Name)
                           & """ cannot import project """
                           & String (Imp.Name)
                           & """ that is not a shared library project",
                           P_Data.Trees.Project.Imports.Element
                             (Imp.Path_Name)));

                  elsif Imp.Is_Static_Library
                    and then (not PV.Is_Library
                              or else PV.Library_Standalone /= Encapsulated)
                  then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "shared library project """ & String (View.Name)
                           & """ cannot import static library project """
                           & String (Imp.Name) & '"',
                           P_Data.Trees.Project.Imports.Element
                             (Imp.Path_Name)));

                  elsif Imp.Is_Shared_Library
                    and then PV.Is_Library
                    and then PV.Library_Standalone = Encapsulated
                  then
                     Self.Messages.Append
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
            end Check_Shared_Lib;

         begin
            if View.Is_Library and then View.Is_Shared_Library then
               if View.Check_Attribute (PRA.Library_Version, Result => Attr)
                 and then not View.Tree.Is_Windows_Target
                 --  Library_Version attribute has no effect on Windows
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
                         (Lib_Ver (Lib_Ver'First + Lib_Fn'Length
                                   .. Lib_Ver'Last),
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

               if Self.Check_Shared_Lib then
                  Check_Shared_Lib (View);
               end if;
            end if;

            if View.Kind in K_Standard | K_Library | K_Aggregate_Library
              and then Check_Object_Dir_Exists
            then
               Check_Directory
                 (PRA.Object_Dir, "object",
                  Project.View.Object_Directory'Access);
            end if;

            if View.Kind = K_Standard
              and then Check_Exec_Dir_Exists
            then
               Check_Directory
                 (PRA.Exec_Dir, "exec",
                  Project.View.Executable_Directory'Access);
            end if;

            if View.Is_Library then
               Check_Directory
                 (PRA.Library_Dir, "library",
                  Project.View.Library_Directory'Access);

               Check_Directory
                 (PRA.Library_Ali_Dir, "library ALI",
                  Project.View.Library_Ali_Directory'Access);

               Check_Directory
                 (PRA.Library_Src_Dir, "",
                  Project.View.Library_Src_Directory'Access);
            end if;
         end;
      end Validity_Check;

   begin
      --  Now the first step is to set the configuration project view if any
      --  and to create the runtime project if possible.

      if Self.Has_Configuration then
         Set_View (Self.Conf.Corresponding_View);

         Self.Runtime := Create_Runtime_View (Self);
      end if;

      --  First ensure that we now load all projects inside aggregate library

      for View in Self.Iterate
        (Filter => (F_Aggregate | F_Aggregate_Library => True,
                    others                            => False))
      loop
         Set_View (Element (View), Aggregate_Only => True);
      end loop;

      --  Propagate the change in the project Tree. That is for each project in
      --  the tree we need to update the corresponding view. We do not handle
      --  the aggregated projects here. Those projects are handled specifically
      --  in Set_View. This is needed as parsing the aggregate project may
      --  change the Project_Files attribute and so the actual aggregated
      --  project. So we cannot use the current aggregated project list.

      if not Has_Error then
         for View of Self loop
            Set_View (View);
         end loop;

         --  We now have an up-to-date tree, do some validity checks if there
         --  is no issue detected yet.

         for View of Self loop
            Validity_Check (View);
         end loop;
      end if;

      if Has_Error then
         raise Project_Error
           with Self.Root.Path_Name.Value & " semantic error";
      end if;
   end Set_Context;

   ------------
   -- Target --
   ------------

   function Target (Self : Object) return Name_Type is
      TA : Attribute.Object;
   begin
      if Self.Has_Configuration
        and then Self.Conf.Target /= ""
      then
         return Self.Conf.Target;

      elsif Self.Root /= View.Undefined
        and then Self.Root_Project.Check_Attribute (PRA.Target, Result => TA)
      then
         return Name_Type (TA.Value.Text);

      elsif Self.Has_Configuration
        and then Self.Conf.Corresponding_View.Check_Attribute
                   (PRA.Target, Result => TA)
      then
         return Name_Type (TA.Value.Text);

      else
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

      Self.Units.Clear;
      Self.Sources.Clear;
      Self.Messages.Clear;
      Self.Search_Paths.Clear;
      Self.Views.Clear;
      Self.Views_Set.Clear;
   end Unload;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources (Self : Object; Stop_On_Error : Boolean := True) is
   begin
      for V of Self.Views_Set loop
         Definition.Get (V).Update_Sources (V, Stop_On_Error);
      end loop;
   end Update_Sources;

   --------------
   -- View_For --
   --------------

   function View_For
     (Self         : Object;
      Name         : Name_Type;
      Context_View : View.Object) return View.Object
   is
      View : Project.View.Object := Self.Get (Name, Context_View);
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

   Definition.Register := Register_View'Access;
end GPR2.Project.Tree;
