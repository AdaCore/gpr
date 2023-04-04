--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Holders;
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

pragma Warnings (Off, ".* is not referenced");
with GPR2.Build.Source.Sets;
pragma Warnings (On, ".* is not referenced");

with GPR2.Project.Parser.Create;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Import.Set;
with GPR2.Project.Parser;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Tree.View_Builder;
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

   package PRP renames Project.Registry.Pack;
   package IDS renames GPR2.View_Ids;

   Is_Windows_Host : constant Boolean :=
                       GNATCOLL.OS.Constants.OS = GNATCOLL.OS.Windows
                         with Warnings => Off;

   Wildcards       : constant Strings.Maps.Character_Set :=
                       Strings.Maps.To_Set ("?*[]");
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

   procedure Lint
      (Self : in out Object;
       Msg  : String;
       Sloc : Source_Reference.Object'Class);
   --  Append a lint message to Self.Messages

   function Register_View
     (Def : in out Definition.Data) return Project.View.Object
     with Post => Register_View'Result.Is_Defined;
   --  Register view definition in the Tree and return the View object

   procedure Set_Context
     (Self    : in out Object;
      Changed : access procedure (Project : View.Object) := null);
   --  Update project tree with updated context

   function Get_Context
     (View : Project.View.Object) return GPR2.Context.Object
   is
     (View.Tree.Context (View.Context));
   --  Returns context of the project view

   function Are_Sources_Loaded (Tree : Object) return Boolean is
     (Tree.Sources_Loaded);

   type Iterator is new Project_Iterator.Forward_Iterator with record
      Views : Project_View_Store.Vector;
      Tree  : access Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Recursive_Load
     (Self          : in out Object;
      Root_Project  : Project_Descriptor;
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

   procedure Update_Context
     (Context     : in out GPR2.Context.Object;
      Externals   : Containers.Name_List;
      Environment : GPR2.Environment.Object);
   --  For all externals in Externals, if external is not already present in
   --  the context, fetch its value from the environment and insert it into the
   --  context.

   procedure Update_Project_Search_Path_From_Config
     (Self : in out Object;
      Conf : Project.Configuration.Object)
     with Pre => Conf.Is_Defined;
   --  Update project search path with directories relevant to

   procedure Process_Subtree
     (Self             : Object;
      View             : Project.View.Object := Project.View.Undefined;
      Externally_Built : Boolean := False;
      Do_Action        : not null access procedure
                           (View : Project.View.Object))
     with Pre => Self.Is_Defined;
   --  Call Do_Action for View & View's subtree

   procedure Prepend_Search_Paths
     (Self : in out Object; Dir : GPR2.Path_Name.Object)
     with Pre => Dir.Is_Defined;

   procedure Append_Search_Paths
     (Self : in out Object; Dir : GPR2.Path_Name.Object)
     with Pre => Dir.Is_Defined;

   procedure Update_Search_Paths (Self : in out Object);
   --  Update Self.Search_Paths after Prepend, Append & Environment changes

   ---------------------
   -- Add_Tool_Prefix --
   ---------------------

   function Add_Tool_Prefix
     (Self      : Object;
      Tool_Name : Name_Type) return Name_Type
   is

      Not_Defined : constant Optional_Name_Type := " ";
      --  Value returned when GNAT_Compilers_Prefix cannot find Compiler
      --  Driver in View.

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
         if View.Is_Defined
           and then View.Has_Package (PRP.Compiler)
           and then View.Has_Languages
         then
            for Language of View.Languages loop
               declare
                  Index  : constant Attribute_Index.Object :=
                             Attribute_Index.Create (Language.Text);
                  Attr   : constant Attribute.Object :=
                             (if View.Has_Attribute (PRA.Compiler.Driver,
                                                     Index)
                              then View.Attribute (PRA.Compiler.Driver, Index)
                              else Attribute.Undefined);
                  Driver : constant String :=
                             (if Attr.Is_Defined
                              then String
                                (Path_Name.Create_File
                                   (Filename_Type (Attr.Value.Text)).Base_Name)
                              else "");
               begin
                  if Driver'Length > 2 then
                     declare
                        subtype Last_3_Chars_Range is Positive range
                          Driver'Last - 2 .. Driver'Last;
                        Last_3_Chars : constant String :=
                                         (if Is_Windows_Host
                                          then Characters.Handling.To_Lower
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
      --  Try to find prefix from root project compiler package or from its
      --  associated configuration project.

   begin
      if Prefix_From_Root = Not_Defined then
         --  Use Target as prefix if not defined in project and no
         --  configuration file was loaded.

         return Name_Type (String (Self.Target) & "-" & String (Tool_Name));

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

   -------------------------
   -- Append_Search_Paths --
   -------------------------

   procedure Append_Search_Paths
     (Self : in out Object; Dir : GPR2.Path_Name.Object) is
   begin
      Self.Search_Paths.Appended.Append (Dir);
      Self.Update_Search_Paths;
   end Append_Search_Paths;

   ------------------------
   -- Artifacts_Database --
   ------------------------

   function Artifacts_Database
     (Self : Object) return Build.Tree_Db.Object_Access is
   begin
      if not Self.Tree_Db.Is_Defined then
         Self.Self.Tree_Db.Load (Self);
      end if;

      return Self.Tree_Db.Reference;
   end Artifacts_Database;

   --------------------
   -- Artifacts_Dir --
   --------------------

   function Artifacts_Dir (Self : Object) return Path_Name.Object is
   begin
      if Self.Root_Project.Kind in With_Object_Dir_Kind then
         return Self.Root_Project.Object_Directory;
      else
         return Self.Root_Project.Apply_Root_And_Subdirs (PRA.Object_Dir);
      end if;
   end Artifacts_Dir;
   --  Object_Directory has a precondition to prevent its use when the view
   --  don't expect one (aggregate, abstract). But Apply_Root_And_Subdirs
   --  doesn't, and object_directory will default to Project_Dir in such case.

   -------------------
   -- Clear_Sources --
   -------------------

   procedure Clear_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined)
   is
   begin
      Self.Self.Tree_Db.Unload;
   end Clear_Sources;

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
      Views_Curs : constant View.Set.Set.Cursor :=
                     Self.Views_Set.Find
                       (Project_View_Store.Element (Position.Current));
      Ref        : View.Set.Set.Constant_Reference_Type renames
                     Self.Views_Set.Constant_Reference (Views_Curs);
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

      procedure Add_Attribute (Name : Q_Attribute_Id; Value : Value_Type);
      --  Add builtin attribute into Data.Attrs

      -------------------
      -- Add_Attribute --
      -------------------

      procedure Add_Attribute (Name : Q_Attribute_Id; Value : Value_Type) is
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

      RTD := CV.Attribute (Name  => PRA.Runtime_Dir,
                           Index => Attribute_Index.Create (Ada_Language));

      if RTD.Is_Defined and then RTD.Value.Text /= "" then
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

         Add_Attribute (PRA.Object_Dir, RTD.Value.Text & DS & "adalib");

         --  The only language supported is Ada

         Add_Attribute (PRA.Languages, "ada");

         Data.Tree      := Self.Self;
         Data.Kind      := K_Standard;
         Data.Path      := Path_Name.Create_Directory
                             (Filename_Type (RTD.Value.Text));
         Data.Is_Root   := True;
         Data.Unique_Id := GPR2.View_Ids.Runtime_View_Id;

         Data.Trees.Project := Project.Parser.Create
           (Name      => Name (PRA.Runtime.Attr),
            File      => RTF,
            Qualifier => K_Standard);

         return Result : View.Object := Register_View (Data) do
            --  If we simply return Register_View (Data) the reference counter
            --  will be one more than should be, see T709-001.
            --  It is not actual for now because line below is added after the
            --  T709-001 bug detected.

            --  ??? We shouldn't consider the runtime view as a root view, but
            --  instead add it as implicitly limited withed project for all
            --  views that have Ada sources.

            Definition.Get_RW (Result).Root_Views.Append
              (View_Ids.Runtime_View_Id);
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
      return Project_View_Store.Element (Position.Current);
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

   ------------------
   -- Find_Project --
   ------------------

   function Find_Project
     (Self       : Object;
      Base_Name  : Simple_Name) return Path_Name.Object
   is
      GPR_Name    : constant Simple_Name :=
                      Project.Ensure_Extension (Base_Name);

   begin
      for V in Self.Iterate
        (Status => (Project.S_Externally_Built => Indeterminate))
      loop
         declare
            View : constant Project.View.Object := Project.Tree.Element (V);
         begin
            if View.Path_Name.Simple_Name = GPR_Name then
               return View.Path_Name;
            end if;
         end;
      end loop;

      return Path_Name.Undefined;
   end Find_Project;

   -----------
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is
   begin
      if Iter.Views.Is_Empty then
         return No_Element;
      else
         return (Current => Iter.Views.First,
                 Tree    => Iter.Tree);
      end if;
   end First;

   ---------------------
   -- For_Each_Source --
   ---------------------

   procedure For_Each_Source
     (Self             : Object;
      View             : Project.View.Object := Project.View.Undefined;
      Action           : access procedure (Source : Build.Source.Object);
      Language         : Language_Id := No_Language;
      Externally_Built : Boolean := False) is

      procedure Do_Action (View : Project.View.Object)
        with Pre => View.Is_Defined;
      --  Call Action for all View's source object having a valid language

      ---------------
      -- Do_Action --
      ---------------

      procedure Do_Action (View : Project.View.Object) is
      begin
         for S of View.Sources loop
            if Language = No_Language or else S.Language = Language then
               Action (S);
            end if;
         end loop;
      end Do_Action;

   begin
      Process_Subtree (Self, View, Externally_Built, Do_Action'Access);
   end For_Each_Source;

   --------------
   -- Get_View --
   --------------

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
      return Project_View_Store.Has_Element (Position.Current);
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

   ---------------------
   -- Has_Src_Subdirs --
   ---------------------

   function Has_Src_Subdirs (Self : Object) return Boolean is
   begin
      return Self.Src_Subdirs /= Null_Unbounded_String;
   end Has_Src_Subdirs;

   -----------------
   -- Instance_Of --
   -----------------

   function Instance_Of
     (Self        : Object;
      Instance_Id : GPR2.View_Ids.View_Id) return View.Object is
   begin
      return Self.View_Ids.Element (Instance_Id);
   end Instance_Of;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (Position : Cursor) return Boolean is
      V : constant View.Object := Element (Position);
   begin
      return Position.Tree.Root = V;
   end Is_Root;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Self   : Object;
      Kind   : Iterator_Control := Default_Iterator;
      Filter : Filter_Control   := Default_Filter;
      Status : Status_Control   := Default_Status)
      return Project_Iterator.Forward_Iterator'Class
   is
      Iter : Iterator;
      --  The returned object

      Seen : GPR2.Project.View.Set.Object;
      --  Keep track of already seen projects. Better than using the P vector
      --  which is not efficient when checking if an element exists.

      P_Set    : GPR2.Project.View.Set.Object;
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
              (Status (S_Externally_Built),
               View.Is_Externally_Built) in True | Indeterminate
            then
               declare
                  Qualifier : constant Project_Kind := View.Kind;
               begin
                  --  Check if it corresponds to the current filter
                  if (Qualifier = K_Library and then Filter (F_Library))
                    or else
                     (Qualifier = K_Standard and then Filter (F_Standard))
                    or else
                     (Qualifier = K_Abstract and then Filter (F_Abstract))
                    or else
                     (Qualifier = K_Aggregate and then Filter (F_Aggregate))
                    or else
                     (Qualifier = K_Aggregate_Library
                      and then Filter (F_Aggregate_Library))
                  then
                     Iter.Views.Append (View);
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
               if Kind (I_Recursive) then
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
            if Kind (I_Recursive) then
               For_Project (I);
            else
               Append (I);
            end if;
         end loop;

         for I of Definition.Get_RO (View).Limited_Imports loop
            if Kind (I_Recursive) then
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

            if Kind (I_Imported) or else Kind (I_Recursive) then
               For_Imports (View);
            end if;

            --  Handle extended if any

            if Kind (I_Extended) then
               declare
                  Data : constant Definition.Const_Ref :=
                           Definition.Get_RO (View);
               begin
                  if Data.Extended_Root.Is_Defined then
                     if Kind (I_Recursive) then
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

            if Kind (I_Aggregated) then
               For_Aggregated (View);
            end if;
         end if;
      end For_Project;

   begin
      Iter := (Project_View_Store.Empty, Self.Self);
      For_Project (Self.Root);

      if Self.Has_Runtime_Project and then Kind (I_Runtime) then
         For_Project (Self.Runtime);
      end if;

      if Self.Has_Configuration and then Kind (I_Configuration) then
         For_Project (Self.Configuration.Corresponding_View);
      end if;

      return Iter;
   end Iterate;

   ----------
   -- Lint --
   ----------

   procedure Lint
     (Self : in out Object;
      Msg  : String;
      Sloc : Source_Reference.Object'Class) is
   begin
      Self.Messages.Append (Message.Create (Message.Lint, Msg, Sloc));
   end Lint;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self             : in out Object;
      Root_Project     : Project_Descriptor;
      Context          : GPR2.Context.Object;
      Config           : PC.Object                 := PC.Undefined;
      Build_Path       : Path_Name.Object          := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type        := No_Name;
      Src_Subdirs      : Optional_Name_Type        := No_Name;
      Check_Shared_Lib : Boolean                   := True;
      Absent_Dir_Error : Error_Level               := Warning;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Pre_Conf_Mode    : Boolean                   := False;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference;
      Environment      : GPR2.Environment.Object :=
                           GPR2.Environment.Process_Environment)
   is
      Gpr_Path      : Path_Name.Object;
      Root_Context  : GPR2.Context.Object := Context;
      Def           : Definition.Ref;

   begin
      Self.Self := Self'Unchecked_Access;

      Self.Set_Environment (Environment);

      --  Let ada or gpr parser use this reader

      Self.File_Reader_Ref := File_Reader;

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
            Update_Context (Root_Context, Config.Externals, Environment);
         end if;

         Definition.Bind_Configuration_To_Tree (Self.Conf, Self.Self);

         declare
            C_View : GPR2.Project.View.Object := Self.Conf.Corresponding_View;
            P_Data : constant Definition.Ref := Definition.Get_RW (C_View);
         begin
            --  Set and record the tree now, needed for the parsing

            P_Data.Tree := Self.Self;
            P_Data.Is_Root := True;

            --  Parse the configuration project, no need for full/complex
            --  parsing as a configuration project is a simple project no
            --  with clauses.

            GPR2.Project.Parser.Process
              (P_Data.Trees.Project,
               Self,
               Root_Context,
               C_View,
               Ext_Conf_Mode => True);

            if Self.Conf.Is_Defined then
               Update_Project_Search_Path_From_Config
                 (Self, Self.Conf);
            end if;

            pragma Assert
              (P_Data.Kind = K_Configuration,
               "expected K_Configuration, found : " & P_Data.Kind'Img);
         end;
      end if;

      Self.Build_Path       := Build_Path;
      Self.Subdirs          := To_Unbounded_String (String (Subdirs));
      Self.Src_Subdirs      := To_Unbounded_String (String (Src_Subdirs));
      Self.Check_Shared_Lib := Check_Shared_Lib;
      Self.Implicit_With    := Implicit_With;
      Self.Absent_Dir_Error := Absent_Dir_Error;
      Self.Pre_Conf_Mode    := Pre_Conf_Mode;

      if Root_Project.Kind = Project_Definition then
         Gpr_Path := Root_Project.Data.Trees.Project.Path_Name;

      elsif Root_Project.Path.Has_Dir_Name then
         Gpr_Path := Root_Project.Path;

      else
         --  If project directory still not defined, search it in full set
         --  of search paths.

         Gpr_Path := Create
           (Root_Project.Path.Name, Self.Search_Paths.All_Paths);

         if not Build_Path.Is_Defined then
            Self.Build_Path := Path_Name.Create_Directory
              (Filename_Type (Gpr_Path.Dir_Name));
         end if;
      end if;

      --  Add full project path in the message log

      Self.Messages.Append
        (Message.Create
           (Message.Information,
            "Parsing """ & Gpr_Path.Value & """",
            Source_Reference.Create (Gpr_Path.Value, 0, 0)));

      --  Add all search paths into the message log

      declare
         Search_Paths : Unbounded_String;
      begin
         for P of Self.Search_Paths.All_Paths loop
            Append (Search_Paths, GNAT.OS_Lib.Path_Separator & P.Value);
         end loop;
         --  Remove first path separator
         Delete (Search_Paths, 1, 1);

         Self.Messages.Append
           (Message.Create
              (Message.Information,
               "project search path: " & To_String (Search_Paths),
               Source_Reference.Create (Gpr_Path.Value, 0, 0)));
      end;

      Self.Root := Recursive_Load
        (Self,
         Root_Project => (if Root_Project.Kind = Project_Definition
                          then Root_Project
                          else (Project_Path, Gpr_Path)),
         Context      => Root);

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

         if Config.Is_Defined and then Config.Has_Externals
           and then Self.Root.Kind in Aggregate_Kind
         then
            Update_Context
              (Self.Context (Aggregate), Config.Externals, Environment);
         end if;

         Set_Context (Self, Context);

         Definition.Check_Same_Name_Extended (Self.Root);

         if not Self.Pre_Conf_Mode then
            --  We only need those checks if we are not in pre-configuration
            --  stage, otherwise we might have errors if a project references
            --  corresponding attributes from a not yet found project and their
            --  values default to empty ones.
            Definition.Check_Aggregate_Library_Dirs (Self.Root);
            Definition.Check_Package_Naming (Self.Root);
            Definition.Check_Excluded_Source_Dirs (Self.Root);
         end if;
      end if;

      if not Self.Pre_Conf_Mode and then Self.Messages.Has_Error then
         raise Project_Error
           with Gpr_Path.Value & ": fatal error, cannot load the project tree";
      end if;
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self             : in out Object;
      Filename         : Path_Name.Object;
      Context          : GPR2.Context.Object;
      Config           : PC.Object                 := PC.Undefined;
      Build_Path       : Path_Name.Object          := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type        := No_Name;
      Src_Subdirs      : Optional_Name_Type        := No_Name;
      Check_Shared_Lib : Boolean                   := True;
      Absent_Dir_Error : Error_Level               := Warning;
      Implicit_With    : GPR2.Path_Name.Set.Object :=
                           GPR2.Path_Name.Set.Empty_Set;
      Pre_Conf_Mode    : Boolean                   := False;
      File_Reader      : GPR2.File_Readers.File_Reader_Reference :=
                           GPR2.File_Readers.No_File_Reader_Reference;
      Environment      : GPR2.Environment.Object :=
                           GPR2.Environment.Process_Environment) is
   begin
      if not Filename.Is_Directory then
         GPR2.Project.Parser.Clear_Cache;

         Self.Load
           (Project_Descriptor'(Project_Path, Filename),
            Context          => Context,
            Config           => Config,
            Build_Path       => Build_Path,
            Subdirs          => Subdirs,
            Src_Subdirs      => Src_Subdirs,
            Check_Shared_Lib => Check_Shared_Lib,
            Absent_Dir_Error => Absent_Dir_Error,
            Implicit_With    => Implicit_With,
            Pre_Conf_Mode    => Pre_Conf_Mode,
            File_Reader      => File_Reader,
            Environment      => Environment);

         GPR2.Project.Parser.Clear_Cache;
      else
         --  Load an empty project in directory "Filename"

         View_Builder.Load
           (Self,
            View_Builder.Create (Filename, "Default"),
            Context          => Context,
            Config           => Config,
            Build_Path       => Build_Path,
            Subdirs          => Subdirs,
            Src_Subdirs      => Src_Subdirs,
            Check_Shared_Lib => Check_Shared_Lib,
            Absent_Dir_Error => Absent_Dir_Error,
            Implicit_With    => Implicit_With,
            Pre_Conf_Mode    => Pre_Conf_Mode,
            File_Reader      => File_Reader,
            Environment      => Environment);
      end if;
   end Load;

   -------------------
   -- Load_Autoconf --
   -------------------

   procedure Load_Autoconf
     (Self              : in out Object;
      Root_Project      : Project_Descriptor;
      Context           : GPR2.Context.Object;
      Build_Path        : Path_Name.Object          := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type        := No_Name;
      Src_Subdirs       : Optional_Name_Type        := No_Name;
      Check_Shared_Lib  : Boolean                   := True;
      Absent_Dir_Error  : Error_Level               := Warning;
      Implicit_With     : GPR2.Path_Name.Set.Object :=
                            GPR2.Path_Name.Set.Empty_Set;
      Target            : Optional_Name_Type        := No_Name;
      Language_Runtimes : Containers.Lang_Value_Map :=
                            Containers.Lang_Value_Maps.Empty_Map;
      Base              : GPR2.KB.Object            := GPR2.KB.Undefined;
      Config_Project    : GPR2.Path_Name.Object     :=
                            GPR2.Path_Name.Undefined;
      File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                            GPR2.File_Readers.No_File_Reader_Reference;
      Environment      : GPR2.Environment.Object :=
                            GPR2.Environment.Process_Environment)
       is separate;

   procedure Load_Autoconf
     (Self              : in out Object;
      Filename          : Path_Name.Object;
      Context           : GPR2.Context.Object;
      Build_Path        : Path_Name.Object        := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type      := No_Name;
      Src_Subdirs       : Optional_Name_Type      := No_Name;
      Check_Shared_Lib  : Boolean                 := True;
      Absent_Dir_Error  : Error_Level             := Warning;
      Implicit_With     : GPR2.Path_Name.Set.Object :=
                            GPR2.Path_Name.Set.Empty_Set;
      Target            : Optional_Name_Type      := No_Name;
      Language_Runtimes : Containers.Lang_Value_Map :=
                            Containers.Lang_Value_Maps.Empty_Map;
      Base              : GPR2.KB.Object          := GPR2.KB.Undefined;
      Config_Project    : GPR2.Path_Name.Object   := GPR2.Path_Name.Undefined;
      File_Reader       : GPR2.File_Readers.File_Reader_Reference :=
                            GPR2.File_Readers.No_File_Reader_Reference;
      Environment       : GPR2.Environment.Object :=
                            GPR2.Environment.Process_Environment) is
   begin
      if not Filename.Is_Directory then
         Self.Load_Autoconf
           (Root_Project      => (Project_Path, Filename),
            Context           => Context,
            Build_Path        => Build_Path,
            Subdirs           => Subdirs,
            Src_Subdirs       => Src_Subdirs,
            Check_Shared_Lib  => Check_Shared_Lib,
            Absent_Dir_Error  => Absent_Dir_Error,
            Implicit_With     => Implicit_With,
            Target            => Target,
            Language_Runtimes => Language_Runtimes,
            Base              => Base,
            Config_Project    => Config_Project,
            File_Reader       => File_Reader,
            Environment       => Environment);
      else
         View_Builder.Load_Autoconf
           (Self,
            View_Builder.Create (Filename, "Default"),
            Context           => Context,
            Build_Path        => Build_Path,
            Subdirs           => Subdirs,
            Src_Subdirs       => Src_Subdirs,
            Check_Shared_Lib  => Check_Shared_Lib,
            Absent_Dir_Error  => Absent_Dir_Error,
            Implicit_With     => Implicit_With,
            Target            => Target,
            Language_Runtimes => Language_Runtimes,
            Base              => Base,
            Config_Project    => Config_Project,
            File_Reader       => File_Reader,
            Environment       => Environment);
      end if;
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

      --  Copy all configuration message into the main tree's log message list

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

   -----------------------------
   -- Namespace_Root_Projects --
   -----------------------------

   function Namespace_Root_Projects (Self : Object) return View.Set.Object is
   begin
      return Result : View.Set.Object do
         if Self.Root.Kind = K_Aggregate then
            for V of Self.Root.Aggregated loop
               Result.Insert (V);
            end loop;

         else
            Result.Insert (Self.Root);
         end if;
      end return;
   end Namespace_Root_Projects;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor
   is
   begin
      return (Current => Project_View_Store.Next (Position.Current),
              Tree    => Position.Tree);
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
   -- Prepend_Search_Paths --
   --------------------------

   procedure Prepend_Search_Paths
     (Self : in out Object; Dir : GPR2.Path_Name.Object) is
   begin
      Self.Search_Paths.Prepended.Prepend (Dir);
      Self.Update_Search_Paths;
   end Prepend_Search_Paths;

   ---------------------
   -- Process_Subtree --
   ---------------------

   procedure Process_Subtree
     (Self             : Object;
      View             : Project.View.Object := Project.View.Undefined;
      Externally_Built : Boolean := False;
      Do_Action        : not null access procedure
                           (View : Project.View.Object)) is

      Processed : GPR2.Project.View.Set.Object;

      procedure Process (View : Project.View.Object)
        with Pre => View.Is_Defined;
      --  If not yet processed, then Do_Action for view & parse View's subtree

      -------------
      -- Process --
      -------------

      procedure Process (View : Project.View.Object) is
      begin
         if Processed.Contains (View) then
            return;
         end if;

         Processed.Insert (View);

         if Externally_Built or else not View.Is_Externally_Built then
            if View.Qualifier in K_Standard | K_Library then
               Do_Action (View);
            end if;
         end if;

         if View.Qualifier in Aggregate_Kind then
            for Aggregated of View.Aggregated loop
               Process (Aggregated);
            end loop;
         end if;

         if View.Qualifier /= K_Aggregate then
            for Imported of View.Imports loop
               Process (Imported);
            end loop;

            for Limited_Imported of View.Limited_Imports loop
               Process (Limited_Imported);
            end loop;

            if View.Is_Extending then
               Process (View.Extended_Root);
            end if;
         end if;
      end Process;

   begin
      Process ((if View.Is_Defined then View else Self.Root_Project));
   end Process_Subtree;

   --------------------------
   -- Project_Search_Paths --
   --------------------------

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object is
   begin
      return Self.Search_Paths.All_Paths;
   end Project_Search_Paths;

   --------------------
   -- Recursive_Load --
   --------------------

   function Recursive_Load
     (Self          : in out Object;
      Root_Project  : Project_Descriptor;
      Context       : Context_Kind;
      Starting_From : View.Object := View.Undefined) return View.Object
   is

      type Relation_Status is
        (Root,            --  Root project
         Extended,        --  Extended project
         Aggregated,      --  In an aggregate project
         Simple);         --  Import, Limited import or aggregate library

      P_Names     : Containers.Name_Value_Map;
      --  Used to check for duplicate project names possibly in
      --  different (inconsistent naming) filenames.

      Search_Path : Path_Name.Set.Object := Self.Search_Paths.All_Paths;
      PP          : Attribute.Object;

      function Load (Filename : Path_Name.Object) return Definition.Data;
      --  Returns the Data definition for the given project

      function Internal
        (Project     : Project_Descriptor;
         Aggregate   : View.Object;
         Status      : Relation_Status;
         Parent      : View.Object;
         Extends_Ctx : View.Vector.Object) return View.Object;
      --  Internal function doing the actual load of the tree.
      --  Project:   the project to load
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

      procedure Propagate_Aggregate
        (View                 : in out GPR2.Project.View.Object;
         Root                 : GPR2.View_Ids.View_Id;
         Is_Aggregate_Library : Boolean) with Inline;
      --  Make sure that all views in the subtree of View reference the
      --  Aggregate Library (if set), or set their namespace root to Root.
      --
      --  In case of Aggregate Libraries, this is needed if several aggregate
      --  libraries exist in the tree and they reference the same project.
      --
      --  This is also needed for both aggregate and aggregate library cases
      --  if a subproject is withed from several subtrees of the aggregate.

      --------------
      -- Internal --
      --------------

      function Internal
        (Project     : Project_Descriptor;
         Aggregate   : View.Object;
         Status      : Relation_Status;
         Parent      : View.Object;
         Extends_Ctx : View.Vector.Object) return View.Object
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

         Filename  : constant GPR2.Path_Name.Object :=
                       (if Project.Kind = Project_Definition
                        then Project.Data.Trees.Project.Path_Name
                        else Project.Path);
         Id        : constant IDS.View_Id :=
                       IDS.Create
                         (Project_File => Filename,
                          Context      => Context,
                          Extending    => Extending);
         View      : GPR2.Project.View.Object := Self.Get_View (Id);

      begin
         --  If the view is already defined just return it

         if not View.Is_Defined then
            declare
               Data : Definition.Data := (if Project.Kind = Project_Definition
                                          then Project.Data
                                          else Load (Filename));
            begin
               --  If there are parsing errors, do not go further

               if Self.Messages.Has_Error then
                  return View;
               end if;

               --  Compute directory used as project file directory
               --  This is influenced by the Project_Dir parameters used on
               --  load to simulate that a project is in another location.

               Data.Path := Path_Name.Create_Directory
                              (Filename_Type (Filename.Dir_Name));

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

               Update_Context
                 (Self.Context (Context), Data.Externals, Self.Environment);

               --  At this stage even if not complete we can create the view
               --  and register it so that we can have references to it.

               Data.Tree      := Self.Self;
               Data.Context   := Context;
               Data.Is_Root   := Status = Root;
               Data.Unique_Id := Id;

               View := Register_View (Data);

               --  Check if we already have a project recorded with the same
               --  name. Issue an error if it is on a different filename.

               declare
                  Full_Name : constant String :=
                                OS_Lib.Normalize_Pathname
                                  (View.Path_Name.Value);
               begin
                  if P_Names.Contains (View.Name) then
                     if P_Names (View.Name) /= Full_Name then
                        Self.Messages.Append
                          (GPR2.Message.Create
                             (Level   => Message.Error,
                              Message => "duplicate project name """
                                          & String (View.Name) & """ in """
                                          & P_Names (View.Name)
                                          & """",
                              Sloc    => Source_Reference.Create
                                           (Full_Name, 1, 1)));
                     end if;

                  else
                     P_Names.Insert (View.Name, Full_Name);
                  end if;
               end;
            end;

            declare
               Data            : constant Definition.Ref :=
                                   Definition.Get_RW (View);
               New_Extends_Ctx : GPR2.Project.View.Vector.Object :=
                                   Extends_Ctx;
               --  Extends all context for the extended project if any

            begin
               --  Set the root view(s), so the view that is at the top level
               --  of a standalone hierarchy (so either the root project,
               --  or the top-level aggregated projects.
               --
               --  Note that in case of aggregated projects, a view can have
               --  several root views.

               if not Parent.Is_Defined
                 or else View.Kind = K_Aggregate
                 or else Parent.Kind = K_Aggregate
               then
                  --  This is the root project or an aggregated project. This
                  --  creates a new namespace (i.e. root in the subtree)

                  Data.Root_Views.Append (View.Id);

               else
                  Data.Root_Views :=
                    Definition.Get_RO (Parent).Root_Views;

                  Data.Agg_Libraries :=
                    Definition.Get_RO (Parent).Agg_Libraries;

                  if Parent.Kind = K_Aggregate_Library
                    and then Status = Aggregated
                  then
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
                     Temp_View : GPR2.Project.View.Object :=
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

               for Prj of Data.Trees.Imports loop
                  declare
                     Imported_View : GPR2.Project.View.Object;
                     Limited_With  : Boolean := False;

                  begin
                     --  Look for the list of imported projects from the
                     --  extending project to see if we need to substitute
                     --  regular imports with their extended view.

                     Extends_Loop : for Ext of New_Extends_Ctx loop
                        Import_Loop : for Imp of Ext.Imports loop
                           if Imp.Is_Extending
                             and then
                               Imp.Extended_Root.Path_Name = Prj.Path_Name
                           then
                              Imported_View := Imp;
                              exit Extends_Loop;
                           end if;
                        end loop Import_Loop;
                     end loop Extends_Loop;

                     if not Imported_View.Is_Defined then
                        Imported_View :=
                          Internal
                            ((Project_Path, Prj.Path_Name),
                             Aggregate   => GPR2.Project.View.Undefined,
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

                        if Is_Limited (View, Prj.Path_Name) then
                           Limited_With := True;
                        end if;
                     end if;

                     if Data.Limited_Imports.Contains (Prj.Name)
                       or else Data.Imports.Contains (Prj.Name)
                     then
                        --  Do not try to insert the project below as it is
                        --  a duplicate import. The error will be reported
                        --  later, so here we do nothing.
                        null;

                     else
                        if Limited_With then
                           Data.Limited_Imports.Insert
                             (Prj.Name, Imported_View);
                        else
                           Data.Imports.Insert (Prj.Name, Imported_View);
                        end if;
                     end if;

                     Data.Closure.Include (Prj.Name, Imported_View);

                     for C in
                       Definition.Get_RO (Imported_View).Closure.Iterate
                     loop
                        Data.Closure.Include
                          (Definition.Project_View_Store.Key (C),
                           Definition.Project_View_Store.Element (C));
                     end loop;
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
                                         ((Project_Path,
                                          Data.Trees.Extended.Path_Name),
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

            --  At this stage the view is complete. Update mappings
            --  (i.e effective view for extends and extends all) and DAG to
            --  order the views.

            declare
               use IDS;
               Unique_ID : constant View_Id := View.Id;
            begin
               pragma Assert (Is_Defined (Unique_ID));

               --  Finally update the DAG structure that will define the
               --  processing order for the views.

               declare
                  Predecessors : GPR2.View_Ids.Set.Set;
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
                  --  project.

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

         elsif Parent.Is_Defined then
            if Parent.Kind = K_Aggregate_Library
              and then Status = Aggregated
            then
               --  We need to keep track of aggregate libraries
               --  closure, and namespace root views

               Propagate_Aggregate (View, Parent.Id, True);
            end if;

            if Parent.Kind /= K_Aggregate then
               for Root of Parent.Namespace_Roots loop
                  Propagate_Aggregate (View, Root.Id, False);
               end loop;
            elsif Status = Aggregated then
               Propagate_Aggregate (View, View.Id, False);
            end if;
         end if;

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
         Project : constant GPR2.Project.Parser.Object :=
                     GPR2.Project.Parser.Parse
                       (Filename,
                        Self.Implicit_With,
                        Self.Messages,
                        GPR2.File_Readers.Convert (Self.File_Reader));
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
                        GPR2.Project.Parser.Parse
                          (Import_Filename,
                           Self.Implicit_With,
                           Self.Messages,
                           GPR2.File_Readers.Convert (Self.File_Reader)));

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
                     Data.Trees.Extended := GPR2.Project.Parser.Parse
                       (Extended_Filename,
                        Self.Implicit_With,
                        Self.Messages,
                        GPR2.File_Readers.Convert (Self.File_Reader));
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

      -------------------------
      -- Propagate_Aggregate --
      -------------------------

      procedure Propagate_Aggregate
        (View                 : in out GPR2.Project.View.Object;
         Root                 : GPR2.View_Ids.View_Id;
         Is_Aggregate_Library : Boolean)
      is
         Data     : constant GPR2.Project.Definition.Ref :=
                      Definition.Get_RW (View);
         Position : GPR2.View_Ids.Set.Cursor;
         Inserted : Boolean := False;

      begin
         if Is_Aggregate_Library then
            Data.Agg_Libraries.Insert (Root, Position, Inserted);
         elsif not Data.Root_Views.Contains (Root) then
            Data.Root_Views.Append (Root);
            Inserted := True;
         end if;

         if not Inserted then
            return;
         end if;

         if Data.Extended_Root.Is_Defined then
            Propagate_Aggregate
              (Data.Extended_Root, Root, Is_Aggregate_Library);
         end if;

         for Import of Data.Imports loop
            Propagate_Aggregate (Import, Root, Is_Aggregate_Library);
         end loop;

         for Import of Data.Limited_Imports loop
            Propagate_Aggregate (Import, Root, Is_Aggregate_Library);
         end loop;

         if not Is_Aggregate_Library
           and then Data.Kind = K_Aggregate_Library
         then
            --  Propagate the namespace root
            for Agg of Data.Aggregated loop
               Propagate_Aggregate (Agg, Root, False);
            end loop;
         end if;
      end Propagate_Aggregate;

   begin
      if Starting_From.Is_Defined then
         PP := Starting_From.Attribute (PRA.Project_Path);
      end if;

      if PP.Is_Defined then
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

               Parent := Starting_From;

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
               --  Aggregate library project
               Status := Aggregated;

            else
               Status := Simple;
            end if;
         end if;

         Result := Internal
           (Project     => Root_Project,
            Aggregate   => Aggregate,
            Status      => Status,
            Parent      => Parent,
            Extends_Ctx => View.Vector.Empty_Vector);

         --  Update the DAG with the newly loaded tree.
         --
         --  ??? This is certainly not optimal, in particular when there's
         --  a lot of aggregates, potentially nested, in the final tree, as
         --  Recursive_Load will be called for each aggregated project. However
         --  that's convenient to do it there as we have a single entry point
         --  to handle circularity issues. If performance becomes an issue, we
         --  would need to move this DAG update to the upper level to have a
         --  smarter handling of aggregated projects, so in Set_Context and
         --  Load (and of course we need to propagate the list to this upper
         --  level handling)

         declare
            Cycle       : GPR2.View_Ids.Vector.Vector;
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
                       (Result.Path_Name.Value, 0, 0)));

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
     (Self : in out Object; Dir : Path_Name.Object) is
   begin
      Prepend_Search_Paths (Self, Dir);
   end Register_Project_Search_Path;

   -------------------
   -- Register_View --
   -------------------

   function Register_View
     (Def : in out Definition.Data) return Project.View.Object
   is
      View : Project.View.Object;

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

      return View;
   end Register_View;

   ------------------------------------
   -- Restrict_Autoconf_To_Languages --
   ------------------------------------

   procedure Restrict_Autoconf_To_Languages
     (Self  : in out Object;
      Langs : Containers.Language_Set) is
   begin
      Self.Langs_Of_Interest := Langs;
   end Restrict_Autoconf_To_Languages;

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
      if Self.Root.Is_Defined then
         TA := Self.Root.Attribute
           (PRA.Runtime, Index => Attribute_Index.Create (Language));

         if TA.Is_Defined then
            return Optional_Name_Type (TA.Value.Text);
         end if;
      end if;

      return No_Name;
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
      Def         : Definition.Ref;
      Attr        : Attribute.Object;

   begin
      --  Register the root context for this project tree

      Self.Context (GPR2.Context.Root) := Context;

      --  Take missing external values from environment

      Update_Context
        (Self.Context (GPR2.Context.Root), Root.Externals, Self.Environment);

      Set_Context (Self, Changed);

      for V of Self.Views_Set loop
         Def := Definition.Get (V);
         Def.Clear_Cache;

         if Self.Has_Src_Subdirs
           and then V.Kind not in K_Configuration | K_Abstract | Aggregate_Kind
           and then V /= Self.Runtime
         then
            --  ensure we don't cache the value of Source_Dirs
            Def.Disable_Cache;

            declare
               SD  : constant Attribute.Object    :=
                       V.Attribute (PRA.Source_Dirs);
               SV  : Containers.Source_Value_List := SD.Values;
            begin
               SV.Prepend
                 (Source_Reference.Value.Object
                    (Source_Reference.Value.Create
                         (Source_Reference.Object (SD),
                          V.Source_Subdirectory.Value)));

               Def.Attrs.Include (Attribute.Create (SD.Name, SV));
            end;

            Def.Enable_Cache;
         end if;

         if V.Is_Library then
            Def.Interface_Units.Clear;
            Def.Interface_Sources.Clear;

            if V.Check_Attribute (PRA.Library_Interface, Result => Attr) then
               for Val of Attr.Values loop
                  Def.Interface_Units.Insert
                    (Name_Type (Val.Text), Source_Reference.Object (Val));
               end loop;
            end if;

            if V.Check_Attribute (PRA.Interfaces, Result => Attr) then
               for Val of Attr.Values loop
                  Def.Interface_Sources.Insert
                    (Filename_Type (Val.Text), Source_Reference.Object (Val));
               end loop;
            end if;
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
         Agg_Paths     : GPR2.Containers.Filename_Set;
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

               View_Dir      : constant GPR2.Path_Name.Object :=
                                 Path_Name.Create_Directory
                                   (Filename_Optional
                                      (View.Path_Name.Dir_Name));
               --  View root directory

               Pattern       : constant GPR2.Path_Name.Object :=
                                 (if OS_Lib.Is_Absolute_Path (Projects.Text)
                                  then Path_Name.Create_File
                                    (Filename_Optional (Projects.Text),
                                     Path_Name.No_Resolution)
                                  else View_Dir.Compose
                                    (Filename_Optional (Projects.Text)));
               --  The absolute path pattern to get matching files

               Dir_Part      : constant Filename_Optional :=
                                 Filename_Optional
                                   (Pattern.Containing_Directory.Relative_Path
                                      (View_Dir).Value);
               --  The dir part without the trailing directory separator

               Filename_Part : constant Filename_Optional :=
                                 Filename_Optional (Pattern.Simple_Name);
               --  The filename pattern of matching files

               Filename      : constant Filename_Optional :=
                                 (if Strings.Fixed.Index
                                    (String (Filename_Part),
                                     Wildcards,
                                     Going => Strings.Backward) = 0
                                  then Filename_Part
                                  else "");
               --  "" if Filename part is a regular expression otherwise the
               --  filename to locate.

               Filename_Regexp : constant GNAT.Regexp.Regexp :=
                                   GPR2.Compile_Regexp (Filename_Part);
               --  regexp pattern for matching Filename

               procedure Handle_File
                 (File      : GPR2.Path_Name.Object;
                  Timestamp : Ada.Calendar.Time);

               procedure Is_Directory_Handled
                 (Directory       : GPR2.Path_Name.Object;
                  Is_Root_Dir     : Boolean;
                  Do_Dir_Visit    : in out Boolean;
                  Do_Subdir_Visit : in out Boolean);

               -----------------
               -- Handle_File --
               -----------------

               procedure Handle_File
                 (File      : GPR2.Path_Name.Object;
                  Timestamp : Ada.Calendar.Time)
               is
                  pragma Unreferenced (Timestamp);
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
                 (Directory       : GPR2.Path_Name.Object;
                  Is_Root_Dir     : Boolean;
                  Do_Dir_Visit    : in out Boolean;
                  Do_Subdir_Visit : in out Boolean)
               is
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
               Definition.Foreach
                 (Base_Dir          => View.Dir_Name,
                  Messages          => View.Tree.Log_Messages.all,
                  Directory_Pattern => Dir_Part,
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
            when IO_Exceptions.Name_Error =>
               Log (Message.Error,
                    Projects.Text & " contains an invalid directory");
               return Files;
         end Get_Matching_Files;

         function Is_Implicitly_Abstract
           (View : Project.View.Object) return Boolean;
         --  Returns True if project can be recognized as abstract project.
         --  I.e. not inherited from non abstract project and one of
         --  source list defining attributes is empty.

         ----------------------------
         -- Is_Implicitly_Abstract --
         ----------------------------

         function Is_Implicitly_Abstract
           (View : Project.View.Object) return Boolean
         is

            function Is_Defined_Empty (Attr : Q_Attribute_Id) return Boolean;
            --  Returns True if attribute defined as empty list in view

            ----------------------
            -- Is_Defined_Empty --
            ----------------------

            function Is_Defined_Empty (Attr : Q_Attribute_Id) return Boolean is
            begin
               Tmp_Attr := View.Attribute (Attr);

               return Tmp_Attr.Is_Defined and then Tmp_Attr.Values.Is_Empty;
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
         GPR2.Project.Parser.Process
           (P_Data.Trees.Project,
            Self,
            (if View.Kind = K_Configuration
                  and then Self.Root.Kind in Aggregate_Kind
             then Self.Context (Aggregate)
             else View.Context),
            View,
            Self.Pre_Conf_Mode);

         if Self.Messages.Has_Error then
            return;
         end if;

         if View.Qualifier not in Aggregate_Kind then
            New_Signature := View.Context.Signature (P_Data.Externals);

         elsif not P_Data.Attrs.Contains (PRA.Project_Files.Attr) then
            --  Aggregate project must have Project_Files attribute

            Self.Error
              ("attribute ""Project_Files"" must be defined in"
               & " an aggregate project",
               Source_Reference.Create (View.Path_Name.Value, 0, 0));

         else
            --  If an aggregate project and an attribute external is defined
            --  then remove the dependency on the corresponding externals.

            if P_Data.Is_Root then
               for C in P_Data.Attrs.Iterate (Name => PRA.External.Attr) loop
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

               for C in P_Data.Attrs.Iterate (PRA.External.Attr) loop
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
            Agg_Paths.Clear;

            --  Pathname for Project_Files projects are relative to the
            --  aggregate project only.

            Paths.Append (View.Path_Name);

            for Project of P_Data.Attrs.Element (PRA.Project_Files.Attr).Values
            loop
               declare
                  Found : Boolean := False;
               begin
                  for Pathname of Get_Matching_Files (Project) loop
                     if Pathname = View.Path_Name then
                        --  We are loading recursively the aggregate project
                        --  As in GPR1 ignore it.

                        Found := True;

                     elsif Agg_Paths.Contains
                       (Filename_Type (Pathname.Value))
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
                                          Root_Project  => (Project_Path,
                                                            Pathname),
                                          Context       => Context,
                                          Starting_From => View);
                        begin
                           --  If there was error messages during the parsing
                           --  of the aggregated project, just exit now.

                           if Self.Messages.Has_Error then
                              raise Project_Error with Pathname.Value;
                           end if;

                           --  Record aggregated view into the aggregate's view

                           P_Data.Aggregated.Append (A_View);
                           Agg_Paths.Insert (Filename_Type (Pathname.Value));
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
                       (PRA.Library_Name, Result => Tmp_Attr)
                 and then (Tmp_Attr.Value.Text /= ""
                           or else Tmp_Attr.Value.Is_From_Default)
                 and then View.Check_Attribute
                            (PRA.Library_Dir, Result => Tmp_Attr)
                 and then (Tmp_Attr.Value.Text /= ""
                           or else Tmp_Attr.Value.Is_From_Default)
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
            end if;
         end if;
      end Set_View;

      --------------------
      -- Validity_Check --
      --------------------

      procedure Validity_Check (View : Project.View.Object) is
         use type PRA.Index_Value_Type;

         P_Kind : constant Project_Kind := View.Kind;
         P_Data : constant Definition.Const_Ref := Definition.Get_RO (View);

      begin
         --  Check global properties

         if View.Kind /= View.Qualifier then
            Self.Lint
              ("project qualifier could be explicitly set to "
               & Image (View.Kind),
               Source_Reference.Create (View.Path_Name.Value, 0, 0));
         end if;

         --  Check packages

         for P of P_Data.Packs loop
            if Registry.Pack.Exists (P.Id) then
               --  Check the package itself

               if not Registry.Pack.Is_Allowed_In (P.Id, P_Kind) then
                  Self.Warning
                    ("package """ & Image (P.Id)
                     & """ cannot be used in " & Image (P_Kind) & 's',
                     P);
               end if;

               --  Check package's attributes

               for A of P.Attrs loop
                  declare
                     Q_Name : constant Q_Attribute_Id :=
                                (P.Id, A.Name.Id.Attr);
                     Def    : constant PRA.Def := PRA.Get (Q_Name);

                  begin
                     if not Def.Is_Allowed_In (P_Kind) then
                        Self.Warning
                          ("attribute """ & Image (Q_Name)
                           & """ cannot be used in " & Image (P_Kind),
                           A);
                     end if;

                     --  In aggregate project, the Builder package only
                     --  accepts the index "others" for file globs.

                     if P_Data.Kind in Aggregate_Kind
                       and then P.Id = PRP.Builder
                       and then Def.Index_Type in
                            PRA.FileGlob_Index | PRA.FileGlob_Or_Language_Index
                       and then A.Index.Is_Defined
                       and then not A.Index.Is_Others
                     then
                        Self.Warning
                          ("attribute """ & Image (Q_Name)
                           & """ only supports index ""others"""
                           & " in aggregate projects",
                           A);
                     end if;
                  end;
               end loop;
            end if;
         end loop;

         --  Check top level attributes

         for A of P_Data.Attrs loop
            declare
               Q_Name : constant Q_Attribute_Id := A.Name.Id;
            begin
               declare
                  Allowed : constant PRA.Allowed_In :=
                              PRA.Get (Q_Name).Is_Allowed_In;
                  Found   : Natural := 0;
                  Allow   : Project_Kind;
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
                          ('"' & Image (A.Name.Id) & """ is only valid in "
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
            end;
         end loop;

         --  Check Library_Version attribute format

         declare
            procedure Check_Directory
              (Name          : Q_Attribute_Id;
               Human_Name    : String;
               Get_Directory : not null access function
                                 (Self : Project.View.Object)
                                  return Path_Name.Object;
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
              (Name          : Q_Attribute_Id;
               Human_Name    : String;
               Get_Directory : not null access function
                                 (Self : Project.View.Object)
                                  return Path_Name.Object;
               Mandatory     : Boolean := False;
               Must_Exist    : Boolean := True) is
            begin
               --  We don't warn for default attributes (such as exec dir
               --  defaulting to a non-existing object dir), as we already
               --  warn for the referenced attribute.
               if View.Check_Attribute (Name, Result => Attr)
                 and then not Attr.Is_Default
               then
                  declare
                     AV : constant Source_Reference.Value.Object := Attr.Value;
                     PN : constant Path_Name.Object := Get_Directory (View);
                  begin
                     if Must_Exist
                       and then Self.Absent_Dir_Error /= No_Error
                       and then not PN.Exists
                     then
                        Self.Messages.Append
                          (Message.Create
                             ((if Self.Absent_Dir_Error = Error
                              then Message.Error
                              else Message.Warning),
                              (if Human_Name = ""
                               then "D"
                               else Human_Name & " d") & "irectory """
                              & AV.Text & """ not found",
                              Sloc => AV));

                     elsif Self.Build_Path.Is_Defined
                       and then OS_Lib.Is_Absolute_Path (AV.Text)
                       and then Self.Root.Is_Defined
                       and then Self.Build_Path /= Self.Root.Dir_Name
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
                        "attribute " & Image (Name.Attr) & " not declared",
                        Source_Reference.Create (View.Path_Name.Value, 0, 0)));
               end if;
            end Check_Directory;

         begin
            if View.Kind in K_Standard | K_Library | K_Aggregate_Library
              and then not View.Is_Aggregated_In_Library
            then
               Check_Directory
                 (PRA.Object_Dir,
                  "object",
                  Project.View.Object_Directory'Access,
                  Must_Exist => not View.Is_Aggregated_In_Library
                                  and then not View.Is_Extended);
            end if;

            if View.Is_Library
              and then not View.Is_Aggregated_In_Library
            then
               Check_Directory
                 (PRA.Library_Dir,
                  "library",
                  Project.View.Library_Directory'Access,
                  Mandatory  => True,
                  Must_Exist => not View.Is_Aggregated_In_Library
                                  and then not View.Is_Extended);

               Check_Directory
                 (PRA.Library_Ali_Dir,
                  "library ALI",
                  Project.View.Library_Ali_Directory'Access,
                  Must_Exist => not View.Is_Aggregated_In_Library
                                  and then not View.Is_Extended);

               if View.Has_Library_Interface
                 or else View.Has_Attribute (PRA.Interfaces)
               then
                  Check_Directory
                    (PRA.Library_Src_Dir,
                     "",
                     Project.View.Library_Src_Directory'Access);
               end if;

               if not View.Check_Attribute (PRA.Library_Name, Result => Attr)
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
                  if not View.Is_Aggregated_In_Library then
                     Check_Directory
                       (PRA.Exec_Dir,
                        "exec",
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

         --  Check source list files
         if View.Kind in K_Standard | K_Library then
            declare
               Att  : Attribute.Object;
               File : Path_Name.Object;
            begin
               if View.Check_Attribute
                 (PRA.Source_List_File, Result => Att)
               then
                  File := Path_Name.Create_File
                    (Filename_Type (Att.Value.Text),
                     Filename_Type (View.Path_Name.Dir_Name));
                  if not File.Exists or else File.Is_Directory then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "source list file " & File.Value & " not found",
                           Sloc => View.Attribute_Location
                             (PRA.Source_List_File)));
                  end if;
               end if;

               if View.Check_Attribute
                 (PRA.Excluded_Source_List_File, Result => Att)
               then
                  File := Path_Name.Create_File
                    (Filename_Type (Att.Value.Text),
                     Filename_Type (View.Path_Name.Dir_Name));
                  if not File.Exists or else File.Is_Directory then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "excluded source list file "
                           & File.Value & " not found",
                           Sloc => View.Attribute_Location
                             (PRA.Excluded_Source_List_File)));
                  end if;
               end if;
            end;
         end if;
      end Validity_Check;

   begin
      --  Clear attribute value cache

      for V of Self.Views_Set loop
         Definition.Get (V).Clear_Cache;
      end loop;

      --  Now the first step is to set the configuration project view if any
      --  and to create the runtime project if possible.

      if Self.Has_Configuration then
         Set_View (Self.Conf.Corresponding_View);

         Self.Runtime := Create_Runtime_View (Self);
      end if;

      declare
         Closure_Found : Boolean := True;
         Closure       : GPR2.View_Ids.Set.Set;
         Position      : GPR2.View_Ids.Set.Cursor;
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

               exit when Has_Error;
            end if;
         end loop;

         --  Now evaluate the remaining views

         if not Has_Error then
            Closure_Loop :
            loop
               for View of Self.Ordered_Views loop
                  Closure.Insert (View.Id, Position, Inserted);

                  if Inserted then
                     Closure_Found := False;
                     Set_View (View);
                     exit Closure_Loop when Has_Error;
                  end if;
               end loop;

               exit Closure_Loop when Closure_Found;
               Closure_Found := True;
            end loop Closure_Loop;
         end if;
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

   ---------------------
   -- Set_Environment --
   ---------------------

   procedure Set_Environment
     (Self : in out Object; Environment : GPR2.Environment.Object) is
   begin
      Self.Environment := Environment;
      Self.Search_Paths.Default := Default_Search_Paths (True, Environment);
      Self.Update_Search_Paths;
   end Set_Environment;

   ------------------------
   -- Source_Directories --
   ------------------------

   function Source_Directories
     (Self             : Object;
      View             : Project.View.Object := Project.View.Undefined;
      Externally_Built : Boolean := False) return GPR2.Path_Name.Set.Object
   is
      Result : GPR2.Path_Name.Set.Object;

      procedure Do_Action (View : Project.View.Object)
        with Pre => View.Is_Defined;
      --  Add to Result all view's source directories

      ---------------
      -- Do_Action --
      ---------------

      procedure Do_Action (View : Project.View.Object) is
      begin
         for Directory of View.Source_Directories loop
            if not Result.Contains (Directory) then
               Result.Append (Directory);
            end if;
         end loop;
      end Do_Action;

   begin
      Process_Subtree (Self, View, Externally_Built, Do_Action'Access);
      return Result;
   end Source_Directories;

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
        and then Self.Root.Check_Attribute (PRA.Target, Result => TA)
      then
         return Name_Type (TA.Value.Text);

      elsif Self.Base.Is_Defined then
         return Normalized (Target_Name);

      else
         --  Target name as specified during the build
         return Target_Name;
      end if;
   end Target;

   ------------------------------
   -- Target_From_Command_Line --
   ------------------------------

   function Target_From_Command_Line
     (Self       : Object;
      Normalized : Boolean := False) return Name_Type
   is
      Target : constant Optional_Name_Type :=
                 Optional_Name_Type (-Self.Explicit_Target);
   begin
      if Target = No_Name then
         return "all";
      end if;

      if Normalized and then Self.Base.Is_Defined then
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
   end Target_From_Command_Line;

   ------------
   -- Unload --
   ------------

   procedure Unload
     (Self : in out Object;
      Full : Boolean := True) is
   begin
      if Full then
         GPR2.Project.Parser.Clear_Cache;
      end if;

      Self.Self             := Undefined.Self;
      Self.Root             := Undefined.Root;
      Self.Conf             := Undefined.Conf;
      Self.Runtime          := Undefined.Runtime;
      Self.Search_Paths     := Undefined.Search_Paths;
      Self.Implicit_With    := Undefined.Implicit_With;
      Self.Build_Path       := Undefined.Build_Path;
      Self.Subdirs          := Undefined.Subdirs;
      Self.Src_Subdirs      := Undefined.Src_Subdirs;
      Self.Check_Shared_Lib := Undefined.Check_Shared_Lib;
      Self.Absent_Dir_Error := Undefined.Absent_Dir_Error;
      Self.Pre_Conf_Mode    := Undefined.Pre_Conf_Mode;
      Self.Sources_Loaded   := Undefined.Sources_Loaded;
      Self.Explicit_Target  := Undefined.Explicit_Target;
      Self.File_Reader_Ref  := Undefined.File_Reader_Ref;
      Self.Environment      := Undefined.Environment;

      Self.Tree_Db.Unload;
      Self.Messages.Clear;
      Self.Views_Set.Clear;
      Self.View_Ids.Clear;
      Self.View_DAG.Clear;
      Self.Explicit_Runtimes.Clear;
   end Unload;

   --------------------
   -- Update_Context --
   --------------------

   procedure Update_Context
     (Context     : in out GPR2.Context.Object;
      Externals   : Containers.Name_List;
      Environment : GPR2.Environment.Object) is
   begin
      for External of Externals loop
         declare
            External_Value : constant String :=
                               Environment.Value
                                 (String (External), "");
            Position       : GPR2.Context.Key_Value.Cursor;
            Inserted       : Boolean;
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

      Drivers      : Attribute.Set.Object;

      PATH         : constant String := Self.Environment.Value ("PATH");
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
            Append_Search_Paths
              (Self, Path_Name.Create_Directory (Filename_Type (Dir)));
         end if;
      end Append;

      -----------------
      -- Is_Bin_Path --
      -----------------

      function Is_Bin_Path (T_Path : String) return Boolean is
      begin
         return Directories.Simple_Name (T_Path) = "bin";
      exception
         when IO_Exceptions.Name_Error =>
            return False;
      end Is_Bin_Path;

   begin
      if not Conf.Corresponding_View.Has_Package (Registry.Pack.Compiler) then
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

      Drivers := Conf.Corresponding_View.Attributes (PRA.Compiler.Driver);

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
                           --  specified target (if it has been specified).

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

   -------------------------
   -- Update_Search_Paths --
   -------------------------

   procedure Update_Search_Paths (Self : in out Object) is
   begin
      Self.Search_Paths.All_Paths := Self.Search_Paths.Prepended;

      for P of Self.Search_Paths.Default loop
         Self.Search_Paths.All_Paths.Append (P);
      end loop;

      for P of Self.Search_Paths.Appended loop
         Self.Search_Paths.All_Paths.Append (P);
      end loop;
   end Update_Search_Paths;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources
     (Self         : Object;
      With_Runtime : Boolean := False;
      Messages     : out GPR2.Log.Object)
   is
   begin
      Self.Self.Sources_Loaded := True;

      Self.Artifacts_Database.Refresh (With_Runtime, Messages);

      --  Make sure the runtime is taken care of first : it is most certainly
      --  involved in source dependencies at one point or another.

      if With_Runtime
        and then Self.Runtime.Is_Defined
      then
         Self.Artifacts_Database (Self.Runtime).Update
           (Messages);
      end if;
   end Update_Sources;

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
   Definition.Get_Context        := Get_Context'Access;
   Definition.Are_Sources_Loaded := Are_Sources_Loaded'Access;
end GPR2.Project.Tree;
