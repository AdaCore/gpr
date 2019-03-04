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

with Ada.Containers.Ordered_Maps;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Text_IO;

with GPR2.Parser.Project.Create;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Definition;
with GPR2.Project.Import.Set;
with GPR2.Project.Name_Values;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;
with GPR2.Unit;

with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.String_Split;

with GNATCOLL.Utils;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

package body GPR2.Project.Tree is

   use Ada;
   use GNAT;
   use type GPR2.Path_Name.Object;

   package PC renames Project.Configuration;

   GPRls : constant OS_Lib.String_Access :=
             OS_Lib.Locate_Exec_On_Path ("gprls");
   --  Check for GPRls executable

   Version_Regexp : constant Regexp.Regexp :=
                      Regexp.Compile (".[0-9]+.[0-9]+");

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
     (Self          : Object;
      Filename      : Path_Name.Object;
      Context_View  : View.Object;
      Status        : Relation_Status;
      Root_Context  : out GPR2.Context.Object;
      Messages      : out Log.Object;
      Circularities : out Boolean;
      Starting_From : View.Object := View.Undefined) return View.Object
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
      Unit : GPR2.Unit.Object)
   is
      --  If the spec is not present, then the actual source object used is the
      --  first body which must exist. We can't have no spec and no body.

      Src : constant Project.Source.Object :=
              (if not Unit.Spec.Is_Defined
               then Unit.Bodies.First_Element
               else Unit.Spec);
   begin
      --  Clear the unit

      Self.Units.Exclude (Src.Source.Unit_Name);

      --  Clear the corresponding sources

      Self.Sources.Exclude (Name_Type (Src.Source.Path_Name.Value));

      for B of Unit.Bodies loop
         Self.Sources.Exclude (Name_Type (B.Source.Path_Name.Value));
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
   begin
      --  Check runtime path

      if CV.Has_Attributes ("runtime_dir", "ada") then
         --  Runtime_Dir (Ada) exists, this is the Source_Dirs for the Runtime
         --  project view.

         declare
            Runtime_Dir : constant String :=
                            CV.Attribute ("runtime_dir", "ada").Value.Text;
         begin
            Data.Attrs.Insert
              (Project.Attribute.Create
                 (Name  =>
                    Source_Reference.Identifier.Object
                      (Source_Reference.Identifier.Create
                         (Source_Reference.Builtin,
                          Project.Registry.Attribute.Source_Dirs)),
                  Value =>
                    Source_Reference.Value.Object
                      (Source_Reference.Value.Create
                           (Source_Reference.Builtin,
                            Runtime_Dir & DS & "adainclude"))));
         end;

         --  The only language supported is Ada

         Data.Attrs.Insert
           (Project.Attribute.Create
              (Name  =>
                   Source_Reference.Identifier.Object
                     (Source_Reference.Identifier.Create
                       (Source_Reference.Builtin,
                        Project.Registry.Attribute.Languages)),
               Value =>
                    Source_Reference.Value.Object
                      (Source_Reference.Value.Create
                         (Source_Reference.Builtin, "ada"))));

         Data.Tree   := Self.Self;
         Data.Status := Root;
         Data.Kind   := K_Standard;

         Data.Trees.Project := Parser.Project.Create
           (Name      => "Runtime",
            File      => Path_Name.Create_File ("runtime.gpr"),
            Qualifier => K_Standard);

         return Register_View (Data);

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
            Context.Insert (E, Environment_Variables.Value (String (E)));
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
                   (Qualifier = K_Aggregate and then Iter.Filter (F_Aggregate))
                 or else
                   (Qualifier = K_Aggregate_Library
                    and then Iter.Filter (F_Aggregate_Library))
               then
                  Projects.Append (View);
               end if;
            end;

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
            if Equal (Iter.Status (S_Externally_Built),
                      I.Is_Externally_Built) in True | Indeterminate
            then
               if Iter.Kind (I_Recursive) then
                  For_Project (I);
               else
                  Append (I);
               end if;
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
               if Defs.Tree.all = Tree
                 and then Defs.Context_View = Context_View
                 and then (Defs.Status = Status
                           or else Status /= Aggregated)
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
               if Defs.Tree.all = Tree
                 and then Defs.Context_View = Context_View
               then
                  return V;
               end if;
            end;
         end loop;
      end if;

      return Project.View.Undefined;
   end Get;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Self   : Object;
      Source : Path_Name.Object) return Project.View.Object
   is
      Pos : Name_View.Cursor :=
              Self.Sources.Find (Name_Type (Source.Value));
   begin
      if Name_View.Has_Element (Pos) then
         return Name_View.Element (Pos);

      else
         --  Try to update sources and check again

         Update_Sources (Self);
         Pos := Self.Sources.Find (Name_Type (Source.Value));

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
      Config           : PC.Object          := PC.Undefined;
      Build_Path       : Path_Name.Object   := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type := No_Name;
      Check_Shared_Lib : Boolean            := True)
   is
      use Ada.Strings.Unbounded;

      procedure Set_Project_Search_Paths;
      --  Set project search path for the tree

      ------------------------------
      -- Set_Project_Search_Paths --
      ------------------------------

      procedure Set_Project_Search_Paths is

         use type GNAT.OS_Lib.String_Access;

         procedure Append
           (Result : in out Path_Name.Set.Object; Value : String)
           with Post => (if Value'Length = 0
                           then Result'Old.Length = Result.Length
                             else Result'Old.Length + 1 = Result.Length);

         procedure Add_List
           (Result : in out Path_Name.Set.Object;
            Values : String)
           with Post => Result'Old.Length <= Result.Length;
         --  Add list Values (which has OS dependant path separator) into
         --  Result

         --------------
         -- Add_List --
         --------------

         procedure Add_List
           (Result : in out Path_Name.Set.Object;
            Values : String)
         is
            V  : String_Split.Slice_Set;
         begin
            String_Split.Create
              (V, Values, String'(1 => OS_Lib.Path_Separator));

            for K in 1 .. String_Split.Slice_Count (V) loop
               Append (Result, String_Split.Slice (V, K));
            end loop;
         end Add_List;

         ------------
         -- Append --
         ------------

         procedure Append
           (Result : in out Path_Name.Set.Object;
            Value  : String) is
         begin
            if Value /= "" then
               Result.Append
                 (Path_Name.Create_Directory (Name_Type (Value)));
            end if;
         end Append;

      begin
         --  Then in GPR_PROJECT_PATH_FILE, one path per line

         if Environment_Variables.Exists ("GPR_PROJECT_PATH_FILE") then
            declare
               Filename : constant String :=
                            Environment_Variables.Value
                              ("GPR_PROJECT_PATH_FILE");
               Buffer   : String (1 .. 1024);
               Last     : Natural;
               File     : Text_IO.File_Type;
            begin
               if Directories.Exists (Filename) then
                  Text_IO.Open (File, Text_IO.In_File, Filename);

                  while not Text_IO.End_Of_File (File) loop
                     Text_IO.Get_Line (File, Buffer, Last);
                     Append (Self.Search_Paths, Buffer (1 .. Last));
                  end loop;

                  Text_IO.Close (File);
               end if;
            end;
         end if;

         --  Then in GPR_PROJECT_PATH and ADA_PROJECT_PATH

         if Environment_Variables.Exists ("GPR_PROJECT_PATH") then
            Add_List
              (Self.Search_Paths,
               Environment_Variables.Value ("GPR_PROJECT_PATH"));
         end if;

         if Environment_Variables.Exists ("ADA_PROJECT_PATH") then
            Add_List
              (Self.Search_Paths,
               Environment_Variables.Value ("ADA_PROJECT_PATH"));
         end if;

         if GPRls /= null then
            declare
               Prefix : constant String :=
                          Directories.Containing_Directory
                            (Directories.Containing_Directory (GPRls.all));
            begin
               --  <prefix>/<target>/share/gpr

               Append
                 (Self.Search_Paths,
                  Directories.Compose
                    (Directories.Compose
                      (Directories.Compose
                        (Prefix, String (Self.Target)), "share"), "gpr"));

               --  <prefix>/<target>/lig/gnat

               Append
                 (Self.Search_Paths,
                  Directories.Compose
                    (Directories.Compose
                      (Directories.Compose
                        (Prefix, String (Self.Target)), "lib"), "gnat"));

               --  <prefix>/share/gpr

               Append
                 (Self.Search_Paths,
                  Directories.Compose
                    (Directories.Compose (Prefix, "share"), "gpr"));

               --  <prefix>/lib/gnat

               Append
                 (Self.Search_Paths,
                  Directories.Compose
                    (Directories.Compose (Prefix, "lib"), "gnat"));
            end;
         end if;

         --  Add all search paths into the message log

         for P of Self.Search_Paths loop
            Self.Messages.Append
              (Message.Create
                 (Message.Information,
                  P.Value,
                  Source_Reference.Create (Filename.Value, 0, 0)));
         end loop;
      end Set_Project_Search_Paths;

      Root_Context  : GPR2.Context.Object := Context;
      Circularities : Boolean;

   begin
      Self.Self := Self'Unchecked_Access;

      --  First record and parse the configuration object, this is needed as
      --  used to check the target in Set_Project_Search_Paths above.

      if Config.Is_Defined then
         --  Set Tree for this config project

         Self.Conf := Config;

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
               Context,
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
      Self.Check_Shared_Lib := Check_Shared_Lib;

      --  Now we can initialize the project search paths

      Set_Project_Search_Paths;

      Self.Root := Recursive_Load
        (Self, Filename, View.Undefined, Root, Root_Context, Self.Messages,
         Circularities);

      --  Do nothing more if there are errors during the parsing

      if not Self.Messages.Has_Error then
         for V_Data of Self.Views_Set loop
            --  Compute the external dependencies for the views. This
            --  is the set of external used in the project and in all
            --  imported project.

            for V of Definition.Get_RO (V_Data).Imports loop
               for E of Definition.Get_RO (V).Externals loop
                  if not Definition.Get_RO (V_Data).Externals.Contains (E) then
                     --  Note that if we have an aggregate project, then
                     --  we are not dependent on the external if it is
                     --  statically redefined in the aggregate project. But
                     --  at this point we have not yet parsed the project.
                     --
                     --  The externals will be removed in Set_Context when
                     --  the parsing is done.

                     Definition.Get (V_Data).Externals.Append (E);
                  end if;
               end loop;
            end loop;
         end loop;

         Set_Context (Self, Context);

      else
         raise Project_Error with Filename.Value & " syntax error";
      end if;
   end Load;

   -------------------
   -- Load_Autoconf --
   -------------------

   procedure Load_Autoconf
     (Self              : in out Object;
      Filename          : Path_Name.Object;
      Context           : GPR2.Context.Object;
      Build_Path        : Path_Name.Object   := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type := No_Name;
      Check_Shared_Lib  : Boolean            := True;
      Target            : Optional_Name_Type := No_Name;
      Language_Runtimes : GPR2.Containers.Name_Value_Map :=
                            GPR2.Containers.Name_Value_Map_Package.Empty_Map)
   is
      Nb_Languages : Natural;
      Descr_Index  : Natural := 0;
      Conf         : Project.Configuration.Object;

   begin
      Self.Load
        (Filename, Context,
         Build_Path       => Build_Path,
         Subdirs          => Subdirs,
         Check_Shared_Lib => Check_Shared_Lib);

      if not Self.Root_Project.Has_Languages then
         pragma Assert (Self.Root_Project.Attributes.Has_Languages);

         Self.Append_Message
           (Message.Create
              (Level   => Message.Warning,
               Message => "no language for the project:"
               & " configuration skipped",
               Sloc    => Self.Root_Project.Attributes.Languages));
         return;
      end if;

      Nb_Languages := Natural (Self.Root_Project.Languages.Length);

      declare
         Actual_Target : constant Name_Type :=
                           (if Target /= No_Name then Target
                            elsif Self.Root_Project.Has_Attributes ("Target")
                            then Name_Type
                              (Self.Root_Project.Attribute
                                 ("Target").Value.Text)
                            else "all");

         Conf_Descriptions : Project.Configuration.Description_Set
                               (1 .. Nb_Languages);

      begin
         for L of Self.Root_Project.Languages loop
            Descr_Index := Descr_Index + 1;

            declare
               LRT : constant Value_Type :=
                       Containers.Value_Or_Default
                         (Language_Runtimes, Name_Type (L.Text));
               RTS : constant Optional_Name_Type :=
                          (if LRT /= No_Value
                           then Name_Type (LRT)
                           elsif Self.Root_Project.Has_Attributes
                             ("Runtime", "Ada")
                           then Name_Type (Self.Root_Project.Attribute
                             ("Runtime", "Ada").Value.Text)
                           else No_Name);

               --  RTS should be a Value_Path (type introduced in the
               --  multi-unit patch)

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
           (Conf_Descriptions, Actual_Target, Filename);

         Self.Load
           (Filename, Context, Conf, Build_Path,
            Subdirs          => Subdirs,
            Check_Shared_Lib => Check_Shared_Lib);
      end;
   end Load_Autoconf;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration
     (Self     : in out Object;
      Filename : Path_Name.Object) is
   begin
      Self.Self := Self'Unchecked_Access;

      Self.Conf := PC.Load (Filename);
      Definition.Bind_Configuration_To_Tree (Self.Conf, Self.Self);

      if Self.Conf.Has_Messages then
         for M of Self.Conf.Log_Messages loop
            Self.Messages.Append (M);
         end loop;

      else
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
      Source : Path_Name.Full_Name;
      Unit   : Name_Type) is
   begin
      Self.Units.Include (Unit, View);
      Self.Sources.Include (Name_Type (Source), View);
   end Record_View;

   --------------------
   -- Recursive_Load --
   --------------------

   function Recursive_Load
     (Self          : Object;
      Filename      : Path_Name.Object;
      Context_View  : View.Object;
      Status        : Relation_Status;
      Root_Context  : out GPR2.Context.Object;
      Messages      : out Log.Object;
      Circularities : out Boolean;
      Starting_From : View.Object := View.Undefined) return View.Object

   is

      function Load (Filename : Path_Name.Object) return Definition.Data;
      --  Returns the Data definition for the given project

      function Internal
        (Filename  : Path_Name.Object;
         Status    : Relation_Status;
         Aggregate : View.Object) return View.Object;

      procedure Add_Paths_Messages;
      --  Add into Messages the path of the detected circularity

      type Data is record
         Project  : GPR2.Project.Import.Object;
         Extended : Boolean;
      end record;

      package Data_Set is new Ada.Containers.Ordered_Maps
        (GPR2.Path_Name.Object, Data, "<" => GPR2.Path_Name."<");

      Sets  : Data_Set.Map;
      Paths : Path_Name.Set.Object;
      --  Path to the root of the tree from the currently processing project

      Limited_Count : Natural := 0;
      --  Number of limited imports in the Paths

      ------------------------
      -- Add_Paths_Messages --
      ------------------------

      procedure Add_Paths_Messages is
      begin
         for Import of Paths loop
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
        (Filename  : Path_Name.Object;
         Status    : Relation_Status;
         Aggregate : View.Object) return View.Object
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

               Data.Aggregate := Aggregate;

               --  Let's setup the full external environment for project

               Fill_Externals_From_Environment (Root_Context, Data.Externals);

               --  If we have the root project, record the global context

               if Data.Has_Context and then Status = Root then
                  --  This is the root-view, assign the corresponding context
                  Data.Context := Root_Context;
               end if;

               --  Create the view, needed to be able to reference it if it is
               --  an aggregate project as it becomes the new View_Context.

               Data.Context_View := Context_View;
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
                  Last : constant Path_Name.Object := Paths.Last_Element;
               begin
                  if not Sets (Last).Extended and then Is_Limited (Last) then
                     Limited_Count := Limited_Count - 1;
                  end if;

                  Paths.Delete_Last;
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
                  Paths.Append (Path_Name);
               end Push;

            begin
               --  Load the extended project if any

               if Data.Trees.Project.Has_Extended then
                  declare
                     Paths     : constant Path_Name.Set.Object :=
                                   GPR2.Project.Search_Paths
                                     (Filename, Self.Search_Paths);

                     Path_Name : constant GPR2.Path_Name.Object :=
                                   Create
                                     ((if Data.Trees.Project.Extended.
                                        Path_Name.Exists
                                      then Name_Type (Data.Trees.Project.
                                          Extended.Path_Name.Value)
                                      else Data.Trees.Project.Extended.
                                        Path_Name.Name),
                                      Paths);

                  begin
                     if Path_Name.Exists then
                        Push (Path_Name, Data.Trees.Project.Extended, True);

                        Data.Extended :=
                          Internal
                            (Path_Name,
                             Status    => Imported,
                             Aggregate => Project.View.Undefined);

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
                                 Sets.Element (Paths.First_Element).Project));

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
                              Aggregate => GPR2.Project.View.Undefined));

                        Pop;
                     end if;
                  end;
               end loop;
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
                     Parser.Project.Parse (Filename, Messages);
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
                                      Create
                                        ((if Import.Path_Name.Exists
                                         then Name_Type
                                           (Import.Path_Name.Value)
                                         else Import.Path_Name.Name),
                                         Paths);
               begin
                  if Import_Filename.Exists then
                     Data.Trees.Imports.Insert
                       (Import_Filename,
                        Parser.Project.Parse (Import_Filename, Messages));

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
   begin
      if Def.Tree.Views_Set.Is_Empty then
         Def.Id := 1;
      else
         Def.Id := Definition.Get_RO (Def.Tree.Views_Set.Last_Element).Id + 1;
      end if;

      Definition.Set (View, Def);

      Def.Tree.Views_Set.Insert (View);

      pragma Assert (Definition.Refcount (View) = 2);

      if Def.Tree.Views.Contains (Path_Name) then
         Def.Tree.Views (Path_Name).Insert (View);
      else
         Def.Tree.Views.Insert (Path_Name, Project.View.Set.To_Set (View));
      end if;

      pragma Assert (Definition.Refcount (View) = 3);

      if Def.Tree.Views.Contains (Name) then
         Def.Tree.Views (Name).Insert (View);
      else
         Def.Tree.Views.Insert (Name, Project.View.Set.To_Set (View));
      end if;

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
      Root : constant Definition.Ref := Definition.Get_RW (Self.Root);
   begin
      --  Register the root context for this project tree

      Root.Context := Context;

      --  Take missing external values from environment

      Fill_Externals_From_Environment (Root.Context, Root.Externals);

      Set_Context (Self, Changed);
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
            for C in P_Data.Attrs.Iterate
              (Name => Project.Registry.Attribute.External)
            loop
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

            for Project of
              P_Data.Attrs.Element (Registry.Attribute.Project_Files).Values
            loop
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
                              if M.Sloc.Has_Source_Reference then
                                 Self.Messages.Append (M);
                              end if;
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

            for C in P_Data.Attrs.Iterate (Registry.Attribute.External) loop
               declare
                  use all type Project.Registry.Attribute.Value_Kind;

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
               if P_Data.Attrs.Contains (Registry.Attribute.Library_Kind)
                 or else
                   P_Data.Attrs.Contains (Registry.Attribute.Library_Name)
                 or else
                   P_Data.Attrs.Contains (Registry.Attribute.Library_Dir)
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
         use type Registry.Attribute.Index_Kind;
         use type Registry.Attribute.Value_Kind;

         Check_Object_Dir_Exists : Boolean := True;
         --  To avoid error on check Object_Dir existence when attribute is not
         --  correct.

         procedure Check_Def
           (Def : Registry.Attribute.Def;
            A   : Attribute.Object);
         --  Check if attribute definition is valid, record errors into the
         --  message log facility.

         ---------------
         -- Check_Def --
         ---------------

         procedure Check_Def
           (Def : Registry.Attribute.Def;
            A   : Attribute.Object) is
         begin
            if Def.Index = Registry.Attribute.No
              and then A.Has_Index
            then
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & String (A.Name) & """ cannot have index",
                     A));
            end if;

            if Def.Value = Registry.Attribute.Single
              and then A.Kind = Registry.Attribute.List
            then
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & String (A.Name) & """ cannot be a list",
                     A));

               if A.Name = Registry.Attribute.Object_Dir then
                  Check_Object_Dir_Exists := False;
               end if;
            end if;

            if Def.Value = Registry.Attribute.List
              and then A.Kind = Registry.Attribute.Single
            then
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute """ & String (A.Name) & """ must be a list",
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
                     Q_Name : constant Registry.Attribute.Qualified_Name :=
                                Registry.Attribute.Create (A.Name, P.Name);
                     Def    : Registry.Attribute.Def;
                  begin
                     if Registry.Attribute.Exists (Q_Name) then
                        Def := Registry.Attribute.Get (Q_Name);

                        if not Def.Is_Allowed_In (P_Kind) then
                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "attribute """ & String (A.Name)
                                 & """ cannot be used in package "
                                 & String (P.Name),
                                 A));
                        end if;

                        Check_Def (Def, A);

                     else
                        Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "attribute """ & String (A.Name)
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
               Q_Name : constant Registry.Attribute.Qualified_Name :=
                          Registry.Attribute.Create (A.Name);
            begin
               if not Registry.Attribute.Exists (Q_Name) then
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "unrecognized attribute """ & String (A.Name) & '"',
                        A));

               else
                  if not Registry.Attribute.Get
                    (Q_Name).Is_Allowed_In (P_Kind)
                  then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "attribute """ & String (A.Name)
                           & """ cannot be used in " & Image (P_Kind),
                           A));
                  end if;

                  Check_Def (Registry.Attribute.Get (Q_Name), A);
               end if;
            end;
         end loop;

         --  Check Library_Version attribute format

         declare
            package A renames GPR2.Project.Registry.Attribute;

            procedure Check_Shared_Lib (PV : Project.View.Object);
            --  Check that shared library project does not have in imports
            --  static library or standard projects.

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

                  elsif Imp.Is_Static_Library then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "shared library project """ & String (View.Name)
                           & """ cannot import static library project """
                           & String (Imp.Name) & '"',
                           P_Data.Trees.Project.Imports.Element
                             (Imp.Path_Name)));
                  end if;
               end loop;
            end Check_Shared_Lib;

         begin
            if View.Is_Library and then View.Is_Shared_Library then
               if View.Has_Attributes (A.Library_Version) then
                  declare
                     AV      : constant Source_Reference.Value.Object :=
                                 View.Attribute (A.Library_Version).Value;
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
              and then View.Has_Attributes (A.Object_Dir)
            then
               declare
                  AV : constant Source_Reference.Value.Object :=
                         View.Attribute (A.Object_Dir).Value;
               begin
                  if not View.Object_Directory.Exists then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Warning,
                           "object directory """ & AV.Text & """ not found",
                           Sloc => AV));

                  elsif Self.Build_Path.Is_Defined
                    and then OS_Lib.Is_Absolute_Path (AV.Text)
                  then
                     Self.Messages.Append
                       (Message.Create
                          (Message.Warning,
                           '"' & View.Object_Directory.Relative_Path
                                   (Self.Root_Project.Path_Name).Value
                           & """ cannot relocate absolute object directory",
                           Sloc => AV));
                  end if;
               end;
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
   begin
      if Self.Has_Configuration
        and then Self.Conf.Target /= ""
      then
         return Self.Conf.Target;

      elsif Self.Has_Configuration
        and then Self.Conf.Corresponding_View.Has_Attributes
                   (Registry.Attribute.Target)
      then
         return Name_Type
           (Self.Conf.Corresponding_View.Attribute
              (Registry.Attribute.Target).Value.Text);

      else
         return Name_Type (System.OS_Constants.Target_Name);
      end if;
   end Target;

   ------------
   -- Unload --
   ------------

   procedure Unload (Self : in out Object) is
   begin
      for C in Self.Iterate loop
         declare
            V : Project.View.Object := Element (C);
         begin
            V.Release;
         end;
      end loop;

      if Self.Has_Configuration then
         Self.Conf.Release;
      end if;

      if Self.Has_Runtime_Project then
         Self.Runtime.Release;
      end if;

      Self.Self    := Undefined.Self;
      Self.Root    := Undefined.Root;
      Self.Conf    := Undefined.Conf;
      Self.Runtime := Undefined.Runtime;

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

   procedure Update_Sources (Self : Object) is
   begin
      for V of Self.Views_Set loop
         Definition.Get (V).Update_Sources (V);
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
