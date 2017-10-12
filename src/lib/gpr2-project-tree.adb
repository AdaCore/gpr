------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2017, Free Software Foundation, Inc.          --
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

with Ada.Environment_Variables;

with GPR.Sdefault;

with GPR2.Parser.Project.Create;
with GPR2.Project.Attribute.Set;
with GPR2.Project.Import.Set;
with GPR2.Project.Name_Values;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source;
with GPR2.Project.View.Set;
with GPR2.Source_Reference;
with GPR2.Unit;

with GNAT.OS_Lib;

package body GPR2.Project.Tree is

   use Ada;

   type Iterator (Kind : Iterator_Kind; Filter : Project_Filter) is
     new Project_Iterator.Forward_Iterator with
   record
     Root : not null access constant Object;
   end record;

   overriding function First
     (Iter : Iterator) return Cursor;

   overriding function Next
     (Iter : Iterator; Position : Cursor) return Cursor;

   function Recursive_Load
     (Self         : Object;
      Filename     : Path_Name_Type;
      Context_View : View.Object;
      Status       : Definition.Relation_Status;
      Root_Context : out GPR2.Context.Object;
      Messages     : out Log.Object) return View.Object;
   --  Load a project filename recurivelly and returns the corresponding root
   --  view.

   function Create_Runtime_View (Self : Object) return View.Object
     with Pre => Self /= Undefined
                 and then Self.Configuration_Project /= View.Undefined;
   --  Create the runtime view given the configuration project

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
      use type Project.Source.Object;

      --  If the spec is not present, then the actual source object used is the
      --  first body which must exist. We can't have no spec and no body.

      Src : constant Project.Source.Object :=
              (if Unit.Spec = Project.Source.Undefined
               then Unit.Bodies.First_Element
               else Unit.Spec);
   begin
      --  Clear the unit

      Self.Units.Exclude (Src.Source.Unit_Name);

      --  Clear the corresponding sources

      Self.Sources.Exclude (Name_Type (Src.Source.Filename));

      for B of Unit.Bodies loop
         Self.Sources.Exclude (Name_Type (B.Source.Filename));
      end loop;
   end Clear_View;

   ---------------------------
   -- Configuration_Project --
   ---------------------------

   function Configuration_Project (Self : Object) return View.Object is
   begin
      return Self.Conf.Corresponding_View;
   end Configuration_Project;

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
           Definition.Project_View_Store.Constant_Reference
             (Position.Views, Position.Current).Element);
   end Constant_Reference;

   -------------
   -- Context --
   -------------

   function Context (Self : Object) return GPR2.Context.Object is
   begin
      return Self.Root_Project.Context;
   end Context;

   -------------------------
   -- Create_Runtime_View --
   -------------------------

   function Create_Runtime_View (Self : Object) return View.Object is
      CV   : constant View.Object := Self.Conf.Corresponding_View;
      DS   : Character renames GNAT.OS_Lib.Directory_Separator;
      Data : Project.Definition.Data (Has_Context => False);
   begin
      --  Check runtime path

      if CV.Has_Attributes ("runtime_dir", "ada") then
         --  Runtime_Dir (Ada) exists, this is the Source_Dirs for the Runtime
         --  project view.

         declare
            Runtime_Dir : constant String :=
                            CV.Attribute ("runtime_dir", "ada").Value;
         begin
            Data.Attrs.Insert
              (Project.Attribute.Create
                 (Name  => Project.Registry.Attribute.Source_Dirs,
                  Value => Runtime_Dir & DS & "adainclude",
                  Sloc  => Source_Reference.Undefined));
         end;

         --  The only language supported is Ada

         Data.Attrs.Insert
           (Project.Attribute.Create
              (Name  => Project.Registry.Attribute.Languages,
               Value => "ada",
               Sloc  => Source_Reference.Undefined));

         Data.Tree   := Self.Self;
         Data.Status := Definition.Root;
         Data.Kind   := K_Standard;

         Data.Trees.Project := Parser.Project.Create
           (Name      => "Runtime",
            File      => Create_File ("runtime.gpr"),
            Qualifier => K_Standard);

         return Project.Definition.Register (Data);

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
   -- First --
   -----------

   overriding function First (Iter : Iterator) return Cursor is

      Seen : GPR2.Project.View.Set.Object;
      --  Keep track of already seen projects. Better than using the P vector
      --  which is not efficient when checking if an element exists.

      Projects : Definition.Project_View_Store.Vector;
      --  Set of projects for the iterator which is returned in the Cursor and
      --  fill by the recursive procedure For_Project and For_Imports.

      procedure Append (View : Project.View.Object)
        with Post => Seen.Contains (View);
      --  Append into P if not already seen

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
         Qualifier : constant Project_Kind := View.Kind;
      begin
         if not Seen.Contains (View) then
            Seen.Insert (View);

            --  Check if it corresponds to the current filter
            if (Qualifier = K_Library
                and then Is_Set (Iter.Filter, F_Library))
              or else (Qualifier = K_Standard
                       and then Is_Set (Iter.Filter, F_Standard))
              or else (Qualifier = K_Abstract
                       and then Is_Set (Iter.Filter, F_Abstract))
              or else (Qualifier = K_Aggregate
                       and then Is_Set (Iter.Filter, F_Aggregate))
              or else (Qualifier = K_Aggregate_Library
                       and then Is_Set (Iter.Filter, F_Aggregate_Library))
            then
               Projects.Append (View);
            end if;
         end if;
      end Append;

      --------------------
      -- For_Aggregated --
      --------------------

      procedure For_Aggregated (View : Project.View.Object) is
      begin
         if View.Kind in K_Aggregate | K_Aggregate_Library then
            for A of Definition.Get (View).Aggregated loop
               Append (A);
            end loop;
         end if;
      end For_Aggregated;

      -----------------
      -- For_Imports --
      -----------------

      procedure For_Imports (View : Project.View.Object) is
      begin
         for I of Definition.Get (View).Imports loop
            if Is_Set (Iter.Kind, I_Recursive) then
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
            --  Handle imports

            if Is_Set (Iter.Kind, I_Imported)
              or else Is_Set (Iter.Kind, I_Recursive)
            then
               For_Imports (View);
            end if;

            --  Handle extended if any

            if Is_Set (Iter.Kind, I_Extended) then
               declare
                  Data : constant Definition.Data := Definition.Get (View);
               begin
                  if Data.Extended /= Project.View.Undefined then
                     if Is_Set (Iter.Kind, I_Recursive) then
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

            if Is_Set (Iter.Kind, I_Aggregated) then
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

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Self   : Object;
      Source : Path_Name_Type) return Project.View.Object
   is
      Pos : Name_View.Cursor :=
              Self.Sources.Find (Name_Type (Value (Source)));
   begin
      if Name_View.Has_Element (Pos) then
         return Name_View.Element (Pos);

      else
         --  Try to update sources and check again

         Update_Sources (Self);
         Pos := Self.Sources.Find (Name_Type (Value (Source)));

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

   -------------------------------
   -- Has_Configuration_Project --
   -------------------------------

   function Has_Configuration_Project (Self : Object) return Boolean is
      use type Project.Configuration.Object;
   begin
      return Self.Conf /= Project.Configuration.Undefined
        and then Self.Conf.Corresponding_View /= View.Undefined;
   end Has_Configuration_Project;

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
      return Self.Runtime /= View.Undefined;
   end Has_Runtime_Project;

   ------------------------
   -- Invalidate_Sources --
   ------------------------

   procedure Invalidate_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined) is
   begin
      for V of Self loop
         if View in Project.View.Undefined | V then
            V.Invalidate_Sources;
         end if;
      end loop;
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
      Kind   : Iterator_Kind := I_Default;
      Filter : Project_Filter := F_Default)
      return Project_Iterator.Forward_Iterator'Class is
   begin
      return Iterator'(Kind, Filter, Self.Self);
   end Iterate;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self     : in out Object;
      Filename : Path_Name_Type;
      Context  : GPR2.Context.Object;
      Config   : Configuration.Object := Configuration.Undefined)
   is
      function Has_Error return Boolean is
        (Self.Messages.Has_Element
           (Error       => True,
            Information => False,
            Warning     => False,
            Read        => False,
            Unread      => True));

      Root_Context : GPR2.Context.Object := Context;

   begin
      Self.Root := Recursive_Load
        (Self, Filename, View.Undefined, Definition.Root,
         Root_Context, Self.Messages);

      --  Do nothing more if there are errors during the parsing

      if not Has_Error then
         --  Set configuration project if any

         Self.Conf := Config;

         for View of Self loop
            declare
               V_Data : Definition.Data := Definition.Get (View);
            begin
               --  Compute the external dependencies for the views. This
               --  is the set of external used in the project and in all
               --  imported project.

               for E of V_Data.Externals loop
                  if not V_Data.Externals.Contains (E) then
                     V_Data.Externals.Append (E);
                  end if;
               end loop;

               Definition.Set (View, V_Data);
            end;
         end loop;

         Set_Context (Self, Context);

         if Has_Error then
            raise Project_Error with Value (Filename) & " semantic error";
         end if;

      else
         raise Project_Error with Value (Filename) & " syntax error";
      end if;
   end Load;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration
     (Self     : in out Object;
      Filename : Path_Name_Type) is
   begin
      Self.Conf := Configuration.Create (Filename);

      if Self.Conf.Has_Messages then
         for M of Self.Conf.Log_Messages loop
            Self.Messages.Append (M);
         end loop;

      else
         Set_Context (Self, Self.Context);
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

   -----------------
   -- Record_View --
   -----------------

   procedure Record_View
     (Self   : in out Object;
      View   : GPR2.Project.View.Object;
      Source : Full_Path_Name;
      Unit   : Name_Type) is
   begin
      Self.Units.Include (Unit, View);
      Self.Sources.Include (Name_Type (Source), View);
   end Record_View;

   --------------------
   -- Recursive_Load --
   --------------------

   function Recursive_Load
     (Self         : Object;
      Filename     : Path_Name_Type;
      Context_View : View.Object;
      Status       : Definition.Relation_Status;
      Root_Context : out GPR2.Context.Object;
      Messages     : out Log.Object) return View.Object

   is
      function Load (Filename : Path_Name_Type) return Definition.Data;
      --  Returns the Data definition for the given project

      function Internal
        (Self         : Object;
         Filename     : Path_Name_Type;
         Context_View : View.Object;
         Status       : Definition.Relation_Status;
         Root_Context : out GPR2.Context.Object;
         Messages     : out Log.Object) return View.Object;

      Sets  : Project.Import.Set.Object;
      Paths : Containers.Path_Name_List;

      --------------
      -- Internal --
      --------------

      function Internal
        (Self         : Object;
         Filename     : Path_Name_Type;
         Context_View : View.Object;
         Status       : Definition.Relation_Status;
         Root_Context : out GPR2.Context.Object;
         Messages     : out Log.Object) return View.Object
      is
         View : Project.View.Object :=
                  Definition.Get (Filename, Context_View, Status, Self);
      begin

         if View = Project.View.Undefined then
            declare
               use type Definition.Relation_Status;
               Data : Definition.Data := Load (Filename);
            begin
               --  If there are parsing errors, do not go further

               if Messages.Has_Element
                 (Information => False, Warning => False)
               then
                  return View;
               end if;

               --  Let's setup the full external environment for project

               for E of Data.Externals loop
                  --  Fill all known external in the environment variables
                  if not Root_Context.Contains (E)
                    and then Environment_Variables.Exists (String (E))
                  then
                     Root_Context.Include
                       (E, Environment_Variables.Value (String (E)));
                  end if;
               end loop;

               --  If we have the root project, record the global context

               if Data.Has_Context and then Status = Definition.Root
               --                and then Context_View = Project.View.Undefined
               then
                  --  This is the root-view, assign the corresponding context
                  Data.Context := Root_Context;
               end if;

               --  Create the view, needed to be able to reference it if it is
               --  an aggregate project as it becomes the new View_Context.

               Data.Context_View := Context_View;
               Data.Status       := Status;

               View := Definition.Register (Data);

               --  Load the extended project if any

               if Data.Trees.Project.Has_Extended then
                  Data.Extended :=
                    Internal
                      (Self,
                       Create
                         (Name_Type (Value (Data.Trees.Project.Extended)),
                          GPR2.Project.Paths (Filename)),
                       Context_View => View,
                       Status       => Definition.Imported,
                       Root_Context => Root_Context,
                       Messages     => Messages);
               end if;

               --  Now load all imported projects. If we are parsing the root
               --  project or an aggregate project then the context view become
               --  this project.

               for Project of Data.Trees.Imports loop
                  if not Data.Trees.Project.Imports.Element
                    (Project.Path_Name).Is_Limited
                  then
                     if Sets.Contains (Project.Path_Name) then
                        Messages.Append
                          (Message.Create
                             (Message.Error,
                              "circular dependency detected",
                              Source_Reference.Object
                                (Data.Trees.Project.Imports.Element
                                     (Project.Path_Name))));

                        for Import of Paths loop
                           Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "imports " & Value (Import),
                                 Source_Reference.Object
                                   (Sets.Element (Import))));
                        end loop;

                     else
                        Sets.Insert
                          (Project.Path_Name,
                           Data.Trees.Project.Imports.Element
                             (Project.Path_Name));
                        Paths.Append (Project.Path_Name);

                        Data.Imports.Append
                          (Internal
                             (Self,
                              Project.Path_Name,
                              Context_View =>
                                (if Status = Definition.Root
                                 then View
                                 else Context_View),
                              Status       => Definition.Imported,
                              Root_Context => Root_Context,
                              Messages     => Messages));

                        Sets.Delete (Project.Path_Name);
                        Paths.Delete_Last;
                     end if;
                  end if;
               end loop;

            --  And record back new data for this view

               Definition.Set (View, Data);
            end;
         end if;

         return View;
      end Internal;

      ----------
      -- Load --
      ----------

      function Load (Filename : Path_Name_Type) return Definition.Data is
         Paths   : constant Containers.Name_List :=
                     GPR2.Project.Paths (Filename);
         Project : constant Parser.Project.Object :=
                     Parser.Project.Load (Filename, Messages);
         Data    : Definition.Data
                     (Has_Context =>
                        (Context_View = GPR2.Project.View.Undefined)
                      or else Project.Qualifier = K_Aggregate);
      begin
         Data.Trees.Project := Project;

         --  Record the project tree for this view

         Data.Tree := Self.Self;

         --  Do the following only if there are no error messages

         if Messages.Is_Empty then
            Data.Kind := Project.Qualifier;
            Data.Externals := Data.Trees.Project.Externals;

            --  Now load all imported projects if any

            for Import of Data.Trees.Project.Imports loop
               declare
                  Import_Filename : constant Path_Name_Type :=
                                      Create
                                        (Name_Type (Value (Import.Path_Name)),
                                         Paths);
               begin
                  Data.Trees.Imports.Insert
                    (Import_Filename,
                     Parser.Project.Load (Import_Filename, Messages));
               end;
            end loop;
         end if;

         return Data;
      end Load;

   begin
      return Internal
        (Self, Filename, Context_View, Status, Root_Context, Messages);
   end Recursive_Load;

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
      if Self.Has_Configuration_Project
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
      procedure Set_View (View : Project.View.Object);
      --  Set the context for the given view

      procedure Validity_Check (View : Project.View.Object);
      --  Do validity check on the given view

      --------------
      -- Set_View --
      --------------

      procedure Set_View (View : Project.View.Object) is
         use type GPR2.Context.Binary_Signature;

         function Is_In_Closure
           (Project_Name : Path_Name_Type;
            Data         : Definition.Data;
            Messages     : out Log.Object) return Boolean;
         --  Returns True if Project_Name is in the closure of the project
         --  whose Data definition is given. That is, the project data is
         --  containing a reference to the Project_Name.

         -------------------
         -- Is_In_Closure --
         -------------------

         function Is_In_Closure
           (Project_Name : Path_Name_Type;
            Data         : Definition.Data;
            Messages     : out Log.Object) return Boolean
         is

            function Is_In_Imports (Data : Definition.Data) return Boolean;
            --  True if Project_Name is in imports of Data

            -------------------
            -- Is_In_Imports --
            -------------------

            function Is_In_Imports (Data : Definition.Data) return Boolean is
            begin
               for Import of Data.Imports loop
                  --  Skip limited imports
                  if not Data.Trees.Project.Imports.Element
                    (Import.Path_Name).Is_Limited
                  then
                     if Is_In_Imports (Definition.Get (Import))
                       or else Project_Name = Import.Path_Name
                     then
                        Messages.Append
                          (Message.Create
                             (Message.Error,
                              "imports " & Value (Import.Path_Name),
                              Source_Reference.Object
                                (Data.Trees.Project.Imports.Element
                                     (Import.Path_Name))));
                        return True;
                     end if;
                  end if;
               end loop;

               return False;
            end Is_In_Imports;

         begin
            return Is_In_Imports (Data);
         end Is_In_Closure;

         P_Data        : Definition.Data := Definition.Get (View);
         Old_Signature : constant GPR2.Context.Binary_Signature :=
                           P_Data.Signature;
         New_Signature : constant GPR2.Context.Binary_Signature :=
                           Context.Signature (P_Data.Externals);
         Context       : constant GPR2.Context.Object :=
                           View.Context;
         Paths         : Containers.Name_List;
      begin
         Parser.Project.Parse
           (P_Data.Trees.Project,
            Self,
            Context,
            P_Data.Attrs,
            P_Data.Vars,
            P_Data.Packs);

         --  Now we can record the aggregated projects based on the possibly
         --  new Project_Files attribute value.

         if View.Qualifier in K_Aggregate | K_Aggregate_Library then
            P_Data.Aggregated.Clear;

            --  Pathname for Project_Files projects are relative to the
            --  aggregate project only.

            Paths.Append (Name_Type (Dir_Name (View.Path_Name)));

            for Project of
              P_Data.Attrs.Element (Registry.Attribute.Project_Files).Values
            loop
               declare
                  Pathname : constant Path_Name_Type :=
                               Create (Name_Type (Project), Paths);
               begin
                  if Pathname = View.Path_Name then
                     --  We are loading recursively the aggregate project

                     Self.Messages.Append
                       (Message.Create
                          (Message.Error,
                           "project cannot aggregate itself "
                           & String (Base_Name (Pathname)),
                           Source_Reference.Object
                             (P_Data.Attrs.Element
                                  (Registry.Attribute.Project_Files))));

                  else
                     declare
                        Ctx      : GPR2.Context.Object;
                        A_View   : constant GPR2.Project.View.Object :=
                                     Recursive_Load
                                       (Self,
                                        Pathname, View,
                                        Definition.Aggregated,
                                        Ctx,
                                        Self.Messages);
                        Messages : Log.Object;
                     begin
                        --  If there was error messages during the parsing of
                        --  the aggregated project, just return now.

                        if Self.Messages.Has_Element
                          (Information => False,
                           Warning     => False)
                        then
                           return;
                        end if;

                        --  Now check that the aggregated project has no cyclic
                        --  dependencies.

                        if Is_In_Closure
                          (P_Data.Trees.Project.Path_Name,
                           Definition.Get (A_View),
                           Messages)
                        then
                           Self.Messages.Append
                             (Message.Create
                                (Message.Error,
                                 "circular dependency detected",
                                 Source_Reference.Object
                                   (P_Data.Attrs.Element
                                        (Registry.Attribute.Project_Files))));

                           for M of Messages loop
                              Self.Messages.Append (M);
                           end loop;
                        end if;

                        --  Record aggregated view into the aggregate's view

                        P_Data.Aggregated.Append (A_View);

                        --  And set the aggregated view recursivelly
                        Set_View (A_View);
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
                       (Name_Type (External.Index), External.Value);
                  end if;
               end;
            end loop;
         end if;

         P_Data.Signature := New_Signature;

         --  Let's compute the project kind if needed. A project without
         --  an explicit qualifier may actually be a library project if
         --  Library_Name, Library_Kind is declared.

         P_Data.Kind := P_Data.Trees.Project.Qualifier;

         if P_Data.Kind = K_Standard then
            if P_Data.Attrs.Contains (Registry.Attribute.Library_Kind)
              or else P_Data.Attrs.Contains (Registry.Attribute.Library_Name)
              or else P_Data.Attrs.Contains (Registry.Attribute.Library_Dir)
            then
               P_Data.Kind := K_Library;
            end if;
         end if;

         Definition.Set (View, P_Data);

         --  Signal project change only if we have different and non default
         --  signature. That is if there is at least some external used
         --  otherwise the project is stable and won't change.

         if Old_Signature /= New_Signature
           and then P_Data.Signature /= GPR2.Context.Default_Signature
           and then Changed /= null
         then
            Changed (View);
         end if;
      end Set_View;

      --------------------
      -- Validity_Check --
      --------------------

      procedure Validity_Check (View : Project.View.Object) is
         use type Registry.Attribute.Index_Kind;
         use type Registry.Attribute.Value_Kind;

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
                     "attribute " & String (A.Name) & " cannot have index",
                     Source_Reference.Object (A)));
            end if;

            if Def.Value = Registry.Attribute.Single
              and then A.Kind = Registry.Attribute.List
            then
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute " & String (A.Name) & " cannot be a list",
                     Source_Reference.Object (A)));
            end if;

            if Def.Value = Registry.Attribute.List
              and then A.Kind = Registry.Attribute.Single
            then
               Self.Messages.Append
                 (Message.Create
                    (Message.Error,
                     "attribute " & String (A.Name) & " must be a list",
                     Source_Reference.Object (A)));
            end if;
         end Check_Def;

         P_Kind : constant Project_Kind := View.Kind;
         P_Data : constant Definition.Data := Definition.Get (View);

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
                        Source_Reference.Object (P)));
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
                                 "attribute " & String (A.Name)
                                 & " cannot be used in package "
                                 & String (P.Name),
                                 Source_Reference.Object (A)));
                        end if;

                        Check_Def (Def, A);

                     else
                        Self.Messages.Append
                          (Message.Create
                             (Message.Error,
                              "attribute " & String (A.Name)
                              & " not supported in package " & String (P.Name),
                              Source_Reference.Object (A)));
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
               if not Registry.Attribute.Get
                 (Q_Name).Is_Allowed_In (P_Kind)
               then
                  Self.Messages.Append
                    (Message.Create
                       (Message.Error,
                        "attribute " & String (A.Name)
                        & " cannot be used in " & Image (P_Kind),
                        Source_Reference.Object (A)));
               end if;

               Check_Def (Registry.Attribute.Get (Q_Name), A);
            end;
         end loop;
      end Validity_Check;

   begin
      --  Register the root context for this project tree

      declare
         Data : Definition.Data := Definition.Get (Self.Root_Project);
      begin
         Data.Context := Context;
         Definition.Set (Self.Root_Project, Data);
      end;

      --  Now the first step is to set the configuration project view if any
      --  and to create the runtime project if possible.

      if Self.Has_Configuration_Project then
         Set_View (Self.Conf.Corresponding_View);

         Self.Runtime := Create_Runtime_View (Self);
      end if;

      --  Propagate the change in the project Tree. That is for each project in
      --  the tree we need to update the corresponding view. We do not handle
      --  the aggregated projects here. Those projects are handled specifically
      --  in Set_View. This is needed as parsing the aggregate project may
      --  change the Project_Files attribute and so the actual aggregated
      --  project. So we cannot use the current aggregated project list.

      for View in Self.Iterate
        (Kind => I_Project or I_Extended or I_Imported or I_Recursive)
      loop
         Set_View (Element (View));
      end loop;

      --  We now have an up-to-date tree, do some validity checks if there is
      --  no issue detected yet.

      if Self.Messages.Is_Empty then
         for View of Self loop
            Validity_Check (View);
         end loop;
      end if;
   end Set_Context;

   ------------
   -- Target --
   ------------

   function Target (Self : Object) return Name_Type is
   begin
      if Self.Has_Configuration_Project
        and then Self.Conf.Target /= ""
      then
         return Self.Conf.Target;

      elsif Self.Has_Configuration_Project
        and then Self.Conf.Corresponding_View.Has_Attributes
                   (Registry.Attribute.Target)
      then
         return Name_Type
           (Self.Conf.Corresponding_View.Attribute
              (Registry.Attribute.Target).Value);

      else
         return Name_Type (GPR.Sdefault.Hostname);
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

      if Self.Has_Configuration_Project then
         Self.Conf.Release;
      end if;

      if Self.Has_Runtime_Project then
         Self.Runtime.Release;
      end if;

      Self.Self    := Undefined.Self;
      Self.Root    := Undefined.Root;
      Self.Conf    := Undefined.Conf;
      Self.Runtime := Undefined.Runtime;
   end Unload;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources (Self : Object) is
   begin
      for V of Self loop
         V.Update_Sources;
      end loop;
   end Update_Sources;

   --------------
   -- View_For --
   --------------

   function View_For
     (Self : Object;
      Name : Name_Type;
      Ctx  : GPR2.Context.Object) return View.Object
   is
      use type GPR2.Context.Binary_Signature;

      CV : constant View.Object :=
             (if Self.Has_Configuration_Project
              then Self.Conf.Corresponding_View
              else View.Undefined);

   begin
      --  First check for the view in the current tree

      for View of Self.Self.all loop
         if View.Name = Name then
            declare
               P_Data : constant Definition.Data := Definition.Get (View);
               P_Sig  : constant GPR2.Context.Binary_Signature :=
                          Ctx.Signature (P_Data.Externals);
            begin
               if View.Signature = P_Sig then
                  return View;
               end if;
            end;
         end if;
      end loop;

      --  If not found let's check if it is the configuration or runtime
      --  project. Note that this means that any Runtime or Config user's
      --  project name will have precedence.

      if CV /= View.Undefined and then CV.Name = Name then
         return CV;

      elsif Self.Has_Runtime_Project
        and then Self.Runtime.Name = Name
      then
         return Self.Runtime;

      else
         return View.Undefined;
      end if;
   end View_For;

end GPR2.Project.Tree;
