--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Iterator_Interfaces;

with GNATCOLL.Directed_Graph;
with GNATCOLL.OS.FS;

with GPR2.Build.Actions;
with GPR2.Build.Artifacts;
with GPR2.Build.External_Options;
with GPR2.Build.Options;
with GPR2.Build.Process_Manager;
with GPR2.Build.View_Db;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Reporter;
with GPR2.Reporter.Holders;
with GPR2.View_Ids;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Indefinite_Ordered_Maps;

limited private with GPR2.Tree_Internal;

package GPR2.Build.Tree_Db is

   type Object is tagged limited private;
   type Object_Access is access all Object;

   Undefined : constant Object;

   package DG renames GNATCOLL.Directed_Graph;

   function Is_Defined (Self : Object) return Boolean;

   --  CREATING/UNLOADING/REFRESHING THE TREE DATABASE:

   procedure Check_Tree (Self : in out Object)
     with Pre => Self.Is_Defined;
   --  Called when the project tree is updated to check that internal
   --  database structure is updated accordingly (proper view databases
   --  added or removed when appropriate).

   procedure Unload (Self : in out Object; Complete : Boolean := True)
     with Post => (if Complete then not Self.Is_Defined);

   procedure Refresh
     (Self     : in out Object;
      Option   : Source_Info_Option;
      Messages : out GPR2.Log.Object)
     with Pre => Self.Is_Defined;

   function Source_Option (Self : Object) return Optional_Source_Info_Option;

   --  VIEW DATABASE LOOKUP:

   function View_Database
     (Self : Object;
      View : GPR2.Project.View.Object) return Build.View_Db.Object
     with Pre => Self.Is_Defined
                   and then View.Is_Defined
                   and then View.Kind not in K_Aggregate | K_Configuration,
          Inline;

   function View_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object
     with Pre => Self.Is_Defined,
          Inline;

   function Ref (Self : Object) return access Object
     with Pre => Self.Is_Defined;

   --  BUILD GRAPH SUPPORT

   function Add_Action
     (Self     : in out Object;
      Action   : in out Actions.Object'Class) return Boolean
     with Pre => Self.Is_Defined;

   procedure Remove_Action
     (Self : in out Object;
      Id   : Actions.Action_Id'Class)
     with Pre => Self.Has_Action (Id)
       and then not Self.Is_Executing;

   function Has_Action
     (Self : Object;
      Id   : Actions.Action_Id'Class) return Boolean
     with Pre => Self.Is_Defined;

   function Action
     (Self : Object;
      Id   : Actions.Action_Id'Class) return Actions.Object'Class
     with Pre => Self.Is_Defined;
   --  ???

   procedure Add_Artifact
     (Self     : in out Object;
      Artifact : Artifacts.Object'Class)
     with Pre => Self.Is_Defined;

   procedure Remove_Artifact
     (Self     : in out Object;
      Artifact : Artifacts.Object'Class);

   procedure Replace_Artifact
     (Self  : in out Object;
      Old   : Artifacts.Object'Class;
      Value : Artifacts.Object'Class);

   function Has_Artifact
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Boolean;

   procedure Add_Input
     (Self     : in out Object;
      Action   : Actions.Action_Id'Class;
      Artifact : Artifacts.Object'Class;
      Explicit : Boolean)
     with Pre => Self.Is_Defined
                   and then Self.Has_Action (Action)
                   and then Artifact.Is_Defined;

   function Add_Output
     (Self     : in out Object;
      Action   : Actions.Action_Id'Class;
      Artifact : Artifacts.Object'Class) return Boolean
     with Pre => Self.Is_Defined
                   and then Self.Has_Action (Action)
                   and then Artifact.Is_Defined;

   function Execute
     (Self    : in out Object;
      PM      : in out GPR2.Build.Process_Manager.Object'Class;
      Options : GPR2.Build.Process_Manager.PM_Options) return Boolean;

   function Is_Executing (Self : Object) return Boolean;

   --  ACTION MANAGEMENT

   function Db_Filename_Path
     (Self       : in out Object;
      Action     : Actions.Action_Id'Class;
      Must_Exist : Boolean) return Path_Name.Object;

   function Propagate_Actions (Self : Object) return Boolean;
   --  Call the On_Tree_Propagation subprogram for each new action of the tree

   procedure Load_Signatures (Self : Object);
   --  Load the actions signatures

   ----------------------------
   -- Iteration on artifacts --
   ----------------------------

   type Artifact_Cursor is private;

   No_Artifact_Element : constant Artifact_Cursor;

   function Has_Element (Position : Artifact_Cursor) return Boolean;

   package Artifact_Iterators is new Ada.Iterator_Interfaces
     (Artifact_Cursor, Has_Element);

   type Artifacts_List (<>) is tagged private
     with Default_Iterator  => Artifact_Iterate,
          Iterator_Element  => Artifacts.Object'Class,
          Constant_Indexing => Constant_Artifact_Reference;

   function Artifact_Iterate
     (List : Artifacts_List) return Artifact_Iterators.Forward_Iterator'Class;

   type Constant_Artifact_Reference_Type
     (Element : not null access constant Artifacts.Object'Class) is private
     with Implicit_Dereference => Element;

   function Constant_Artifact_Reference
     (Iterator : aliased Artifacts_List;
      Pos      : Artifact_Cursor)
      return Constant_Artifact_Reference_Type;

   package Artifact_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (GPR2.Build.Artifacts.Object'Class,
      GPR2.Build.Artifacts.Less, GPR2.Build.Artifacts."=");

   type Action_Cursor is private;
   No_Action_Element : constant Action_Cursor;

   function Has_Element (Position : Action_Cursor) return Boolean;

   package Action_Iterators is new Ada.Iterator_Interfaces
     (Action_Cursor, Has_Element);

   type Actions_List is tagged private
     with Variable_Indexing => Action_Reference,
          Constant_Indexing => Constant_Action_Reference,
          Default_Iterator  => Action_Iterate,
          Iterator_Element  => Actions.Object'Class;

   function Action_Iterate
     (List : Actions_List) return Action_Iterators.Forward_Iterator'Class;

   type Action_Reference_Type
     (Element : not null access Actions.Object'Class) is private
     with Implicit_Dereference => Element;

   function Action_Id_To_Reference
     (Self : in out Object;
      Id   : Actions.Action_Id'Class) return Action_Reference_Type
     with Pre => Self.Is_Defined;

   function Action_Reference
     (Iterator : aliased in out Actions_List;
      Pos      : Action_Cursor) return Action_Reference_Type;

   type Constant_Action_Reference_Type
     (Element : not null access constant Actions.Object'Class) is private
     with Implicit_Dereference => Element;

   function Constant_Action_Reference
     (Iterator : aliased Actions_List;
      Pos      : Action_Cursor)
      return Constant_Action_Reference_Type;

   function Element (Pos : Action_Cursor) return Actions.Object'Class;

   function All_Actions (Self : Object) return Actions_List'Class;

   function Inputs
     (Self          : Object;
      Action        : Actions.Action_Id'Class;
      Explicit_Only : Boolean := False) return Artifacts_List'Class;

   function Outputs
     (Self   : Object;
      Action : Actions.Action_Id'Class) return Artifacts_List'Class;

   function Successors
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Actions_List'Class;

   function Predecessor
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Actions.Object'Class;

   -------------------------
   -- Temp files handling --
   -------------------------

   type Temp_File (Path_Len : Natural) is record
      FD   : GNATCOLL.OS.FS.File_Descriptor;
      Path : Filename_Type (1 .. Path_Len);
   end record;

   function Get_Or_Create_Temp_File
     (Self     : Object;
      For_View : GPR2.Project.View.Object;
      Purpose  : Simple_Name) return Temp_File
   with Pre => For_View.Kind in With_Object_Dir_Kind;

   procedure Clear_Temp_Files (Self : Object);
   --  Make sure all temp files are cleaned up

   -----------------------------------------------
   -- Message reporting for the Build hierarchy --
   -----------------------------------------------

   function Reporter
     (Self : Object) return GPR2.Reporter.Holders.Reference_Type;

   ----------------------
   -- External options --
   ----------------------

   function External_Options
     (Self : Object) return GPR2.Build.External_Options.Object;
   --  Returns the tree external options object

   procedure Set_External_Options
     (Self    : in out Object;
      Options : GPR2.Build.External_Options.Object);

   function Build_Options
     (Self : Object) return GPR2.Build.Options.Build_Options;

   procedure Set_Build_Options
     (Self : in out Object;
      Options : GPR2.Build.Options.Build_Options);

   --------------------------------------
   -- Helper functions for the Actions --
   --------------------------------------

   function Linker_Lib_Dir_Option (Self : Object) return Value_Type;
   --  returns -L for ld and family, or whatever option for the linker
   --  in use for the build.

private

   use type GPR2.Build.Actions.Action_Id'Class;

   function Hash (A : Artifacts.Object'Class) return Ada.Containers.Hash_Type
   is (A.Hash);

   package Build_DB_Maps is new Ada.Containers.Hashed_Maps
     (GPR2.View_Ids.View_Id, Build.View_Db.Object,
      GPR2.View_Ids.Hash, GPR2.View_Ids."=", Build.View_Db."=");

   package Action_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (GPR2.Build.Actions.Action_Id'Class, GPR2.Build.Actions.Object'Class,
      GPR2.Build.Actions."<", GPR2.Build.Actions."=");

   package Action_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (GPR2.Build.Actions.Action_Id'Class,
      GPR2.Build.Actions."<", GPR2.Build.Actions."=");

   package Action_Artifacts_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (GPR2.Build.Actions.Action_Id'Class, Artifact_Sets.Set,
      GPR2.Build.Actions."<", Artifact_Sets."=");

   package Artifact_Actions_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (GPR2.Build.Artifacts.Object'Class, Action_Sets.Set, Hash,
      GPR2.Build.Artifacts."=", Action_Sets."=");

   package Artifact_Action_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (GPR2.Build.Artifacts.Object'Class, Actions.Action_Id'Class, Hash,
      GPR2.Build.Artifacts."=", Actions."=");

   type Object is tagged limited record
   --  Options:
      Src_Option       : Optional_Source_Info_Option := No_Source;
      External_Options : Build.External_Options.Object;
      Build_Options    : Build.Options.Build_Options;

      Self             : access Object;
      --  Handy self-reference

      Tree             : access GPR2.Tree_Internal.Object;
      --  The project tree

      Build_Dbs        : Build_DB_Maps.Map;
      --  Distributed database objects sources from the views

      Actions          : Action_Maps.Map;
      Artifacts        : Artifact_Sets.Set;
      New_Actions      : Action_Sets.Set;

      Inputs           : Action_Artifacts_Maps.Map;
      --  Explicit input(s) in the command line
      Implicit_Inputs  : Action_Artifacts_Maps.Map;
      --  Implicit input(s): included by the explicit ones.
      Outputs          : Action_Artifacts_Maps.Map;
      --  Artifacts produced by a given action

      Successors       : Artifact_Actions_Maps.Map;
      Predecessor      : Artifact_Action_Maps.Map;

      Executing        : Boolean := False;
      Exec_Ctxt        : aliased Process_Manager.Process_Execution_Context;

      Linker_Lib_Dir_Opt : Unbounded_String;
   end record;

   procedure Create
     (Self : in out Object;
      Tree : GPR2.Tree_Internal.Object)
     with Pre => not Self.Is_Defined;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self.Self /= null);

   function Ref (Self : Object) return access Object is
     (Self.Self);

   function Source_Option (Self : Object) return Optional_Source_Info_Option is
     (Self.Src_Option);

   function External_Options
     (Self : Object) return Build.External_Options.Object
   is (Self.External_Options);

   function Build_Options
     (Self : Object) return Build.Options.Build_Options
   is (Self.Build_Options);

   function View_Database
     (Self : Object; View : GPR2.Project.View.Object)
      return Build.View_Db.Object
   is (Self.Build_Dbs.Element (View.Id));

   function View_Database
     (Self : Object;
      View : GPR2.View_Ids.View_Id) return Build.View_Db.Object
   is (Self.Build_Dbs.Element (View));

   function Has_Action
     (Self : Object;
      Id   : Actions.Action_Id'Class) return Boolean
   is (Self.Actions.Contains (Id));

   function Action
     (Self : Object;
      Id   : Actions.Action_Id'Class) return Actions.Object'Class
   is (Self.Actions (Id));

   function Has_Artifact
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Boolean
   is (Self.Artifacts.Contains (Artifact));

   function Is_Executing (Self : Object) return Boolean is
     (Self.Executing);

   type Artifact_List_Kind is (Global_List,
                               Explicit_Inputs,
                               Implicit_Inputs,
                               Inputs,
                               Outputs);

   type Artifact_Cursor is record
      Pos     : Artifact_Sets.Cursor;
      --  Cursor to the artifact object
      Map_Pos : Action_Artifacts_Maps.Cursor;
      --  Cursor to the action->artifacts map element that contains Pos
      Current : Artifact_List_Kind := Global_List;
      --  If Kind is Inputs, this field is used to know if we're currently on
      --  the explicit or implicit list
   end record;

   No_Artifact_Element : constant Artifact_Cursor := (others => <>);

   function Has_Element (Position : Artifact_Cursor) return Boolean
   is (Artifact_Sets.Has_Element (Position.Pos));

   type Constant_Artifact_Reference_Type
     (Element : not null access constant Artifacts.Object'Class)
   is record
      Ref : Artifact_Sets.Constant_Reference_Type (Element);
   end record;

   type Artifacts_List (Kind : Artifact_List_Kind) is tagged record
      Db : access Object;

      case Kind is
         when Global_List =>
            null;
         when others =>
            Action : Action_Maps.Cursor;
      end case;
   end record;

   type Action_Cursor is record
      Pos     : Action_Maps.Cursor;
      Set_Pos : Action_Sets.Cursor;
   end record;

   No_Action_Element : constant Action_Cursor := (others => <>);

   function Has_Element (Position : Action_Cursor) return Boolean
   is (Action_Maps.Has_Element (Position.Pos));

   function Element (Pos : Action_Cursor) return Actions.Object'Class is
     (Action_Maps.Element (Pos.Pos));

   type Action_Reference_Type
     (Element : not null access Actions.Object'Class)
   is record
      Ref : Action_Maps.Reference_Type (Element);
   end record;

   type Constant_Action_Reference_Type
     (Element : not null access constant Actions.Object'Class)
   is record
      Ref : Action_Maps.Constant_Reference_Type (Element);
   end record;

   type Action_List_Kind is (Global_List,
                             Successors);

   type Actions_List is tagged record
      Kind     : Action_List_Kind := Global_List;
      Db       : access Object;
      Artifact : Artifact_Sets.Cursor := Artifact_Sets.No_Element;
   end record;

   function All_Actions (Self : Object) return Actions_List'Class is
     (Actions_List'(Kind     => Global_List,
                    Db       => Self.Self,
                    Artifact => Artifact_Sets.No_Element));

   function Inputs
     (Self : Object;
      Action : Actions.Action_Id'Class;
      Explicit_Only : Boolean := False) return Artifacts_List'Class
   is ((if Explicit_Only
        then Artifacts_List'
          (Kind   => Explicit_Inputs,
           Db     => Self.Self,
           Action => Self.Actions.Find (Action))
        else Artifacts_List'
          (Kind   => Inputs,
           Db     => Self.Self,
           Action => Self.Actions.Find (Action))));

   function Outputs
     (Self   : Object;
      Action : Actions.Action_Id'Class) return Artifacts_List'Class
   is (Artifacts_List'
         (Kind   => Outputs,
          Db     => Self.Self,
          Action => Self.Actions.Find (Action)));

   function Predecessor
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Actions.Object'Class
   is (Self.Actions (Self.Predecessor (Artifact)));

   function Successors
     (Self     : Object;
      Artifact : Artifacts.Object'Class) return Actions_List'Class
   is (Actions_List'
         (Kind     => Successors,
          Db       => Self.Self,
          Artifact => Self.Artifacts.Find (Artifact)));

end GPR2.Build.Tree_Db;
