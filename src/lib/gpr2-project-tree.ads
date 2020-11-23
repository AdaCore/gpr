------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--                    Copyright (C) 2019-2020, AdaCore                      --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Iterator_Interfaces;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Context;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Configuration;
pragma Elaborate (GPR2.Project.Configuration);
--  Elaborate to avoid a circular dependency due to default Elaborate_Body
with GPR2.Project.View.Set;
with GPR2.Project.Source;
with GPR2.Project.Unit_Info;
with GPR2.Project.Registry.Attribute;

pragma Warnings (Off);
with System.OS_Constants;
pragma Warnings (On);

private with Ada.Containers.Vectors;

package GPR2.Project.Tree is

   use GPR2.Context;

   use type GPR2.Context.Object;
   use type GPR2.Project.View.Object;

   type Object is tagged limited private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => View.Object;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Is_Windows_Target (Tree : Object) return Boolean
     with Pre => Tree.Is_Defined;
   --  Returns true if tree's target is window

   function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are the same tree

   procedure Load
     (Self             : in out Object;
      Filename         : Path_Name.Object;
      Context          : GPR2.Context.Object;
      Config           : Configuration.Object    := Configuration.Undefined;
      Project_Dir      : Path_Name.Object        := Path_Name.Undefined;
      Build_Path       : Path_Name.Object        := Path_Name.Undefined;
      Subdirs          : Optional_Name_Type      := No_Name;
      Src_Subdirs      : Optional_Name_Type      := No_Name;
      Check_Shared_Lib : Boolean                 := True;
      Absent_Dir_Error : Boolean                 := False;
      Implicit_With    : Containers.Filename_Set :=
                           Containers.Empty_Filename_Set)
     with Pre => Filename.Is_Defined
                 and then (not Filename.Is_Implicit_Project
                           or else Project_Dir.Is_Defined);
   --  Loads a root project
   --  If Implicit_Project is True, the main project file being parsed is
   --  deemed to be in the current working directory, even if it is not the
   --  case. Implicit_Project is set to True when a gpr tool is invoked without
   --  a project file and is using an implicit project file that is virtually
   --  in the current working directory, but is physically in another
   --  directory.

   procedure Load_Configuration
     (Self     : in out Object;
      Filename : Path_Name.Object)
     with Pre => Filename.Is_Defined;
   --  Loads a configuration project for this tree

   procedure Load_Autoconf
     (Self              : in out Object;
      Filename          : Path_Name.Object;
      Context           : GPR2.Context.Object;
      Project_Dir       : Path_Name.Object     := Path_Name.Undefined;
      Build_Path        : Path_Name.Object     := Path_Name.Undefined;
      Subdirs           : Optional_Name_Type   := No_Name;
      Src_Subdirs       : Optional_Name_Type   := No_Name;
      Check_Shared_Lib  : Boolean              := True;
      Absent_Dir_Error  : Boolean              := False;
      Implicit_With     : Containers.Filename_Set :=
                            Containers.Empty_Filename_Set;
      Target            : Optional_Name_Type       := No_Name;
      Language_Runtimes : Containers.Name_Value_Map :=
                            Containers.Name_Value_Map_Package.Empty_Map;
      Base              : GPR2.KB.Object       := GPR2.KB.Undefined)
       with Pre => Filename.Is_Defined;
   --  Loads a tree in autoconf mode.
   --  If Target is specified, then we use it directly instead of fetching
   --  the root project attribute.
   --  Same with the Language_Runtime map: for each language Lang in the
   --  project tree, if the map has an association (Lang,RTS) then we use it
   --  instead of any attribute Runtime (Lang) declared in the root project.
   --  Typically this is useful to enforce precedence of the command-line
   --  options --target and --RTS[:lang].
   --  If Project_Dir is defined, the main project file being parsed is deemed
   --  to be in this directory, even if it is not the case. Project_Dir is
   --  defined when a gpr tool is invoked without a project file and is using
   --  an implicit project file that is virtually in the Project_Dir, but is
   --  physically in another directory.
   --  Base is the knowledge base object used to configure the toolchain for
   --  the project.

   procedure Unload (Self : in out Object);
   --  Unloads the tree and free all associated objects (projects, sources,
   --  etc...).

   function Root_Project (Self : Object) return View.Object
     with Pre  => Self.Is_Defined,
          Post => Root_Project'Result.Is_Defined;
   --  Returns the root project for the given tree

   function Has_Configuration (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if a configuration project is loaded on this tree

   function Configuration (Self : Object) return Configuration.Object
     with Pre => Self.Is_Defined and then Self.Has_Configuration;
   --  Returns the configuration project for the given tree

   function Has_Runtime_Project (Self : Object) return Boolean;
   --  Returns True if a runtime project is loaded on this tree

   function Runtime_Project (Self : Object) return View.Object
     with Pre => Self.Is_Defined and then Self.Has_Runtime_Project;
   --  Returns the runtime project for the given tree

   function Target (Self      : Object;
                    Canonical : Boolean := False) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the target for the project tree

   function Runtime
     (Self : Object; Language : Name_Type) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the runtime selected for the given language or the empty string
   --  if no specific runtime has been configured for this project tree.

   function Has_View_For
     (Self    : Object;
      Name    : Name_Type;
      Context : Context_Kind) return Boolean;
   --  Returns True if the project Name is found on the tree (see below).
   --  Context paramter defines where the view going to be looked up in the
   --  root or in the aggregate context.

   function View_For
     (Self    : Object;
      Name    : Name_Type;
      Context : Context_Kind) return View.Object
     with Pre => Self.Is_Defined and then Self.Has_View_For (Name, Context);
   --  Returns the project's view in the tree which corresponds to project name
   --  and that is matching the Aggregated context.
   --  Context paramter defines is the view going to be taken from the root
   --  context or from the aggregate context.

   function Has_Messages (Self : Object) return Boolean;
   --  Returns whether some messages are present for this project tree

   function Log_Messages (Self : Object) return not null access Log.Object
     with Post => not Self.Has_Messages
                  or else not Log_Messages'Result.Is_Empty;
   --  Returns the Logs, this contains information, warning and error messages
   --  found while handling the project.

   procedure Append_Message
     (Self    : in out Object;
      Message : GPR2.Message.Object)
     with Pre  => Self.Is_Defined,
          Post => Self.Log_Messages.Count in Self.Log_Messages.Count'Old
                                          .. Self.Log_Messages.Count'Old + 1;
   --  Adds new message into the Log of Self, does nothing if message already
   --  present.

   --  Context

   --  Note that the context of the project tree corresponds to the context of
   --  the root project view.

   function Has_Context (Self : Object) return Boolean
     with Pre  => Self.Is_Defined;
   --  Returns True if the project tree has some context. If any of the project
   --  in the tree has some external variables then a context is present. A
   --  project without context is fully static has it does not reference any
   --  external (and so modifiable) variables.

   function Context (Self : Object) return Context.Object
     with Pre  => Self.Is_Defined,
          Post => Self.Has_Context = (Context'Result /= GPR2.Context.Empty);
   --  Returns the Context for the given project tree

   function Add_Tool_Prefix
     (Self      : Object;
      Tool_Name : Name_Type) return Name_Type
     with Pre => Self.Is_Defined;
   --  Returns Tool_Name for native compilation otherwise
   --  it returns the tool prefixed with the target-triplet,
   --  for example x86_64-linux-gnu-gcc.

   procedure Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null)
     with Pre => Self.Is_Defined;
   --  Sets the context for the project tree. The callback Changed is called
   --  for any project view which is impacted by this change of context, i.e.
   --  if the project view references directly or indirectly an external
   --  variable.

   --  Iterator

   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : Cursor) return View.Object
     with Post =>
       (if Has_Element (Position)
        then Element'Result.Is_Defined
        else not Element'Result.Is_Defined);

   function Has_Element (Position : Cursor) return Boolean;

   package Project_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Is_Root (Position : Cursor) return Boolean;
   --  Returns True if the cursor is pointing to the root project

   type Constant_Reference_Type
     (View : not null access constant Project.View.Object) is private
     with Implicit_Dereference => View;

   function Constant_Reference
     (Self     : aliased Object;
      Position : Cursor) return Constant_Reference_Type
     with Pre => Self.Is_Defined and then Position /= No_Element;

   function Iterate
     (Self   : Object;
      Kind   : Iterator_Control := Default_Iterator;
      Filter : Filter_Control   := Default_Filter;
      Status : Status_Control   := Default_Status)
      return Project_Iterator.Forward_Iterator'Class
     with Pre => Self.Is_Defined;
   --  Iterates over all project views in the tree given the iterator kind
   --  (only the project with or without imports) and the filter which can be
   --  used to iterate over only some specific projects (only the library
   --  projects for example).

   --  Unit/View

   procedure Record_View
     (Self   : in out Object;
      View   : GPR2.Project.View.Object;
      Source : Path_Name.Object;
      Unit   : Name_Type)
     with Pre => Self.Is_Defined;
   --  Records the view in which unit is defined

   procedure Clear_View
     (Self : in out Object;
      Unit : Unit_Info.Object)
     with Pre => Self.Is_Defined;
   --  Clears the view set for the given unit

   function Get_View
     (Self : Object;
      Unit : Name_Type) return Project.View.Object
     with Pre => Self.Is_Defined;
   --  Gets the view in which unit is defined, returns Undefined if the unit
   --  has not been found.

   function Get_View
     (Self   : Object;
      Source : Path_Name.Object) return Project.View.Object
     with Pre => Self.Is_Defined;
   --  Gets the view in which source file is defined, returns Undefined if the
   --  source file has not been found.

   procedure Invalidate_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined)
     with Pre => Self.Is_Defined;
   --  Invalidates the sources for all views in the tree if View is set or the
   --  source in the given view otherwise. This is needed when some sources
   --  are added or removed from the view. It is not required to call
   --  Update_Sources below, when the routine Sources on one of the views
   --  of the tree will be called, the set of sources will be recomputed.

   procedure Update_Sources
     (Self          : Object;
      Stop_On_Error : Boolean := True;
      With_Runtime  : Boolean := False)
     with Pre => Self.Is_Defined;
   --  Ensures that all views' sources are up-to-date. This is needed before
   --  computing the dependencies of a source in the project tree. This routine
   --  is called where needed and is there for internal use only.
   --  If Stop_On_Error is True then exception Project_Error is raised in case
   --  an error message exists in log after processing one of project files in
   --  tree. If Stop_On_Error is False then no exception is raised and errors
   --  can be discovered from the Log.Object taken from Log_Messages call.

   procedure Register_Project_Search_Path
     (Self : in out Object;
      Dir  : Path_Name.Object)
     with Pre => Dir.Is_Defined;
   --  Adds a project search path for this tree

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object;
   --  Returns the Tree project search paths

   function Archive_Suffix (Self : Object) return Filename_Type;
   --  Returns archive suffix for the project tree

   function Object_Suffix
     (Self : Object; Language : Name_Type := "ada") return Filename_Type;
   --  Returns object suffix for language in project tree

   function Dependency_Suffix
     (Self : Object; Language : Name_Type := "ada") return Filename_Type;
   --  Returns dependency suffix for language in project tree

   function Subdirs (Tree : Object) return Filename_Optional
     with Pre => Tree.Is_Defined;
   --  Returns the subdirs parameter <sub> of the project tree such that, for
   --  each project, the actual {executable,object,library} directories are
   --  {<exec>,<obj>,<lib>}/<sub>.

   function Src_Subdirs (Tree : Object) return Filename_Optional;
   --  Returns the src_subdirs parameter <sub> of the project tree such that,
   --  for each project, the actual source directories list will be prepended
   --  with {object_dir}/<sub>.

   function Build_Path (Tree : Object) return Path_Name.Object
     with Pre => Tree.Is_Defined;
   --  Path to build tree

   function Reference (Tree : Object) return access Object;
   --  Returns access to itself

   function Get_File
     (Self             : Object;
      Base_Name        : Simple_Name;
      View             : Project.View.Object := Project.View.Undefined;
      Use_Source_Path  : Boolean := True;
      Use_Object_Path  : Boolean := True;
      Predefined_Only  : Boolean := False;
      Return_Ambiguous : Boolean := True) return Path_Name.Object
     with Pre => Self.Is_Defined;

   --  Return absolute path of source/object/project file found in Self or in
   --  View when defined.
   --
   --  If a source file matches Base_Name and Use_Source_Path is true, it
   --  is always returned.
   --  Set Predefined_Only to True to disable looking in the project sources
   --  and only look in the predefined source files.
   --
   --  Otherwise, the file will be searched for in the source dirs and/or
   --  object dirs of either a specific Project or in the whole project tree.
   --  As a special case, if Base_Name ends with '.gpr', it is also looked
   --  for among the already loaded project, even if their directory is outside
   --  the source dirs and object dirs.
   --
   --  If no such file is found, Undefined is returned.
   --
   --  The matching from base source names to full path names is potentially
   --  ambiguous when using aggregate projects, because it is valid to have
   --  multiple files with the same base name within a given project tree.
   --  In such an ambiguous case, this function will return Undefined.
   --  To lift this ambiguity, and if you know which project the file is found
   --  in, you must pass a View argument. The file must be a direct source
   --  of that project.
   --
   --  If a given full path is part of the sources for several projects, this
   --  is considered as ambiguous, because the associated object file,
   --  for instance, is different. In this case the returned value is
   --  set to the common source file if Return_Ambiguous is set to True
   --  otherwise Undefined is returned.

   Target_Name : constant Name_Type;
   --  Native host target

private

   package Name_View is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, View.Object);
   --  Map to find in which view a unit/source is defined

   package Filename_View is
     new Ada.Containers.Indefinite_Ordered_Maps (Filename_Type, View.Object);
   --  Map to find in which view a unit/source is defined

   package View_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Value_Not_Empty, View.Set.Object, "=" => View.Set."=");

   function Key
     (View : Project.View.Object; Source : Simple_Name) return String
   is
     (To_Lower (View.Namespace_Root.Name)
      & '|' & Path_Name.To_OS_Case (String (Source)));

   function Key (Item : Source.Object) return String is
     (Key (Item.View, Item.Path_Name.Simple_Name));

   function To_Hash (Item : Source.Object) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash (Key (Item)));

   function Same_Key (Left, Right : Source.Object) return Boolean is
      (Key (Left) = Key (Right));

   package Source_Set is new Ada.Containers.Hashed_Sets
     (Element_Type        => Source.Object,
      Hash                => To_Hash,
      Equivalent_Elements => Same_Key,
      "="                 => Same_Key);

   package Source_Keys is new Source_Set.Generic_Keys
     (String, Key, Ada.Strings.Hash, "=");

   type Two_Contexts is array (Context_Kind) of GPR2.Context.Object;
   --  Root and Aggregate contexts

   type Object is tagged limited record
      Self             : access Object := null;
      Root             : View.Object;
      Conf             : Project.Configuration.Object;
      Base             : GPR2.KB.Object;
      Runtime          : View.Object;
      Units            : Name_View.Map;
      Sources          : Filename_View.Map;
      Rooted_Sources   : Source_Set.Set;
      Messages         : aliased Log.Object;
      Search_Paths     : Path_Name.Set.Object := Default_Search_Paths (True);
      Implicit_With    : Containers.Filename_Set;
      Project_Dir      : Path_Name.Object;
      Build_Path       : Path_Name.Object;
      Subdirs          : Unbounded_String;
      Src_Subdirs      : Unbounded_String;
      Check_Shared_Lib : Boolean := True;
      Absent_Dir_Error : Boolean := False;
      Views            : aliased View_Maps.Map;
      Views_Set        : View.Set.Object; -- All projects in registration order
      Context          : Two_Contexts;    -- Root and aggregate contexts
   end record;

   function "=" (Left, Right : Object) return Boolean
   is (Left.Self = Right.Self);

   package Project_View_Store is
     new Ada.Containers.Vectors (Positive, View.Object);

   type Cursor is record
      Views   : Project_View_Store.Vector;
      Current : Positive;
      Root    : View.Object;
   end record;

   No_Element : constant Cursor :=
                  (Project_View_Store.Empty_Vector,
                   1, View.Undefined);

   type Constant_Reference_Type
     (View : not null access constant Project.View.Object) is null record;

   Undefined : constant Object := (others => <>);

   Target_Name : constant Name_Type :=
                   Name_Type (System.OS_Constants.Target_Name);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Windows_Target (Tree : Object) return Boolean is
     (Tree.Has_Configuration
        and then
      Tree.Configuration.Corresponding_View.Has_Attributes
        (GPR2.Project.Registry.Attribute.Shared_Library_Suffix)
        and then
      Tree.Configuration.Corresponding_View.Attribute
        (GPR2.Project.Registry.Attribute.Shared_Library_Suffix).Value_Equal
          (".dll"));
      --  ??? We may also check that the Tree target name constains mingw or
      --  windows.

   function Archive_Suffix (Self : Object) return Filename_Type is
     (if Self.Has_Configuration
      then Self.Configuration.Archive_Suffix
      else ".a");

   function Object_Suffix
     (Self : Object; Language : Name_Type := "ada") return Filename_Type
   is (if Self.Has_Configuration
       then Self.Configuration.Object_File_Suffix (Language)
       else ".o");

   function Dependency_Suffix
     (Self : Object; Language : Name_Type := "ada") return Filename_Type
   is
     (if Self.Has_Configuration
      then Self.Configuration.Dependency_File_Suffix (Language)
      elsif Language = "ada" then ".ali" else ".d");

   function Reference (Tree : Object) return access Object is
     (Tree'Unrestricted_Access);

   function Subdirs (Tree : Object) return Filename_Optional is
     (Filename_Optional (Ada.Strings.Unbounded.To_String (Tree.Subdirs)));

   function Src_Subdirs (Tree : Object) return Filename_Optional is
     (Filename_Optional (Ada.Strings.Unbounded.To_String (Tree.Src_Subdirs)));

   function Build_Path (Tree : Object) return Path_Name.Object is
     (Tree.Build_Path);

end GPR2.Project.Tree;
