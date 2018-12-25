------------------------------------------------------------------------------
--                                                                          --
--                           GPR2 PROJECT MANAGER                           --
--                                                                          --
--         Copyright (C) 2016-2018, Free Software Foundation, Inc.          --
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
with Ada.Iterator_Interfaces;
with Ada.Strings.Unbounded;

with GPR2.Containers;
with GPR2.Context;
with GPR2.Log;
with GPR2.Message;
with GPR2.Project.Configuration;
pragma Elaborate (GPR2.Project.Configuration);
--  Elaborate to avoid a circular dependency due to default Elaborate_Body
with GPR2.Project.View.Set;

limited with GPR2.Unit;

private with Ada.Containers.Vectors;

package GPR2.Project.Tree is

   use type Ada.Containers.Count_Type;
   use type GPR2.Context.Object;
   use type GPR2.Project.View.Object;

   type Object is tagged limited private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => View.Object;

   subtype Project_Tree is Object;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function "=" (Left, Right : Object) return Boolean;
   --  Returns True if Left and Right are the same tree

   procedure Load
     (Self     : in out Object;
      Filename : Path_Name.Object;
      Context  : GPR2.Context.Object;
      Config   : Configuration.Object := Configuration.Undefined;
      Subdirs  : Optional_Name_Type   := No_Name)
     with Pre => Filename.Is_Defined;
   --  Loads a root project

   procedure Load_Configuration
     (Self     : in out Object;
      Filename : Path_Name.Object)
     with Pre => Filename.Is_Defined;
   --  Loads a configuration project for this tree

   procedure Load_Autoconf
     (Self              : in out Object;
      Filename          : Path_Name.Object;
      Context           : GPR2.Context.Object;
      Subdirs           : Optional_Name_Type := No_Name;
      Target            : Optional_Name_Type := No_Name;
      Language_Runtimes : GPR2.Containers.Name_Value_Map :=
                            GPR2.Containers.Name_Value_Map_Package.Empty_Map)
       with Pre => Filename.Is_Defined;
   --  Loads a tree in autoconf mode.
   --  If Target is specified, then we use it directly instead of fetching
   --  the root project attribute.
   --  Same with the Language_Runtime map: for each language Lang in the
   --  project tree, if the map has an association (Lang,RTS) then we use it
   --  instead of any attribute Runtime (Lang) declared in the root project.
   --  Typically this is useful to enforce precedence of the command-line
   --  options --target and --RTS[:lang].

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

   function Has_Runtime_Project (Self : Object) return Boolean;
   --  Returns True if a configuration project is loaded on this tree

   function Runtime_Project (Self : Object) return View.Object
     with Pre => Self.Is_Defined and then Self.Has_Runtime_Project;
   --  Returns the configuration project for the given tree

   function Target (Self : Object) return Name_Type;
   --  Returns the target for the project tree

   function Runtime
     (Self : Object; Language : Name_Type) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  Returns the runtime selected for the given language or the empty string
   --  if no specific runtime has been configured for this project tree.

   function View_For
     (Self         : Object;
      Name         : Name_Type;
      Context_View : View.Object) return View.Object
     with Pre => Self.Is_Defined;
   --  Returns the project's view in the tree which corresponds to project name
   --  and that is matching the context. The context is needed as in the tree
   --  the same project can have different views with different context (e.g.
   --  under an aggregate project which is redefining some external variables).
   --  Given the context we are not sure of the uniqueness of the view, but
   --  this doesn't matter as all views of the same project with the same
   --  context will have the exact same definition. Returns Undefined if the
   --  view was not found.

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
          Post => Self.Log_Messages.Count = Self.Log_Messages.Count'Old + 1;
   --  Adds new message into the Log of Self

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

   procedure Set_Context
     (Self    : in out Object;
      Context : GPR2.Context.Object;
      Changed : access procedure (Project : View.Object) := null)
     with Pre  => Self.Is_Defined,
          Post => Self.Context = Context
                  or else Self.Root_Project.Qualifier in Aggregate_Kind;
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
      Source : Path_Name.Full_Name;
      Unit   : Name_Type)
     with Pre => Self.Is_Defined;
   --  Records the view in which unit is defined

   procedure Clear_View
     (Self : in out Object;
      Unit : GPR2.Unit.Object)
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
   --  Gets the view in which unit is defined, returns Undefined if the unit
   --  has not been found.

   procedure Invalidate_Sources
     (Self : Object;
      View : Project.View.Object := Project.View.Undefined)
     with Pre => Self.Is_Defined;
   --  Invalidates the sources for all views in the tree if View is set or the
   --  source in the given view otherwise. This is needed when some sources
   --  are added or removed from the view. It is not required to call
   --  Update_Sources below, when the routine Sources on one of the views
   --  of the tree will be called, the set of sources will be recomputed.

   procedure Update_Sources (Self : Object)
     with Pre => Self.Is_Defined;
   --  Ensures that all views' sources are up-to-date. This is needed before
   --  computing the dependencies of a source in the project tree. This routine
   --  is called where needed and is there for internal use only.

   procedure Register_Project_Search_Path
     (Self : in out Object;
      Dir  : Path_Name.Object)
     with Pre => Dir.Is_Defined;
   --  Adds a project search path for this tree

   function Project_Search_Paths (Self : Object) return Path_Name.Set.Object;
   --  Returns the Tree project search paths

   function Archive_Suffix (Self : Object) return Name_Type;
   --  Returns archive suffix for the project tree

   function Object_Suffix
     (Self : Object; Language : Name_Type := "ada") return Name_Type;
   --  Returns object suffix for language in project tree

   function Dependency_Suffix
     (Self : Object; Language : Name_Type := "ada") return Name_Type;
   --  Returns dependency suffix for language in project tree

   function Subdirs (Tree : Object) return Optional_Name_Type
     with Pre => Tree.Is_Defined;
   --  Returns the subdirs parameter <sub> of the project tree such that, for
   --  each project, the actual {executable,object,library} directories are
   --  {<exec>,<obj>,<lib>}/<sub>.

private

   package Name_View is
     new Ada.Containers.Indefinite_Ordered_Maps (Name_Type, View.Object);
   --  Map to find in which view a unit/source is defined

   package View_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, View.Set.Object, "=" => View.Set."=");

   type Object is tagged limited record
      Self         : access Object := null;
      Root         : View.Object;
      Conf         : Project.Configuration.Object;
      Runtime      : View.Object;
      Units        : Name_View.Map;
      Sources      : Name_View.Map;
      Messages     : aliased Log.Object;
      Search_Paths : Path_Name.Set.Object;
      Subdirs      : Ada.Strings.Unbounded.Unbounded_String;
      Views        : aliased View_Maps.Map;
      Views_Set    : View.Set.Object; -- All projects in registration order
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

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Archive_Suffix (Self : Object) return Name_Type is
     (if Self.Has_Configuration
      then Self.Configuration.Archive_Suffix
      else ".a");

   function Object_Suffix
     (Self : Object; Language : Name_Type := "ada") return Name_Type
   is (if Self.Has_Configuration
       then Self.Configuration.Object_File_Suffix (Language)
       else ".o");

   function Dependency_Suffix
     (Self : Object; Language : Name_Type := "ada") return Name_Type
   is
     (if Self.Has_Configuration
      then Self.Configuration.Dependency_File_Suffix (Language)
      elsif Language = "ada" then ".ali" else ".d");

   function Subdirs (Tree : Object) return Optional_Name_Type is
     (Optional_Name_Type (Ada.Strings.Unbounded.To_String (Tree.Subdirs)));

end GPR2.Project.Tree;
