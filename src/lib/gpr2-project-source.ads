--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Calendar;

with GPR2.Unit;
with GPR2.Project.View;
with GPR2.Project.View.Set;
with GPR2.Source;
with GPR2.Source_Info;

limited with GPR2.Project.Source.Artifact;
limited with GPR2.Project.Source.Part_Set;
limited with GPR2.Project.Source.Set;

package GPR2.Project.Source is

   use type GPR2.Source.Object;
   use type GPR2.Unit.Library_Unit_Type;

   type Naming_Exception_Kind is (No, Yes, Multi_Unit);

   subtype Naming_Exception_Value is
     Naming_Exception_Kind range Yes .. Multi_Unit;

   type Object is new GPR2.Source.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   type Constant_Access is access constant Object;

   type Source_Part is record
      Source : Object;
      Index  : Unit_Index;
   end record;

   function "<" (L, R : Source_Part) return Boolean with Inline;

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Source           : GPR2.Source.Object;
      View             : Project.View.Object;
      Naming_Exception : Naming_Exception_Kind;
      Is_Compilable    : Boolean;
      Aggregated       : Project.View.Object := Project.View.Undefined)
      return Object
     with Pre => Source.Is_Defined
                 and then View.Is_Defined
                 and then Aggregated.Is_Defined =
                            (View.Kind = K_Aggregate_Library);
   --  Constructor for Object. View is where the source is defined (found from
   --  View Source_Dirs) and Extending_View is the optional view from which the
   --  project source is extended. That is, if Extending_View is defined then
   --  this source is coming from an extended project for View.

   function View (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined,
          Post => View'Result.Is_Defined;
   --  The view the source is in

   function Aggregated (Self : Object) return Project.View.Object
     with Pre  => Self.Is_Defined;
   --  The view where the source is aggregated from

   function Is_Aggregated (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the source is taken into aggregating library source set
   --  from the aggregated project.

   function Is_Compilable (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if the source is compilable, meaning that a compiler is
   --  defined for this language.

   function Is_Interface (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self is part of the project view interface

   function Has_Naming_Exception (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether the source comes from a naming exception

   function Naming_Exception (Self : Object) return Naming_Exception_Kind
     with Pre  => Self.Is_Defined,
          Post => Self.Has_Naming_Exception
                  or else Naming_Exception'Result = No;
   --  Returns whether the source comes from a naming exception

   function Has_Aggregating_View (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if Self has an aggregating view defined, that is source
   --  is part of an aggregate library.

   function Aggregating_Views (Self : Object) return Project.View.Set.Object
     with Pre  => Self.Is_Defined and then Self.Has_Aggregating_View,
          Post => (for all Agg of Aggregating_Views'Result =>
                     Agg.Kind = K_Aggregate_Library);
   --  Returns the aggregating view

   function Is_Main (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether the source is the main file to create executable

   function Is_Compilable (Self : Object;
                           Index : Unit_Index) return Boolean;
   --  Tells if the unit identified by index, or the source (if no units)
   --  is compilable (e.g. is a body unit, or a spec_only unit)

   function Artifacts
     (Self : Object; Force_Spec : Boolean := False) return Artifact.Object
     with Pre => Self.Is_Defined;
   --  Returns the source artifacts for this source. Note that the returned
   --  Source artifacts may not exist if the compilation has not yet been
   --  done/finished.
   --  If Flag Force_Spec is True than the artifact object created like the
   --  spec does not have a body. This mode is needed for gprinstall -m option.

   --
   --  The following routines only make sense if Has_Units is True
   --

   function Dependencies
     (Self    : Object;
      Index   : Unit_Index := No_Index;
      Closure : Boolean    := False;
      Sorted  : Boolean    := True) return Part_Set.Object
     with Pre => Self.Is_Defined;
   --  Returns the source files on which the current source file depends.
   --
   --  In case of unit-based sources, if index is No_Index, then dependencies
   --  of all the units in the source are returned.
   --  Sorted: if set, the returned object is ordered to have consistent
   --  result order across runs, else the returned set is a hashed set, that is
   --  faster to compute.

   procedure Dependencies
     (Self     : Object;
      Index    : Unit_Index;
      For_Each : not null access procedure
                   (Source    : Object;
                    Index     : Unit_Index;
                    Timestamp : Ada.Calendar.Time);
      Closure  : Boolean := False;
      Sorted   : Boolean := True);
   --  Call For_Each routine for each dependency unit with it's source
   --
   --  The following routines may be used for both unit-based and
   --  non-unit-based sources. In the latter case, Index is not used.
   --  Sorted: if set, the returned object is ordered to have consistent
   --  result order across runs, else the returned set is a hashed set, that is
   --  faster to compute.

   procedure Dependencies
     (Self     : Object;
      Index    : Unit_Index;
      For_Each : not null access procedure
                   (Source    : Object;
                    Unit      : GPR2.Unit.Object;
                    Timestamp : Ada.Calendar.Time);
      Closure  : Boolean := False;
      Sorted   : Boolean := True);
   --  Call For_Each routine for each dependency unit with it's source
   --
   --  The following routines may be used for both unit-based and
   --  non-unit-based sources. In the latter case, Index is not used.

   function Has_Other_Part
     (Self  : Object;
      Index : Unit_Index := No_Index) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns True if an other part exists for this project source's unit at
   --  the given index.

   function Other_Part
     (Self  : Object;
      Index : Unit_Index := No_Index) return Source_Part
     with Pre  => Self.Is_Defined and then Self.Has_Other_Part (Index),
          Post => Other_Part'Result.Source.Is_Defined;
   --  Returns the project's source containing the other part for this project
   --  source's unit at the given index.

   function Other_Part_Unchecked
     (Self : Object;
      Index : Unit_Index) return Source_Part;
   --  Same as Other_Part, but returns Undefined if no other part exists for
   --  Self.

   function Separate_From
     (Self  : Object;
      Index : Unit_Index) return Source_Part
     with Pre => Self.Is_Defined
                 and then Self.Kind (Index) = GPR2.Unit.S_Separate;
   --  Returns the project's source containing the separate for Self's unit at
   --  the given index.

   procedure Update
     (Self     : in out Object;
      Backends : Source_Info.Backend_Set := Source_Info.All_Backends)
     with Pre => Self.Is_Defined;
   --  Ensure that the project source is parsed/updated if needed

   procedure Update
     (Self     : in out Object;
      C        : Project.Source.Set.Cursor;
      Backends : Source_Info.Backend_Set := Source_Info.All_Backends)
     with Pre => Self.Is_Defined;
   --  Ensure that the project source is parsed/updated if needed

private

   type Object is new GPR2.Source.Object with record
      View             : Project.Weak_Reference;
      --  Use weak reference to View to avoid reference cycle between Source
      --  and its View. Otherwise we've got memory leak after release view and
      --  valgrind detected mess in memory deallocations at the process exit.

      Aggregated       : Project.Weak_Reference;
      --  View where the source is aggregated from

      Naming_Exception : Naming_Exception_Kind := No;
      Is_Compilable    : Boolean               := False;
      Inherited        : Boolean               := False;
      --  From extended project
   end record;

   Undefined : constant Object := (GPR2.Source.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Is_Aggregated (Self : Object) return Boolean is
     (not Definition_References."="
        (Self.Aggregated, Definition_References.Null_Weak_Ref));

   function Is_Compilable (Self : Object;
                           Index : Unit_Index) return Boolean
   is (Kind (Self, Index) in GPR2.Unit.Body_Kind
       or else (Self.Language = Ada_Language
         and then Kind (Self, Index) in GPR2.Unit.Spec_Kind
         and then not Self.Has_Other_Part (Index)));
   --  The condition above is about Ada package specs
   --  without a body, which have to be compilable.

   function Has_Naming_Exception (Self : Object) return Boolean is
     (Self.Naming_Exception in Naming_Exception_Value);

   function Naming_Exception (Self : Object) return Naming_Exception_Kind is
     (Self.Naming_Exception);

   function "<" (L, R : Source_Part) return Boolean is
     (if L.Source = R.Source
      then L.Index < R.Index
      else L.Source < R.Source);

end GPR2.Project.Source;
