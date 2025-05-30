--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with GPR2.Containers;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.View;
with GPR2.Source_Reference;

package GPR2.Build.Compilation_Unit is

   type Unit_Location is record
      View   : Project.View.Object;
      Source : Path_Name.Object;
      Index  : Unit_Index := No_Index;
   end record;
   --  Identifies the location of a Unit (spec/body or separate)

   No_Unit : constant Unit_Location := (others => <>);

   package Unit_Location_Vectors is new Ada.Containers.Vectors
     (Positive, Unit_Location);

   type Unit_Location_Vector is new Unit_Location_Vectors.Vector
   with null record;
   subtype Unit_Location_Cursor is Unit_Location_Vectors.Cursor;
   Empty_Vector : constant Unit_Location_Vector :=
                    Unit_Location_Vector'(Unit_Location_Vectors.Empty_Vector
                                                           with null record);

   package Separate_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Unit_Location);

   type Object is tagged private;
   type Object_List is array (Natural range <>) of Object;

   Undefined : constant Object;

   function Create
     (Name    : Name_Type;
      Context : GPR2.Project.View.Object) return Object
     with Pre => Context.Is_Namespace_Root;
   --  Create a new compilation unit object with name Name

   procedure Check_Name_Validity
     (Self     : Object;
      Messages : in out GPR2.Log.Object)
     with Pre => Self.Is_Defined and then not Self.Is_Empty;
   --  Check that the unit name is valid

   function Check_Name_Validity
     (Name : Name_Type) return Boolean;
   --  Same as above but using a generic name and just returning whether the
   --  name is valid or not

   function Check_Name_Validity
     (Name     : Name_Type;
      Sloc     : Source_Reference.Object'Class;
      As_Error : Boolean := False;
      Messages : in out GPR2.Log.Object) return Boolean;
   --  Same as above but allow specifying a source location and whether
   --  the messages should be errors. Returns the status of the validity check.

   function Is_Defined (Self : Object) return Boolean;
   --  Whether Self is defined

   function Is_Empty (Self : Object) return Boolean;
   --  False if compilation unit has any unit

   function Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Return the name of the compilation unit

   function Root_View (Self : Object) return GPR2.Project.View.Object
     with Pre  => Self.Is_Defined,
          Post => Root_View'Result.Is_Namespace_Root;
   --  Return the root view of the subtree Self belongs to

   function Owning_View (Self : Object) return GPR2.Project.View.Object
     with Pre => Self.Is_Defined;
   --  Return the view that defines the main part of this compilation unit.
   --  Result may be undefined if Self is empty or only contains separates.

   function Has_Part
     (Self : Object;
      Kind : Valid_Unit_Kind) return Boolean
     with Pre => Self.Is_Defined;
   --  Whether a unit with Kind is defined for Self

   procedure Add
     (Self     : in out Object;
      Kind     : Valid_Unit_Kind;
      View     : GPR2.Project.View.Object;
      Path     : GPR2.Path_Name.Object;
      Index    : Unit_Index := No_Index;
      Sep_Name : Optional_Name_Type := "";
      Success  : out Boolean)
     with Pre => Self.Is_Defined
                   and then (Sep_Name'Length = 0) = (Kind /= S_Separate);

   function Get
     (Self     : Object;
      Kind     : Valid_Unit_Kind;
      Sep_Name : Optional_Name_Type := "") return Unit_Location
     with Pre => Self.Is_Defined
                   and then (Sep_Name'Length = 0) = (Kind /= S_Separate);
   --  Retrieve the unit part identified by Kind.
   --  If kind is "separate", the separate name must be provided.

   procedure Remove
     (Self     : in out Object;
      Kind     : Valid_Unit_Kind;
      View     : GPR2.Project.View.Object;
      Path     : GPR2.Path_Name.Object;
      Index    : Unit_Index := No_Index;
      Sep_Name : Optional_Name_Type := "")
     with Pre => Self.Is_Defined
                   and then (Sep_Name'Length = 0) = (Kind /= S_Separate);
   --  Remove unit part identified by Kind from the compilation unit.
   --  If kind is separate, the separate name must be provided.

   function Spec (Self : Object) return Unit_Location
     with Pre => Self.Is_Defined and then Self.Has_Part (S_Spec);
   --  Returns the spec for the compilation unit

   function Main_Body (Self : Object) return Unit_Location
     with Pre => Self.Is_Defined and then Self.Has_Part (S_Body);
   --  Returns the body for the compilation unit. Note: body being a keyword
   --  we can't name the function "Body".

   function Separates (Self : Object) return Separate_Maps.Map
     with Pre => Self.Is_Defined;
   --  Returns the list of separates for this compilation unit, indexed by
   --  their identifiers relative to the compilation unit.

   function Has_Main_Part (Self : Object) return Boolean
     with Pre => Self.Is_Defined;

   function Main_Part (Self : Object) return Unit_Location
     with Pre => Self.Is_Defined
                  and then (Self.Has_Part (S_Spec)
                            or else Self.Has_Part (S_Body));
   --  Returns the body of the compilation unit if it exists, or the spec

   function Main_Part (Self : Object) return Unit_Kind
     with Pre => Self.Is_Defined and then Self.Has_Main_Part;

   function Is_Body_Needed_For_SAL (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Returns whether the body of this unit is needed for SAL. This is the
   --  case for spec being generic, having some generic routines or inlining.

   procedure For_All_Part
     (Self : Object;
      Action : access procedure
        (Kind     : Unit_Kind;
         View     : GPR2.Project.View.Object;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type))
     with Pre => Self.Is_Defined;
   --  Execute Action for all parts of the given compilation unit

   function Known_Dependencies
     (Self      : Object;
      Spec_Only : Boolean := False) return Containers.Name_Set;
   --  Return the list of unit names withed by Self.
   --  If Spec_Only is set, only the units withed by the spec of Self are
   --  returned.

   function Object_File (Self : Object) return Simple_Name;
   --  Returns the .o's simple name for Self

   function Dependency_File (Self : Object) return Simple_Name;
   --  Returns the .ali's simple name for Self

private

   type Object is tagged record
      Name       : Unbounded_String;
      Owner      : GPR2.Project.View.Object;
      Root_View  : GPR2.Project.View.Object;
      Spec       : Unit_Location;
      Implem     : Unit_Location;
      Separates  : Separate_Maps.Map;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
      (Self /= Undefined);

   function Is_Empty (Self : Object) return Boolean is
     (Self.Spec = No_Unit
      and then Self.Implem = No_Unit
      and then Self.Separates.Is_Empty);

   function Name (Self : Object) return Name_Type is
     (-Self.Name);

   function Root_View (Self : Object) return GPR2.Project.View.Object is
     (Self.Root_View);

   function Owning_View (Self : Object) return GPR2.Project.View.Object is
     (Self.Owner);

   function Has_Part
     (Self : Object;
      Kind : Valid_Unit_Kind) return Boolean
   is (case Kind is
          when S_Spec => Self.Spec /= No_Unit,
          when S_Body => Self.Implem /= No_Unit,
          when S_Separate => not Self.Separates.Is_Empty);

   function Spec (Self : Object) return Unit_Location is
     (Self.Spec);

   function Main_Body (Self : Object) return Unit_Location is
     (Self.Implem);

   function Separates (Self : Object) return Separate_Maps.Map is
     (Self.Separates);

   function Main_Part (Self : Object) return Unit_Location is
     (if Self.Implem /= No_Unit then Self.Implem else Self.Spec);

   function Main_Part (Self : Object) return Unit_Kind is
     (if Self.Implem /= No_Unit then S_Body else S_Spec);

   function Has_Main_Part (Self : Object) return Boolean is
     (Self.Implem /= No_Unit or else Self.Spec /= No_Unit);

end GPR2.Build.Compilation_Unit;
