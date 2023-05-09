--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with GPR2.Path_Name;
with GPR2.View_Ids;
limited with GPR2.Project.Tree;

package GPR2.Build.Compilation_Unit is

   type Unit_Location is record
      View   : View_Ids.View_Id;
      Source : Path_Name.Object;
      Index  : Unit_Index := No_Index;
   end record;
   --  Identifies the location of a Unit (spec/body or separate).

   No_Unit : constant Unit_Location := (others => <>);

   package Unit_Location_Vectors is new Ada.Containers.Vectors
     (Positive, Unit_Location);

   subtype Unit_Location_Vector is Unit_Location_Vectors.Vector;

   package Separate_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Unit_Location);

   type Object is tagged private;

   Undefined : constant Object;

   function Create (Name : Name_Type) return Object;
   --  Create a new compilation unit object with name Name

   function Is_Defined (Self : Object) return Boolean;
   --  Whether Self is defined

   function Is_Empty (Self : Object) return Boolean;
   --  False if compilation unit has any unit

   function Name (Self : Object) return Name_Type
     with Pre => Self.Is_Defined;
   --  Return the name of the compilation unit

   function Has_Part
     (Self : Object;
      Kind : Unit_Kind) return Boolean
     with Pre => Self.Is_Defined;
   --  Whether a unit with Kind is defined for Self

   procedure Add
     (Self     : in out Object;
      Kind     : Unit_Kind;
      View     : GPR2.View_Ids.View_Id;
      Path     : GPR2.Path_Name.Object;
      Index    : Unit_Index := No_Index;
      Sep_Name : Optional_Name_Type := "";
      Success  : out Boolean)
     with Pre => Self.Is_Defined
                   and then (Sep_Name'Length = 0) = (Kind /= S_Separate);

   function Get
     (Self     : Object;
      Kind     : Unit_Kind;
      Sep_Name : Optional_Name_Type) return Unit_Location
     with Pre => Self.Is_Defined
                   and then (Sep_Name'Length = 0) = (Kind /= S_Separate);
   --  Retrieve the unit part identified by Kind.
   --  If kind is "separate", the separate name must be provided.

   procedure Remove
     (Self     : in out Object;
      Kind     : Unit_Kind;
      View     : GPR2.View_Ids.View_Id;
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
   --  we can't name the function "Body"

   function Separates (Self : Object) return Separate_Maps.Map
     with Pre => Self.Is_Defined and then Self.Has_Part (S_Separate);
   --  Returns the list of separates for this compilation unit, indexed by
   --  their identifiers relative to the compilation unit.

   function Main_Part (Self : Object) return Unit_Location
     with Pre => Self.Is_Defined
                  and then (Self.Has_Part (S_Spec)
                            or else Self.Has_Part (S_Body));
   --  Returns the body of the compilation unit if it exists, or the spec

   procedure For_All_Part
     (Self : Object;
      Action : access procedure
        (Kind     : Unit_Kind;
         View     : View_Ids.View_Id;
         Path     : Path_Name.Object;
         Index    : Unit_Index;
         Sep_Name : Optional_Name_Type))
     with Pre => Self.Is_Defined;
   --  Execute Action for all parts of the given compilation unit

   function Object_File (Self : Object;
                         Tree : GPR2.Project.Tree.Object) return Simple_Name;
   --  Returns the .o's simple name for Self.

private

   type Clashing_Unit (Sep_Name_Len : Natural) is record
      Loc : Unit_Location;
      Kind : Unit_Kind;
      Sep_Name : Optional_Name_Type (1 .. Sep_Name_Len);
   end record;

   package Duplicates_List is new Ada.Containers.Indefinite_Vectors
     (Positive, Clashing_Unit);

   type Object is tagged record
      Name       : Unbounded_String;
      Spec       : Unit_Location;
      Implem     : Unit_Location;
      Separates  : Separate_Maps.Map;
      Duplicates : Duplicates_List.Vector;
   end record;

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
      (Self /= Undefined);

   function Is_Empty (Self : Object) return Boolean is
     (Self.Spec = No_Unit
      and then Self.Implem = No_Unit
      and then Self.Separates.Is_Empty);

   function Name (Self : Object) return Name_Type is
     (Name_Type (-Self.Name));

   function Has_Part
     (Self : Object;
      Kind : Unit_Kind) return Boolean
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

end GPR2.Build.Compilation_Unit;
