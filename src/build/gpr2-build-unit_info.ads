--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Containers;

package GPR2.Build.Unit_Info is

   type Object (<>) is tagged private;

   function Create
     (Unit_Name     : Optional_Name_Type;
      Index         : Unit_Index;
      Kind          : Unit_Kind;
      Separate_Name : Optional_Name_Type := No_Name;
      Dependencies  : GPR2.Containers.Name_Set :=
                        GPR2.Containers.Name_Type_Set.Empty_Set;
      Parsed        : Boolean := False) return Object;

   function Is_Defined (Self : Object) return Boolean;

   function Kind (Self : Object) return Unit_Kind
     with Pre => Self.Is_Defined;
   --  Kind of unit

   function Index (Self : Object) return Unit_Index
     with Pre => Self.Is_Defined;
   --  In case of multi-unit source, the index of the unit, else No_Index

   function Is_Parsed (Self : Object) return Boolean
     with Pre => Self.Is_Defined;
   --  Whether we used the Ada parser to analyze the unit

   procedure Set_Parsed_State (Self : in out Object; State : Boolean)
     with Pre  => Self.Is_Defined,
          Post => Self.Is_Parsed = State;

   function Name (Self : Object) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  The compilation unit name. May be empty in case of a body with
   --  pragma No_Body.

   function Full_Name (U : Object) return Name_Type;
   --  If the part denotes a separate, return Name.Separate_Name, else just
   --  reutrn Name.

   function Separate_Name (Self : Object) return Optional_Name_Type
     with Pre => Self.Is_Defined;
   --  In case Kind is S_Separate, the name of the subunit (without the
   --  compilation unit name part).

   function Dependencies (Self : Object) return GPR2.Containers.Name_Set
     with Pre => Self.Is_Defined;
   --  List of compilation unit names that are explicitly withed by this
   --  unit.

private

   type Object
     (Name_Len     : Natural;
      Separate_Len : Natural)
   is tagged record
      Kind          : Unit_Kind := S_Spec;
      --  Kind of unit
      Index         : Unit_Index := No_Index;
      --  In case of multi-unit source, the index of the unit, else No_Index
      Is_Parsed     : Boolean := False;
      --  Whether we used the Ada parser to analyze the unit
      --  ??? Add the with clauses here
      Name          : Optional_Name_Type (1 .. Name_Len);
      --  The compilation unit name. May be empty in case of a body with
      --  pragma No_Body.
      Separate_Name : Optional_Name_Type (1 .. Separate_Len);
      --  In case Kind is S_Separate, the name of the subunit (without the
      --  compilation unit name part).
      Dependencies   : GPR2.Containers.Name_Set;
      --  List of unit identifiers withed by this unit part
   end record;
   --  Structure used to describe the unit(s) contained in the source.
   --  The corresponding Compilation Unit can be retrieved from the main
   --  tree_db object.

   function Create
     (Unit_Name      : Optional_Name_Type;
      Index          : Unit_Index;
      Kind           : Unit_Kind;
      Separate_Name  : Optional_Name_Type := No_Name;
      Dependencies   : GPR2.Containers.Name_Set :=
                         GPR2.Containers.Name_Type_Set.Empty_Set;
      Parsed         : Boolean := False) return Object
   is (Name_Len       => Unit_Name'Length,
       Separate_Len   => Separate_Name'Length,
       Kind           => Kind,
       Index          => Index,
       Is_Parsed      => Parsed,
       Name           => Optional_Name_Type (Ada.Characters.Handling.To_Upper
                                               (String (Unit_Name))),
       Separate_Name  => Optional_Name_Type
                           (Ada.Characters.Handling.To_Upper
                              (String (Separate_Name))),
       Dependencies   => Dependencies);

   Undefined : constant Object := (0, 0, others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Kind (Self : Object) return Unit_Kind is
     (Self.Kind);

   function Index (Self : Object) return Unit_Index is
     (Self.Index);

   function Is_Parsed (Self : Object) return Boolean is
     (Self.Is_Parsed);

   function Name (Self : Object) return Optional_Name_Type is
     (Self.Name);

   function Full_Name (U : Object) return Name_Type is
     (if U.Separate_Len = 0
      then U.Name
      else GPR2."&" (GPR2."&" (U.Name, "."), U.Separate_Name));

   function Separate_Name (Self : Object) return Optional_Name_Type is
     (Self.Separate_Name);

   function Dependencies (Self : Object) return GPR2.Containers.Name_Set is
     (Self.Dependencies);

end GPR2.Build.Unit_Info;
