--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This package shares the implementation part of attributes and variables.
--  That is it abstracts out a name associated with a single value or a list
--  of values (possibly none).

with GPR2.Containers;
with GPR2.Project.Registry.Attribute;
with GPR2.Source_Reference.Identifier;
with GPR2.Source_Reference.Value;

package GPR2.Project.Name_Values is

   use all type Registry.Attribute.Value_Kind;

   type Object is new Source_Reference.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   subtype Value_Kind is Registry.Attribute.Value_Kind;

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Create
     (Name  : Source_Reference.Identifier.Object;
      Value : Source_Reference.Value.Object) return Object
     with Post => Create'Result.Kind = Single
                  and then Create'Result.Name.Text = Name.Text
                  and then Create'Result.Count_Values = 1;
   --  Create a single-valued object

   function Create
     (Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List) return Object
     with Post => Create'Result.Kind = List
                  and then Create'Result.Name.Text = Name.Text
                  and then Create'Result.Count_Values = Values.Length;
   --  Create a multi-valued object

   function Kind (Self : Object'Class) return Registry.Attribute.Value_Kind
     with Pre => Object (Self).Is_Defined;
   --  Returns the Kind for the Name/Values pair object

   function Name (Self : Object) return Source_Reference.Identifier.Object
     with Pre => Self.Is_Defined;
   --  Returns the name of the Name/Value pair object

   function Count_Values (Self : Object) return Containers.Count_Type
     with Pre  => Self.Is_Defined,
          Post => (if Self.Kind = Single then Count_Values'Result = 1);
   --  Returns the number of values for the Name/Values pair object

   function Values (Self : Object) return Containers.Source_Value_List
     with Pre  => Self.Is_Defined,
          Post => Values'Result.Length = Self.Count_Values;
   --  Returns the values for the Name/Values pair object

   function Value (Self : Object) return Source_Reference.Value.Object
     with Pre => Self.Is_Defined and then Self.Kind = Single;
   --  Returns the value for the Name/Values pair object

private

   type Object is new Source_Reference.Object with record
      Kind   : Registry.Attribute.Value_Kind := List;
      Name   : Source_Reference.Identifier.Object;
      Values : Containers.Source_Value_List;
      V_Map  : Containers.Value_Source_Reference;  -- fast check
   end record;

   Undefined : constant Object :=
                 (Source_Reference.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Project.Name_Values;
