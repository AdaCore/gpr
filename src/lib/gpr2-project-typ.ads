--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GPR2.Project.Name_Values;

private with GPR2.Project.Registry.Attribute;

package GPR2.Project.Typ is

   type Object is new Name_Values.Object with private;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   function Image
     (Self : Object; Name_Len : Natural := 0) return String
     with Pre => Self.Is_Defined;
   --  Returns a string representation

private

   use all type GPR2.Project.Registry.Attribute.Value_Kind;
   use type GPR2.Project.Name_Values.Object;

   type Object is new Name_Values.Object with null record
     with Dynamic_Predicate =>
       Name_Values.Object (Object) = Name_Values.Undefined
       or else Object.Kind = List;

   Undefined : constant Object := (Name_Values.Undefined with null record);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Project.Typ;
