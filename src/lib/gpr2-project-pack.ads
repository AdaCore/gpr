--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Handle project's packages which are a set of attributes and variables

with Ada.Containers.Ordered_Maps;

with GPR2.Project.Attribute.Set;
with GPR2.Project.Variable.Set;
with GPR2.Source_Reference.Pack;

private package GPR2.Project.Pack is

   type Object is new Source_Reference.Pack.Object with record
      Attrs : Project.Attribute.Set.Object;
      Vars  : Project.Variable.Set.Object;
   end record;

   Undefined : constant Object;
   --  This constant is equal to any object declared without an explicit
   --  initializer.

   overriding function Is_Defined (Self : Object) return Boolean;
   --  Returns true if Self is defined

   package Set is
     new Ada.Containers.Ordered_Maps (Package_Id, Object, "<");

private

   Undefined : constant Object :=
                 (Source_Reference.Pack.Undefined with others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

end GPR2.Project.Pack;
