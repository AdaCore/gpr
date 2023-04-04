--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Ordered_Sets;
with GPR2.Project.View;

package GPR2.Build.Compilation_Input.Sets is

   package Internal is new Ada.Containers.Ordered_Sets
     (Compilation_Input.Object, "<" => Compilation_Input."<");

   type Object is new Internal.Set with null record;
   Empty_Set : constant Object;

   subtype Cursor is Internal.Cursor;

   function Create (View : Project.View.Object) return Object
     with Pre => View.Is_Defined;

private

   Empty_Set : constant Object := Object'(Internal.Empty_Set with null record);

end GPR2.Build.Compilation_Input.Sets;
