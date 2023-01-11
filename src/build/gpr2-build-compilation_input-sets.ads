--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Ordered_Sets;
with GPR2.Build.View_Db;

package GPR2.Build.Compilation_Input.Sets is

   package Internal is new Ada.Containers.Ordered_Sets
     (Compilation_Input.Object, "<" => Compilation_Input."<");

   type Object is new Internal.Set with null record;

   function Create (View : View_Db.Object) return Object;

end GPR2.Build.Compilation_Input.Sets;
