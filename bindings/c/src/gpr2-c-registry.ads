--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Values;
with GPR2.Project.Tree;
with GPR2.Project.View;

package GPR2.C.Registry is

   --  Project Tree

   package Tree is

      function Register
        (Tree : GPR2.Project.Tree.Object) return GPR2.C.JSON.Values.JSON_Value;
      --  Registers project tree

      function Lookup
        (Id : GPR2.C.JSON.Values.JSON_Value) return GPR2.Project.Tree.Object;
      --  Lookup for project tree with given identifier. Returns `Undefined`
      --  when project tree is not found.

      procedure Unregister (Id : GPR2.C.JSON.Values.JSON_Value);
      --  Unregisters project tree

   end Tree;

   --  Project View

   package View is

      function Register
        (View : GPR2.Project.View.Object) return GPR2.C.JSON.Values.JSON_Value;
      --  Registers project view

      function Lookup
        (Id : GPR2.C.JSON.Values.JSON_Value) return GPR2.Project.View.Object;
      --  Lookup for project view with given identifier. Returns `Undefined`
      --  when project view is not found.

      procedure Unregister (Id : GPR2.C.JSON.Values.JSON_Value);
      --  Unregisters project view

   end View;

end GPR2.C.Registry;
