--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded;

package GPR2.Project.Tree.C is

   function Id
     (Tree : GPR2.Project.Tree.Object)
      return Ada.Strings.Unbounded.Unbounded_String;

end GPR2.Project.Tree.C;
