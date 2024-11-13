--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.Tree;

package GPR2.Build.Actions_Population is

   function Populate_Actions
     (Tree : GPR2.Project.Tree.Object) return Boolean;

end GPR2.Build.Actions_Population;
