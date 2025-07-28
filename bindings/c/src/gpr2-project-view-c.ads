--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Strings.Unbounded;

package GPR2.Project.View.C is

   function Id
     (View : GPR2.Project.View.Object)
      return Ada.Strings.Unbounded.Unbounded_String;

end GPR2.Project.View.C;
