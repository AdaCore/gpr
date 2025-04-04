--
--  Copyright (C) 2019-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Ordered_Maps;

with GPR2.Path_Name;

package GPR2.Project_Parser.Set is

   use type GPR2.Path_Name.Object;

   package Set is new Ada.Containers.Ordered_Maps
     (GPR2.Path_Name.Object, Object);

   subtype Object is Set.Map;

end GPR2.Project_Parser.Set;
