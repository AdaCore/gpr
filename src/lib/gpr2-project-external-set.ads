--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Project.External.Set is

   --  The type names must not be case-sensitive

   package Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Object, "<");

   subtype Object is Set.Map;

end GPR2.Project.External.Set;
