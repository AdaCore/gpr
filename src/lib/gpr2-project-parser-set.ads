--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Ordered_Maps;

with GPR2.Path_Name;

package GPR2.Project.Parser.Set is

   use type GPR2.Path_Name.Object;

   package Set is new Ada.Containers.Ordered_Maps
     (GPR2.Path_Name.Object, Object);

   subtype Object is Set.Map;

end GPR2.Project.Parser.Set;
