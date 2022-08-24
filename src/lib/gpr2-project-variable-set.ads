--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Project.Variable.Set is

   --  The variable name must not be case-sensitive

   package Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Object, "<");

   subtype Object is Set.Map;

   Empty_Set : constant Object := Set.Empty_Map;

end GPR2.Project.Variable.Set;
