--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Ordered_Sets;

package GPR2.Project.View.Set is

   package Set is new Ada.Containers.Ordered_Sets (Object);

   type Object is new Set.Set with private;

   Empty_Set : constant Object;

private

   type Object is new Set.Set with null record;

   Empty_Set : constant Object := (Set.Empty_Set with null record);

end GPR2.Project.View.Set;
