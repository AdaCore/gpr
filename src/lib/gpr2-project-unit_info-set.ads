--
--  Copyright (C) 2019-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Hashed_Maps;

package GPR2.Project.Unit_Info.Set is

   package Set is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Name_Type, Unit_Info.Object, GPR2.Hash, GPR2."=");

   subtype Object is Set.Map;

   subtype Cursor is Set.Cursor;

end GPR2.Project.Unit_Info.Set;
