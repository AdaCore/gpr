--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Indefinite_Ordered_Maps;

package GPR2.Build.Compilation_Unit.Maps is
  new Ada.Containers.Indefinite_Ordered_Maps
    (Name_Type, Compilation_Unit.Object);
