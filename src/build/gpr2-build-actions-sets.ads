--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Ordered_Sets;

package GPR2.Build.Actions.Sets is
  new Ada.Containers.Indefinite_Ordered_Sets (Actions.Object'Class);
