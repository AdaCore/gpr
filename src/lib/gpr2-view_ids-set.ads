--
--  Copyright (C) 2019-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Hashed_Sets;

package GPR2.View_Ids.Set is new Ada.Containers.Hashed_Sets
  (GPR2.View_Ids.View_Id,
   Hash => GPR2.View_Ids.Hash,
   Equivalent_Elements => "=");
