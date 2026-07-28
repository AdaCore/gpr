--
--  Copyright (C) 2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Indefinite_Vectors;

package GPR2.Build.Artifacts.Library.Vectors is new
  Ada.Containers.Indefinite_Vectors
    (Index_Type   => Positive,
     Element_Type => Object);
